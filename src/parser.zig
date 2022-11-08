const std = @import("std");
const lex = @import("lexer.zig");
const testing = std.testing;

/// Lexer is a wrapper enum so we can do static dispatch on real/fake lexers for testing purposes
const Lexer = union(enum) {
    real: lex.Lexer,
    fake: lex.Fake,

    fn next(self: *Lexer) lex.Lexer.Error!?lex.TokLoc {
        return switch (self.*) {
            .real => |*r| r.next(),
            .fake => |*f| f.next(),
        };
    }
};

/// Table represents a TOML table (i.e. dicitonary/hashmap). It assumes that the key and value were allocated with the
/// same allocator
pub const Table = struct {
    table: TableBase = .{},

    const TableBase = std.StringHashMapUnmanaged(Value);

    pub fn hasKey(self: *Table, key: []const u8) bool {
        return self.table.get(key) != null;
    }

    pub fn insert(self: *Table, allocator: std.mem.Allocator, key: []const u8, value: Value) !void {
        return try self.table.put(allocator, key, value);
    }

    pub fn getOrPutTable(self: *Table, allocator: std.mem.Allocator, key: []const u8, value: Table) !*Table {
        var v = try self.table.getOrPutValue(allocator, key, .{ .table = value });
        if (v.value_ptr.* != .table) return error.not_table;

        return &v.value_ptr.table;
    }

    pub fn getOrPutArray(self: *Table, allocator: std.mem.Allocator, key: []const u8) !*Value.ArrayBase {
        var v = try self.table.getOrPutValue(allocator, key, .{ .array = .{} });
        if (v.value_ptr.* != .array) return error.not_array;

        return &v.value_ptr.array;
    }

    pub fn deinit(self: *Table, allocator: std.mem.Allocator) void {
        var it = self.table.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(allocator);
        }

        self.table.deinit(allocator);
    }
};

/// Value represents a TOML value: i.e. an integer, string, float, boolean, table, array
pub const Value = union(enum) {
    integer: i64,
    string: []const u8,
    array: ArrayBase,
    table: Table,
    boolean: bool,

    const ArrayBase = std.ArrayListUnmanaged(Value);

    fn dupe(self: *const Value, allocator: std.mem.Allocator) !Value {
        return switch (self.*) {
            .string => |s| Value{ .string = try allocator.dupe(u8, s) },
            .array => |al| b: {
                var new = try std.ArrayListUnmanaged(Value).initCapacity(allocator, al.items.len);
                new.appendSliceAssumeCapacity(al.items);
                break :b Value{ .array = new };
            },
            .table => |t| b: {
                var new_table = Table{};
                var it = t.table.iterator();
                while (it.next()) |entry| {
                    try new_table.insert(
                        allocator,
                        try allocator.dupe(u8, entry.key_ptr.*),
                        try entry.value_ptr.dupe(allocator),
                    );
                }
                break :b Value{ .table = new_table };
            },
            else => return self.*,
        };
    }

    fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            Value.table => |*t| t.deinit(allocator),
            Value.array => |*arr| {
                for (arr.items) |*item| {
                    item.deinit(allocator);
                }
                arr.deinit(allocator);
            },
            Value.string => |*s| allocator.free(s.*),
            else => {},
        }
    }
};

/// Parser takes a Lexer and parses the tokens into a table.
const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    peeked: ?lex.TokLoc = null,
    diag: ?lex.Diagnostic = null,
    top_level_table: *Table,
    current_table: *Table,

    fn init(allocator: std.mem.Allocator, lexer: Lexer) !Parser {
        var table = try allocator.create(Table);
        table.* = .{};
        return .{ .allocator = allocator, .lexer = lexer, .top_level_table = table, .current_table = table };
    }

    fn peek(self: *Parser) !lex.TokLoc {
        if (self.peeked) |tokloc| return tokloc;

        self.peeked = try self.lexer.next();
        return self.peeked orelse error.eof;
    }

    fn pop(self: *Parser) !lex.TokLoc {
        if (self.peeked) |tokloc| {
            self.peeked = null;
            return tokloc;
        }

        const next = try self.lexer.next();
        return next orelse error.eof;
    }

    /// expect asserts that the current token has the same tag as expected.
    ///
    /// NOTE: If the token stores a value (e.g. boolean, string) then it does not check that the value is the same
    fn expect(self: *Parser, comptime expected: lex.Tok, comptime str: []const u8) !void {
        const actual = try self.pop();
        if (std.meta.activeTag(actual.tok) != std.meta.activeTag(expected)) {
            self.diag = .{
                .msg = "expected '" ++ str ++ "'",
                .loc = actual.loc,
            };
            return error.unexpected_token;
        }
    }

    /// parseKey parses a potentially nested (i.e. with dots in) key and inserts each key segment into al. It returns
    /// the location of the final key
    fn parseKey(self: *Parser, key: []const u8, loc: lex.Loc, al: *std.ArrayList([]const u8)) !lex.Loc {
        var dup: []const u8 = try self.allocator.dupe(u8, key);
        {
            errdefer self.allocator.free(dup);
            try al.append(dup);
        }

        var prev_loc = loc;

        while (true) {
            const next = try self.peek();
            switch (next.tok) {
                .equals, .close_square_bracket => return prev_loc,
                .dot => {},
                else => {
                    self.diag = .{
                        .msg = "expected '.', ']' or '=' after key",
                        .loc = next.loc,
                    };
                    return error.unexpected_token;
                },
            }

            _ = self.pop() catch unreachable;

            const key_tok = try self.pop();
            prev_loc = key_tok.loc;
            const key_s = switch (key_tok.tok) {
                .key => |k| k,
                .string => |s| s,
                else => {
                    self.diag = .{
                        .msg = "expected key after '.'",
                        .loc = key_tok.loc,
                    };
                    return error.unexpected_token;
                },
            };

            var new_dup = try self.allocator.dupe(u8, key_s);
            {
                errdefer self.allocator.free(new_dup);
                try al.append(new_dup);
            }
        }
    }

    fn parseValue(self: *Parser) !Value {
        const tokloc = try self.pop();
        var val = switch (tokloc.tok) {
            .string => |s| Value{ .string = try self.allocator.dupe(u8, s) },
            .integer => |i| Value{ .integer = i },
            .boolean => |b| Value{ .boolean = b },
            .open_square_bracket => try self.parseInlineArray(),
            else => {
                self.diag = .{
                    .msg = "expected value type",
                    .loc = tokloc.loc,
                };
                return error.unexpected_token;
            },
        };

        return val;
    }

    fn skipNewlines(self: *Parser) !void {
        while ((try self.peek()).tok == .newline) {
            _ = self.pop() catch unreachable;
        }
    }

    /// parseInlineArray parses a value of the form "[ <value-1>, <value-2>, ...]"
    fn parseInlineArray(self: *Parser) error{ OutOfMemory, unexpected_token, unexpected_char, eof }!Value {
        var al = std.ArrayListUnmanaged(Value){};
        errdefer {
            for (al.items) |*item| {
                item.deinit(self.allocator);
            }
            al.deinit(self.allocator);
        }

        while (true) {
            try self.skipNewlines();

            var v = try self.parseValue();
            {
                errdefer v.deinit(self.allocator);
                try al.append(self.allocator, v);
            }

            try self.skipNewlines();

            const tokloc = try self.pop();
            switch (tokloc.tok) {
                .comma => {},
                .newline => {},
                .close_square_bracket => return Value{ .array = al },
                else => {
                    self.diag = .{
                        .msg = "expected one of '\n', ',' or ']' after list entry",
                        .loc = tokloc.loc,
                    };
                    return error.unexpected_char;
                },
            }
        }
    }

    /// createPath takes a key_path and ensures that it exists. If any key at any point in the path does not exist then
    /// it will be created. All but the final key are created as tables - e.g. "foo.bar.baz" will ensure "foo" is a
    /// table in self.current_table, "bar" is a table in that. "baz" will be a value with an undefined type which the
    /// caller should fill in
    fn createPath(
        self: *Parser,
        key_path: []const []const u8,
        loc: lex.Loc,
        allow_exists: bool,
    ) !struct { table: *Table, value: *Value, existed: bool = false } {
        std.debug.assert(key_path.len > 0);

        var tbl = self.current_table;
        for (key_path) |k, i| {
            if (i == key_path.len - 1) {
                if (tbl.hasKey(k)) {
                    if (allow_exists)
                        return .{ .table = tbl, .value = tbl.table.getPtr(k) orelse unreachable, .existed = true };

                    self.diag = .{
                        .msg = "key already exists",
                        .loc = loc,
                    };
                    return error.key_already_exists;
                }

                var val = tbl.table.getPtr(k) orelse b: {
                    var dup = try self.allocator.dupe(u8, k);
                    errdefer self.allocator.free(dup);

                    try tbl.table.put(self.allocator, dup, undefined);
                    break :b tbl.table.getPtr(k) orelse unreachable;
                };
                return .{ .table = tbl, .value = val };
            }

            const existed = tbl.table.get(k) != null;
            var dup = try self.allocator.dupe(u8, k);
            errdefer self.allocator.free(dup);
            tbl = try tbl.getOrPutTable(self.allocator, dup, .{});
            if (existed) self.allocator.free(dup);
        }

        unreachable;
    }

    /// parseAssignment parses a key/value assignment to `key`, followed by either a newline or EOF
    fn parseAssignment(self: *Parser, loc: lex.Loc, key: []const u8) !void {
        var val = b: {
            var al = std.ArrayList([]const u8).init(self.allocator);
            defer {
                for (al.items) |s| self.allocator.free(s);
                al.deinit();
            }

            const new_loc = try self.parseKey(key, loc, &al);
            var res = try self.createPath(al.items, new_loc, false);
            break :b res.value;
        };

        try self.expect(.equals, "=");

        val.* = try self.parseValue();

        self.expect(.newline, "\n") catch |err| switch (err) {
            error.eof => return,
            else => return err,
        };
    }

    /// parseTableHeader parses "[<key>]\n" which specifies the next assignments should be in the table defined by <key>
    ///
    /// NOTE: Assumes "[" has already been parsed
    fn parseTableHeader(self: *Parser) !void {
        std.debug.assert(self.current_table == self.top_level_table);

        const tokloc = try self.pop();
        const key = switch (tokloc.tok) {
            .key => |k| k,
            .string => |s| s,
            else => {
                self.diag = .{ .msg = "expected key inside of square brackets", .loc = tokloc.loc };
                return error.unexpected_token;
            },
        };

        var al = std.ArrayList([]const u8).init(self.allocator);
        defer {
            for (al.items) |s| self.allocator.free(s);
            al.deinit();
        }

        const new_loc = try self.parseKey(key, tokloc.loc, &al);
        var res = try self.createPath(al.items, new_loc, false);

        res.value.* = .{ .table = .{} };
        self.current_table = &res.value.table;

        try self.expect(.close_square_bracket, "]");
        try self.expect(.newline, "\n");
    }

    /// parseArrayHeaderKey parses the key inside an array header. It will ensure that all but the last key in the path
    /// exists as a table. It then returns the final table it created (or just self.current_table if the key inside the
    /// header is not a path) and the final key.
    ///
    /// NOTE: The caller is responsible for freeing the key in the result
    pub fn parseArrayHeaderKey(self: *Parser, key: []const u8, loc: lex.Loc) !struct { table: *Table, key: []const u8 } {
        var al = std.ArrayList([]const u8).init(self.allocator);
        defer {
            for (al.items) |s| self.allocator.free(s);
            al.deinit();
        }

        const new_loc = try self.parseKey(key, loc, &al);

        std.debug.assert(al.items.len > 0);
        if (al.items.len == 1) return .{ .table = self.current_table, .key = try self.allocator.dupe(u8, al.items[0]) };

        const all_but_last_key = al.items[0 .. al.items.len - 1];
        var res = try self.createPath(all_but_last_key, new_loc, true);
        if (res.existed and res.value.* != .table) {
            self.diag = .{
                .msg = "key already exists and is not a table",
                .loc = new_loc,
            };
            return error.not_table;
        }

        if (!res.existed) res.value.* = .{ .table = .{} };

        return .{ .table = &res.value.table, .key = try self.allocator.dupe(u8, al.items[al.items.len - 1]) };
    }

    /// parseArrayHeader parses "[[<key>]]\n" which specifies the next assignments should be in a table in the array
    /// <key>
    ///
    /// NOTE: Assumes "[[" has already been parsed
    fn parseArrayHeader(self: *Parser) !void {
        std.debug.assert(self.current_table == self.top_level_table);

        const tokloc = try self.pop();
        const key = switch (tokloc.tok) {
            .key => |k| k,
            .string => |s| s,
            else => {
                self.diag = .{ .msg = "expected key inside of square brackets", .loc = tokloc.loc };
                return error.unexpected_token;
            },
        };

        var res = try self.parseArrayHeaderKey(key, tokloc.loc);
        const existed = res.table.hasKey(res.key);
        var arr = b: {
            errdefer self.allocator.free(res.key);

            break :b try res.table.getOrPutArray(self.allocator, res.key);
        };

        defer if (existed) self.allocator.free(res.key);

        try arr.append(self.allocator, .{ .table = .{} });
        self.current_table = &arr.items[arr.items.len - 1].table;

        try self.expect(.close_square_bracket, "]");
        try self.expect(.close_square_bracket, "]");
        try self.expect(.newline, "\n");
    }

    fn parse(self: *Parser) !Table {
        while (true) {
            const tokloc = self.pop() catch |err| switch (err) {
                error.eof => {
                    const table = self.top_level_table.*;
                    self.allocator.destroy(self.top_level_table);

                    self.top_level_table = try self.allocator.create(Table);
                    self.top_level_table.* = .{};
                    self.current_table = self.top_level_table;
                    return table;
                },
                else => return err,
            };

            switch (tokloc.tok) {
                .key => |k| try self.parseAssignment(tokloc.loc, k),
                .string => |s| try self.parseAssignment(tokloc.loc, s),
                .open_square_bracket => {
                    self.current_table = self.top_level_table;
                    const next = try self.peek();
                    switch (next.tok) {
                        .open_square_bracket => {
                            _ = self.pop() catch unreachable;
                            try self.parseArrayHeader();
                        },
                        else => try self.parseTableHeader(),
                    }
                },
                .newline => {},
                else => {
                    self.diag = .{
                        .msg = "expected key",
                        .loc = tokloc.loc,
                    };
                    return error.unexpected_token;
                },
            }
        }
    }

    fn deinit(self: *Parser) void {
        self.top_level_table.deinit(self.allocator);
        self.allocator.destroy(self.top_level_table);
    }
};

/// parse takes a given TOML source and returns a Table which has been allocated with the given allocator.
pub fn parse(allocator: std.mem.Allocator, src: []const u8) !Table {
    var lexer = Lexer{ .real = lex.Lexer.init(allocator, src) };
    var parser = try Parser.init(allocator, lexer);
    defer parser.deinit();

    return try parser.parse();
}

const KV = struct { k: []const u8, v: Value };

/// kvsToTable takes a slice of KVs (i.e. assignments) and returns the table they would make
fn kvsToTable(kvs: []const KV) !Table {
    var table = Table{};
    for (kvs) |entry| {
        const v = try entry.v.dupe(testing.allocator);
        try table.insert(testing.allocator, try testing.allocator.dupe(u8, entry.k), v);
    }
    return table;
}

/// toksToLocToks takes a slice of toks and gives them a dummy location
fn toksToLocToks(toks: []const lex.Tok) ![]lex.TokLoc {
    const loc = lex.Loc{ .line = 1, .col = 1 };

    var al = std.ArrayListUnmanaged(lex.TokLoc){};
    for (toks) |tok| {
        try al.append(testing.allocator, .{ .tok = tok, .loc = loc });
    }

    return al.items;
}

/// testParse takes the given toks and parses them into a table
fn testParse(toks: []const lex.Tok) !Table {
    var toklocs = try toksToLocToks(toks);
    defer testing.allocator.free(toklocs);

    var lexer = Lexer{ .fake = .{ .toklocs = toklocs } };

    var parser = try Parser.init(testing.allocator, lexer);
    defer parser.deinit();

    return try parser.parse();
}

fn expectEqualTables(expected: Table, actual: Table) !void {
    try testing.expectEqual(expected.table.count(), actual.table.count());

    var it = expected.table.iterator();
    while (it.next()) |entry| {
        var value = actual.table.get(entry.key_ptr.*);
        try testing.expect(value != null);
        try testing.expectEqual(std.meta.activeTag(entry.value_ptr.*), std.meta.activeTag(value.?));

        switch (entry.value_ptr.*) {
            .string => |s| try testing.expectEqualStrings(s, value.?.string),
            else => try testing.expectEqual(entry.value_ptr.*, value.?),
        }
    }
}

/// expectTableEqualTo asserts that an actual table is equal to the KV assignments expected
fn expectTableEqualTo(expected: []const KV, actual: Table) !void {
    var expected_table = try kvsToTable(expected);
    defer expected_table.deinit(testing.allocator);

    try expectEqualTables(expected_table, actual);
}

/// expectEqualParses parses toks and asserts that it gives the same value as expected, once expected is turned into a
/// table
fn expectEqualParses(toks: []const lex.Tok, expected: []const KV) !void {
    var parsed_table = try testParse(toks);
    defer parsed_table.deinit(testing.allocator);

    try expectTableEqualTo(expected, parsed_table);
}

/// expectErrorParse asserts that trying to parse toks gives err
fn expectErrorParse(err: anyerror, toks: []const lex.Tok) !void {
    var toklocs = try toksToLocToks(toks);
    defer testing.allocator.free(toklocs);

    var lexer = Lexer{ .fake = .{ .toklocs = toklocs } };
    var parser = try Parser.init(testing.allocator, lexer);
    defer parser.deinit();

    try testing.expectError(err, parser.parse());
}

test "default table assignment" {
    try expectEqualParses(
        &.{ .{ .key = "foo" }, .equals, .{ .string = "a" } },
        &.{.{ .k = "foo", .v = .{ .string = "a" } }},
    );

    try expectEqualParses(
        &.{ .{ .key = "foo" }, .equals, .{ .integer = 147 } },
        &.{.{ .k = "foo", .v = .{ .integer = 147 } }},
    );

    try expectEqualParses(
        &.{ .{ .key = "foo" }, .equals, .{ .boolean = true } },
        &.{.{ .k = "foo", .v = .{ .boolean = true } }},
    );

    try expectEqualParses(
        &.{ .{ .string = "foo" }, .equals, .{ .string = "a" }, .newline, .{ .key = "bar" }, .equals, .{ .string = "b" } },
        &.{
            .{ .k = "foo", .v = .{ .string = "a" } },
            .{ .k = "bar", .v = .{ .string = "b" } },
        },
    );
}

test "fail: default table assignment" {
    try expectErrorParse(error.unexpected_token, &.{.equals});
    try expectErrorParse(error.unexpected_token, &.{ .{ .key = "foo" }, .newline });
    try expectErrorParse(error.unexpected_token, &.{ .{ .string = "foo" }, .equals, .{ .string = "a" }, .{ .key = "bar" }, .equals, .{ .string = "b" } });
    try expectErrorParse(error.unexpected_token, &.{ .{ .key = "foo" }, .equals, .{ .key = "a" } });
    try expectErrorParse(error.unexpected_token, &.{ .{ .integer = 147 }, .equals, .{ .string = "a" } });
    try expectErrorParse(error.unexpected_token, &.{ .{ .boolean = true }, .equals, .{ .string = "a" } });
}

test "dotted assignment" {
    // zig fmt: off
    {
        var table = try testParse(&.{ .{ .key = "foo"}, .dot, .{ .key = "bar" }, .equals, .{ .string = "a" }});
        defer table.deinit(testing.allocator);
        
        try testing.expectEqualStrings("a", table.table.get("foo").?.table.table.get("bar").?.string);
    }

    {
        var table = try testParse(&.{ 
            .{ .key = "foo"}, .dot, .{ .key = "bar" }, .equals, .{ .string = "a" }, .newline,
            .{ .key = "foo"}, .dot, .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.table.get("foo").?.table.table.get("bar").?.string);
        try testing.expectEqualStrings("b", table.table.get("foo").?.table.table.get("baz").?.string);
    }

    {
        var table = try testParse(&.{ 
            .{ .key = "foo"}, .dot, .{ .key = "bar" }, .equals, .{ .string = "a" }, .newline,
            .{ .key = "foo"}, .dot, .{ .string = "baz baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.table.get("foo").?.table.table.get("bar").?.string);
        try testing.expectEqualStrings("b", table.table.get("foo").?.table.table.get("baz baz").?.string);
    }
    // zig fmt: on
}

test "fail: dotted assignment" {
    try expectErrorParse(error.key_already_exists, &.{
        .{ .key = "foo" }, .dot, .{ .key = "bar" }, .equals, .{ .string = "a" }, .newline,
        .{ .key = "foo" }, .dot, .{ .key = "bar" }, .equals, .{ .string = "b" }, .newline,
    });
}

test "table header" {
    // zig fmt: off
    {
        var table = try testParse(&.{ 
            .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .newline,
            .{ .key = "bar"}, .equals, .{ .string = "a" }, .newline,
            .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.table.get("foo").?.table.table.get("bar").?.string);
        try testing.expectEqualStrings("b", table.table.get("foo").?.table.table.get("baz").?.string);
    }

    {
        var table = try testParse(&.{ 
            .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .newline,
            .{ .key = "bar"}, .equals, .{ .string = "a" }, .newline,
            .newline, 
            .newline, 
            .newline,
            .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.table.get("foo").?.table.table.get("bar").?.string);
        try testing.expectEqualStrings("b", table.table.get("foo").?.table.table.get("baz").?.string);
    }

    {
        var table = try testParse(&.{ 
            .open_square_bracket, .{ .key="foo" }, .dot, .{.key = "bar"}, .close_square_bracket, .newline,
            .{ .key = "baz" }, .equals, .{ .string = "a" }, .newline,
            .{ .key = "bat" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings(
            "a", 
            table.table.get("foo").?.table.table.get("bar").?.table.table.get("baz").?.string);
        try testing.expectEqualStrings(
            "b",
            table.table.get("foo").?.table.table.get("bar").?.table.table.get("bat").?.string);
    }

    {
        var table = try testParse(&.{ 
            .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .newline,
            .{ .key = "a"}, .equals, .{ .integer = 1 }, .newline,
            .open_square_bracket, .{ .key="bar" }, .close_square_bracket, .newline,
            .{ .key = "b" }, .equals, .{ .integer = 2 }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqual(@as(i64, 1), table.table.get("foo").?.table.table.get("a").?.integer);
        try testing.expectEqual(@as(i64, 2), table.table.get("bar").?.table.table.get("b").?.integer);
    }
    // zig fmt: on
}

test "fail: table header" {
    try expectErrorParse(error.eof, &.{ .open_square_bracket, .{ .key = "foo" } });
    try expectErrorParse(error.unexpected_token, &.{ .open_square_bracket, .{ .key = "foo" }, .equals });
}

test "inline array" {
    // zig fmt: off
    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .comma, .{ .integer = 2 },
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqualSlices(Value, 
            &.{ .{ .integer = 1 }, .{ .integer = 2 }},
            table.table.get("foo").?.array.items);
    }

    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .newline, .newline, .comma, .newline, .{ .integer = 2 },
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqualSlices(Value, 
            &.{ .{ .integer = 1 }, .{ .integer = 2 }},
            table.table.get("foo").?.array.items);
    }
    
    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .comma, .{ .string = "bar" },
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqual(@as(usize, 2), table.table.get("foo").?.array.items.len);
        try testing.expectEqual(Value{ .integer = 1}, table.table.get("foo").?.array.items[0]);
        try testing.expectEqualStrings("bar", table.table.get("foo").?.array.items[1].string);

    }

    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .comma, 
                .open_square_bracket, .{ .integer = 2 }, .comma, .{ .integer = 3 }, .close_square_bracket,
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqual(@as(usize, 2), table.table.get("foo").?.array.items.len);
        try testing.expectEqual(Value{ .integer = 1}, table.table.get("foo").?.array.items[0]);
        try testing.expectEqualSlices(Value, 
            &.{ .{ .integer = 2 }, .{ .integer = 3 }},
            table.table.get("foo").?.array.items[1].array.items);

    }
    // zig fmt: on
}

test "fail: inline array" {
    try expectErrorParse(error.eof, &.{ .{ .key = "foo" }, .equals, .open_square_bracket });
    try expectErrorParse(error.eof, &.{ .{ .key = "foo" }, .equals, .open_square_bracket, .{ .integer = 1 } });
    try expectErrorParse(error.eof, &.{ .{ .key = "foo" }, .equals, .open_square_bracket, .{ .integer = 1 }, .comma });
    try expectErrorParse(
        error.unexpected_token,
        &.{ .{ .key = "foo" }, .equals, .open_square_bracket, .{ .integer = 1 }, .comma, .close_square_bracket },
    );
}

test "arrays" {
    // zig fmt: off
    {
        var table = try testParse(&.{ 
            .open_square_bracket, .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "bar"}, .equals, .{ .string = "a" }, .newline,

            .open_square_bracket, .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqual(@as(usize, 1), table.table.count());
        const arr =  table.table.get("foo").?.array;
        try testing.expectEqual(@as(usize, 2), arr.items.len);
        try testing.expectEqualStrings("a", arr.items[0].table.table.get("bar").?.string);
        try testing.expectEqualStrings("b", arr.items[1].table.table.get("baz").?.string);
    }

    {
        var table = try testParse(&.{ 
            .open_square_bracket, .open_square_bracket, 
                .{ .key="foo" }, .dot, .{ .key = "bar" }, 
            .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "a"}, .equals, .{ .integer = 1 }, .newline,

            .open_square_bracket, .open_square_bracket, 
                .{ .key="foo" }, .dot, .{ .key = "bar" }, 
            .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "b" }, .equals, .{ .integer = 2 }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqual(@as(usize, 1), table.table.count());
        const arr =  table.table.get("foo").?.table.table.get("bar").?.array;
        try testing.expectEqual(@as(usize, 2), arr.items.len);
        try testing.expectEqual(@as(i64, 1), arr.items[0].table.table.get("a").?.integer);
        try testing.expectEqual(@as(i64, 2), arr.items[1].table.table.get("b").?.integer);
    }
    // zig fmt: on
}

test "fail: arrays" {
    try expectErrorParse(error.eof, &.{ .open_square_bracket, .open_square_bracket });
    try expectErrorParse(error.eof, &.{ .open_square_bracket, .open_square_bracket, .{ .key = "foo" } });
    try expectErrorParse(
        error.eof,
        &.{ .open_square_bracket, .open_square_bracket, .{ .key = "foo" }, .close_square_bracket },
    );

    // zig fmt: off
    try expectErrorParse(error.key_already_exists, &.{
            .open_square_bracket, .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "bar"}, .equals, .{ .string = "a" }, .newline,

            .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .newline,
            .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,
    });

    try expectErrorParse(error.not_array, &.{
            .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .newline,
            .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,

            .open_square_bracket, .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "bar"}, .equals, .{ .string = "a" }, .newline,
    });
    // zig fmt: on
}
