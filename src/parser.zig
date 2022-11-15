const std = @import("std");
const lex = @import("lexer.zig");
const testing = std.testing;

/// Lexer is a wrapper enum so we can do static dispatch on real/fake lexers for testing purposes
pub const Lexer = union(enum) {
    real: lex.Lexer,
    fake: lex.Fake,

    fn next(self: *Lexer, force_key: bool) lex.Lexer.Error!?lex.TokLoc {
        return switch (self.*) {
            .real => |*r| r.next(force_key),
            .fake => |*f| f.next(),
        };
    }

    fn deinit(self: *Lexer) void {
        switch (self.*) {
            .real => |*r| r.deinit(),
            else => {},
        }
    }
};

/// Table represents a TOML table (i.e. dicitonary/hashmap). It assumes that the key and value were allocated with the
/// same allocator
pub const Table = struct {
    table: TableBase = .{},
    source: Source,

    const Source = enum { @"inline", header, top_level, assignment };

    const TableBase = std.StringHashMapUnmanaged(Value);

    pub fn contains(self: *const Table, key: []const u8) bool {
        return self.table.get(key) != null;
    }
    
    pub fn getInteger(self: *const Table, key: []const u8) ?i64 {
        const val = self.table.get(key) orelse return null;
        switch (val) {
            .integer => |i| return i,
            else => return null,
        }
    }

    pub fn getFloat(self: *const Table, key: []const u8) ?f64 {
        const val = self.table.get(key) orelse return null;
        switch (val) {
            .float => |f| return f,
            else => return null,
        }
    }

    pub fn getBool(self: *const Table, key: []const u8) ?bool {
        const val = self.table.get(key) orelse return null;
        switch (val) {
            .boolean => |b| return b,
            else => return null,
        }
    }

    pub fn getString(self: *const Table, key: []const u8) ?[]const u8 {
        const val = self.table.get(key) orelse return null;
        switch (val) {
            .string => |s| return s,
            else => return null,
        }
    }

    pub fn getArray(self: *const Table, key: []const u8) ?Array {
        const val = self.table.get(key) orelse return null;
        switch (val) {
            .array => |a| return a,
            else => return null,
        }
    }

    pub fn getTable(self: *const Table, key: []const u8) ?Table {
        const val = self.table.get(key) orelse return null;
        switch (val) {
            .table => |t| return t,
            else => return null,
        }
    }

    fn insert(self: *Table, allocator: std.mem.Allocator, key: []const u8, value: Value) !void {
        return try self.table.put(allocator, key, value);
    }

    fn getOrPutTable(self: *Table, allocator: std.mem.Allocator, key: []const u8, value: Table) !*Table {
        var v = try self.table.getOrPutValue(allocator, key, .{ .table = value });
        if (v.value_ptr.* != .table) return error.not_table;

        return &v.value_ptr.table;
    }

    fn getOrPutArray(self: *Table, allocator: std.mem.Allocator, key: []const u8, source: Array.Source) !*Array {
        var v = try self.table.getOrPutValue(allocator, key, .{ .array = .{ .source = source } });
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

pub const Array = struct {
    array: Base = .{},
    source: Source,

    const Base = std.ArrayListUnmanaged(Value);
    const Source = enum { @"inline", header };
    
    pub fn items(self: *const Array) []const Value {
        return self.array.items;
    }
};

/// Value represents a TOML value: i.e. an integer, string, float, boolean, table, array
pub const Value = union(enum) {
    integer: i64,
    float: f64,
    string: []const u8,
    array: Array,
    table: Table,
    boolean: bool,

    fn dupe(self: *const Value, allocator: std.mem.Allocator) !Value {
        return switch (self.*) {
            .string => |s| Value{ .string = try allocator.dupe(u8, s) },
            .array => |a| b: {
                var new = try std.ArrayListUnmanaged(Value).initCapacity(allocator, a.array.items.len);
                new.appendSliceAssumeCapacity(a.array.items);
                break :b Value{ .array = .{ .array = new, .source = a.source } };
            },
            .table => |t| b: {
                var new_table = Table{ .source = t.source };
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

    fn emptyOtherThanTables(self: *Value) bool {
        if (self.* != .table) return false;

        var iter = self.table.table.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.* != .table) return false;
            if (!entry.value_ptr.emptyOtherThanTables()) return false;
        }

        return true;
    }

    fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            Value.table => |*t| t.deinit(allocator),
            Value.array => |*a| {
                for (a.array.items) |*item| {
                    item.deinit(allocator);
                }
                a.array.deinit(allocator);
            },
            Value.string => |*s| allocator.free(s.*),
            else => {},
        }
    }
};

/// Parser takes a Lexer and parses the tokens into a table.
pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    peeked: ?lex.TokLoc = null,
    diag: ?lex.Diagnostic = null,
    top_level_table: *Table,
    current_table: *Table,

    pub fn init(allocator: std.mem.Allocator, lexer: Lexer) !Parser {
        var table = try allocator.create(Table);
        table.* = .{ .source = .top_level };
        return .{ .allocator = allocator, .lexer = lexer, .top_level_table = table, .current_table = table };
    }

    fn peek(self: *Parser, force_key: bool) !lex.TokLoc {
        if (self.peeked) |tokloc| return tokloc;

        self.peeked = try self.lexer.next(force_key);
        return self.peeked orelse error.eof;
    }

    fn pop(self: *Parser, force_key: bool) !lex.TokLoc {
        if (self.peeked) |tokloc| {
            self.peeked = null;
            return tokloc;
        }

        const next = try self.lexer.next(force_key);
        return next orelse error.eof;
    }

    /// expect asserts that the current token has the same tag as expected.
    ///
    /// NOTE: If the token stores a value (e.g. boolean, string) then it does not check that the value is the same
    fn expect(self: *Parser, comptime expected: lex.Tok, comptime str: []const u8) !void {
        const actual = try self.pop(false);
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
            const next = try self.peek(false);
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

            _ = self.pop(false) catch unreachable;

            const key_tok = try self.pop(true);
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
        const tokloc = try self.pop(false);
        var val = switch (tokloc.tok) {
            .string => |s| Value{ .string = try self.allocator.dupe(u8, s) },
            .integer => |i| Value{ .integer = i },
            .float => |f| Value{ .float = f },
            .boolean => |b| Value{ .boolean = b },
            .open_square_bracket => try self.parseInlineArray(),
            .open_curly_brace => try self.parseInlineTable(),
            else => {
                self.diag = .{ .msg = "expected value type", .loc = tokloc.loc };
                return error.unexpected_token;
            },
        };

        return val;
    }

    fn skipNewlines(self: *Parser, force_key: bool) !bool {
        var had_newline = false;
        while ((try self.peek(force_key)).tok == .newline) {
            had_newline = true;
            _ = self.pop(force_key) catch unreachable;
        }
        return had_newline;
    }

    /// parseInlineArray parses a value of the form "[ <value-1>, <value-2>, ...]"
    fn parseInlineArray(self: *Parser) error{
        OutOfMemory,
        unexpected_token,
        unexpected_char,
        eof,
        key_already_exists,
        not_table_or_array,
        not_table,
        string_not_ended,
        inline_tables_and_arrays_are_immutable,
        invalid_codepoint,
    }!Value {
        var al = std.ArrayListUnmanaged(Value){};
        errdefer {
            for (al.items) |*item| {
                item.deinit(self.allocator);
            }
            al.deinit(self.allocator);
        }

        var had_newline = false;
        var first = true;

        while (true) {
            had_newline = try self.skipNewlines(false) or had_newline;

            if (!first) {
                const tokloc = try self.peek(false);
                switch (tokloc.tok) {
                    .close_square_bracket => _ = {
                        _ = self.pop(false) catch unreachable;
                        return Value{ .array = .{ .array = al, .source = .@"inline" } };
                    },
                    else => {},
                }
            }

            if (first and (try self.peek(false)).tok == .close_square_bracket) {
                _ = self.pop(false) catch unreachable;
                return Value{ .array = .{ .array = al, .source = .@"inline" } };
            }

            first = false;

            var v = try self.parseValue();
            {
                errdefer v.deinit(self.allocator);
                try al.append(self.allocator, v);
            }

            had_newline = try self.skipNewlines(false) or had_newline;

            const tokloc = try self.pop(false);
            switch (tokloc.tok) {
                .comma => {},
                .newline => {},
                .close_square_bracket => return Value{ .array = .{ .array = al, .source = .@"inline" } },
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

    fn parseInlineTable(self: *Parser) error{
        OutOfMemory,
        unexpected_token,
        unexpected_char,
        eof,
        not_table_or_array,
        key_already_exists,
        not_table,
        string_not_ended,
        inline_tables_and_arrays_are_immutable,
        invalid_codepoint,
    }!Value {
        const curr = self.current_table;

        var tbl = Table{ .source = .@"inline" };
        errdefer tbl.deinit(self.allocator);

        self.current_table = &tbl;
        defer self.current_table = curr;

        var first = true;
        while (true) {
            const tokloc = try self.pop(true);
            switch (tokloc.tok) {
                .close_curly_brace => {
                    if (first) return .{ .table = tbl };

                    self.diag = .{ .msg = "trailing comma not allowed in inline table", .loc = tokloc.loc };
                    return error.unexpected_token;
                },
                .key => |k| try self.parseAssignment(tokloc.loc, k),
                .string => |s| try self.parseAssignment(tokloc.loc, s),
                else => {
                    self.diag = .{ .msg = "expected a key in inline table", .loc = tokloc.loc };
                    return error.unexpected_token;
                },
            }

            first = false;

            const next = try self.peek(true);
            switch (next.tok) {
                .close_curly_brace => {
                    _ = self.pop(true) catch unreachable;
                    return .{ .table = tbl };
                },
                .comma => _ = self.pop(true) catch unreachable,
                else => {
                    self.diag = .{ .msg = "expected a comma after assignment in inline table", .loc = next.loc };
                    return error.unexpected_token;
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
        source: Table.Source,
    ) !struct { table: *Table, value: *Value, existed: bool = false } {
        std.debug.assert(key_path.len > 0);

        var tbl = self.current_table;
        for (key_path) |k, i| {
            if (i == key_path.len - 1) {
                if (tbl.table.getPtr(k)) |val| {
                    const empty_other_than_tables = val.emptyOtherThanTables();
                    if (allow_exists or empty_other_than_tables) {
                        if (val.* != .array)
                            return .{ .table = tbl, .value = val, .existed = true };

                        if (val.array.source == .@"inline") {
                            self.diag = .{ .msg = "cannot extend inline arrays", .loc = loc };
                            return error.inline_tables_and_arrays_are_immutable;
                        }
                        var tbl_in_array = &val.array.array.items[val.array.array.items.len - 1];
                        return .{ .table = &tbl_in_array.table, .value = tbl_in_array, .existed = true };
                    }

                    self.diag = .{ .msg = "key already exists", .loc = loc };
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

            if (tbl.table.getPtr(k)) |val| {
                switch (val.*) {
                    .table => |*t| {
                        if (t.source == .@"inline") {
                            self.diag = .{ .msg = "inline tables are immutable", .loc = loc };
                            return error.inline_tables_and_arrays_are_immutable;
                        }
                        tbl = t;
                    },
                    .array => |*a| {
                        if (a.source == .@"inline") {
                            self.diag = .{ .msg = "cannot extend inline arrays", .loc = loc };
                            return error.inline_tables_and_arrays_are_immutable;
                        }

                        tbl = &a.array.items[a.array.items.len - 1].table;
                    },
                    else => {
                        self.diag = .{ .msg = "key already exists and is not a table or array", .loc = loc };
                        return error.not_table_or_array;
                    },
                }
            } else {
                var dup = try self.allocator.dupe(u8, k);
                errdefer self.allocator.free(dup);
                tbl = try tbl.getOrPutTable(self.allocator, dup, .{ .source = source });
            }
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
            var res = try self.createPath(al.items, new_loc, false, .assignment);
            break :b res.value;
        };

        try self.expect(.equals, "=");

        val.* = try self.parseValue();
        if (val.* == .array and val.*.array.array.items.len == 0) {
            self.diag = .{ .msg = "inline arrays cannot be empty", .loc = loc };
            return error.unexpected_token;
        }

        const next = self.peek(false) catch |err| switch (err) {
            error.eof => return,
            else => return err,
        };

        switch (next.tok) {
            .newline, .comma, .close_curly_brace => return,
            else => {
                self.diag = .{ .msg = "expected comma, newline or eof after assignment", .loc = next.loc };
                return error.unexpected_token;
            },
        }
    }

    /// parseAssignmentEatNewline runs parseAssignment and then expects a newline. It is used for assignments on their
    /// own line
    fn parseAssignmentEatNewline(self: *Parser, loc: lex.Loc, key: []const u8) !void {
        try self.parseAssignment(loc, key);
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

        const tokloc = try self.pop(true);
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
        var res = try self.createPath(al.items, new_loc, false, .header);

        if (!res.existed) res.value.* = .{ .table = .{ .source = .header } };
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
        var res = try self.createPath(all_but_last_key, new_loc, true, .header);
        if (res.existed and res.value.* != .table) {
            self.diag = .{
                .msg = "key already exists and is not a table",
                .loc = new_loc,
            };
            return error.not_table;
        }

        if (!res.existed) res.value.* = .{ .table = .{ .source = .header } };

        return .{ .table = &res.value.table, .key = try self.allocator.dupe(u8, al.items[al.items.len - 1]) };
    }

    /// parseArrayHeader parses "[[<key>]]\n" which specifies the next assignments should be in a table in the array
    /// <key>
    ///
    /// NOTE: Assumes "[[" has already been parsed
    fn parseArrayHeader(self: *Parser) !void {
        std.debug.assert(self.current_table == self.top_level_table);

        const tokloc = try self.pop(true);
        const key = switch (tokloc.tok) {
            .key => |k| k,
            .string => |s| s,
            else => {
                self.diag = .{ .msg = "expected key inside of square brackets", .loc = tokloc.loc };
                return error.unexpected_token;
            },
        };

        var res = try self.parseArrayHeaderKey(key, tokloc.loc);
        const existed = res.table.contains(res.key);
        var arr = b: {
            errdefer self.allocator.free(res.key);

            break :b try res.table.getOrPutArray(self.allocator, res.key, .header);
        };

        defer if (existed) self.allocator.free(res.key);

        try arr.array.append(self.allocator, .{ .table = .{ .source = .header } });
        self.current_table = &arr.array.items[arr.array.items.len - 1].table;

        try self.expect(.close_square_bracket, "]");
        try self.expect(.close_square_bracket, "]");
        try self.expect(.newline, "\n");
    }

    pub fn parse(self: *Parser) !Table {
        while (true) {
            const tokloc = self.pop(true) catch |err| switch (err) {
                error.eof => {
                    const table = self.top_level_table.*;
                    self.allocator.destroy(self.top_level_table);

                    self.top_level_table = try self.allocator.create(Table);
                    self.top_level_table.* = .{ .source = .top_level };
                    self.current_table = self.top_level_table;
                    return table;
                },
                else => return err,
            };

            switch (tokloc.tok) {
                .key => |k| try self.parseAssignmentEatNewline(tokloc.loc, k),
                .string => |s| try self.parseAssignmentEatNewline(tokloc.loc, s),
                .open_square_bracket => {
                    self.current_table = self.top_level_table;
                    const next = try self.peek(true);
                    switch (next.tok) {
                        .open_square_bracket => {
                            _ = self.pop(true) catch unreachable;
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

    pub fn deinit(self: *Parser) void {
        self.top_level_table.deinit(self.allocator);
        self.allocator.destroy(self.top_level_table);
        self.lexer.deinit();
    }
};

/// parse takes a given TOML source and returns a Table which has been allocated with the given allocator.
pub fn parse(allocator: std.mem.Allocator, src: []const u8) !Table {
    var parser = try Parser.init(allocator, .{ .real = try lex.Lexer.init(allocator, src) });
    defer parser.deinit();

    return try parser.parse();
}

const KV = struct { k: []const u8, v: Value };

/// kvsToTable takes a slice of KVs (i.e. assignments) and returns the table they would make
fn kvsToTable(kvs: []const KV) !Table {
    var table = Table{ .source = .assignment };
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
        
        try testing.expectEqualStrings("a", table.getTable("foo").?.getString("bar").?);
    }

    {
        var table = try testParse(&.{ 
            .{ .key = "foo"}, .dot, .{ .key = "bar" }, .equals, .{ .string = "a" }, .newline,
            .{ .key = "foo"}, .dot, .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.getTable("foo").?.getString("bar").?);
        try testing.expectEqualStrings("b", table.getTable("foo").?.getString("baz").?);
    }

    {
        var table = try testParse(&.{ 
            .{ .key = "foo"}, .dot, .{ .key = "bar" }, .equals, .{ .string = "a" }, .newline,
            .{ .key = "foo"}, .dot, .{ .string = "baz baz" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.getTable("foo").?.getString("bar").?);
        try testing.expectEqualStrings("b", table.getTable("foo").?.getString("baz baz").?);
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
        try testing.expectEqualStrings("a", table.getTable("foo").?.getString("bar").?);
        try testing.expectEqualStrings("b", table.getTable("foo").?.getString("baz").?);
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
        try testing.expectEqualStrings("a", table.getTable("foo").?.getString("bar").?);
        try testing.expectEqualStrings("b", table.getTable("foo").?.getString("baz").?);
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
            table.getTable("foo").?.getTable("bar").?.getString("baz").?);
        try testing.expectEqualStrings(
            "b",
            table.getTable("foo").?.getTable("bar").?.getString("bat").?);
    }

    {
        var table = try testParse(&.{ 
            .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .newline,
            .{ .key = "a"}, .equals, .{ .integer = 1 }, .newline,
            .open_square_bracket, .{ .key="bar" }, .close_square_bracket, .newline,
            .{ .key = "b" }, .equals, .{ .integer = 2 }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqual(@as(i64, 1), table.getTable("foo").?.getInteger("a").?);
        try testing.expectEqual(@as(i64, 2), table.getTable("bar").?.getInteger("b").?);
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

        try testing.expectEqualSlices(Value,  &.{ .{ .integer = 1 }, .{ .integer = 2 }}, table.getArray("foo").?.items());
    }

    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .newline, .newline, .comma, .newline, .{ .integer = 2 },
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqualSlices(Value,  &.{ .{ .integer = 1 }, .{ .integer = 2 }}, table.getArray("foo").?.items());
    }
    
    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .comma, .{ .string = "bar" },
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqual(@as(usize, 2), table.getArray("foo").?.items().len);
        try testing.expectEqual(Value{ .integer = 1}, table.getArray("foo").?.items()[0]);
        try testing.expectEqualStrings("bar", table.getArray("foo").?.items()[1].string);

    }

    {
        var table = try testParse(&.{.{ .key = "foo" }, .equals, 
            .open_square_bracket, 
                .{ .integer = 1 }, .comma, 
                .open_square_bracket, .{ .integer = 2 }, .comma, .{ .integer = 3 }, .close_square_bracket,
            .close_square_bracket });
        defer table.deinit(testing.allocator);

        try testing.expectEqual(@as(usize, 2), table.getArray("foo").?.items().len);
        try testing.expectEqual(Value{ .integer = 1}, table.getArray("foo").?.items()[0]);
        try testing.expectEqualSlices(Value, 
            &.{ .{ .integer = 2 }, .{ .integer = 3 }},
            table.getArray("foo").?.items()[1].array.items());

    }
    // zig fmt: on
}

test "fail: inline array" {
    try expectErrorParse(error.eof, &.{ .{ .key = "foo" }, .equals, .open_square_bracket });
    try expectErrorParse(error.eof, &.{ .{ .key = "foo" }, .equals, .open_square_bracket, .{ .integer = 1 } });
    try expectErrorParse(error.eof, &.{ .{ .key = "foo" }, .equals, .open_square_bracket, .{ .integer = 1 } });
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
        const arr =  table.getArray("foo").?;
        try testing.expectEqual(@as(usize, 2), arr.items().len);
        try testing.expectEqualStrings("a", arr.items()[0].table.getString("bar").?);
        try testing.expectEqualStrings("b", arr.items()[1].table.getString("baz").?);
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
        const arr =  table.getTable("foo").?.getArray("bar").?;
        try testing.expectEqual(@as(usize, 2), arr.items().len);
        try testing.expectEqual(@as(i64, 1), arr.items()[0].table.getInteger("a").?);
        try testing.expectEqual(@as(i64, 2), arr.items()[1].table.getInteger("b").?);
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

test "array of tables" {
    // zig fmt: off
    {
        var table = try testParse(&.{
            .open_square_bracket, .open_square_bracket, .{ .key = "foo" }, .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "bar"}, .equals, .{ .string = "a" }, .newline,

            .open_square_bracket, .{ .key = "foo" }, .dot, .{ .key = "baz" }, .close_square_bracket, .newline,
            .{ .key = "bat" }, .equals, .{ .string = "b" }, .newline,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqual(@as(usize, 1), table.table.count());
        const arr = table.getArray("foo").?;
        try testing.expectEqual(@as(usize, 1), arr.items().len);
        try testing.expectEqualStrings("a", arr.items()[0].table.getString("bar").?);
        const inner_table = arr.items()[0].table.getTable("baz").?;
        try testing.expectEqualStrings("b", inner_table.getString("bat").?);
    }
    // zig fmt: on
}

test "fail: array of tables" {
    // zig fmt: off
    try expectErrorParse(error.not_array, &.{
            .open_square_bracket, .{ .key="foo" }, .dot, .{ .key = "bar"}, .close_square_bracket, .newline,
            .{ .key = "baz" }, .equals, .{ .string = "b" }, .newline,

            .open_square_bracket, .open_square_bracket, .{ .key="foo" }, .close_square_bracket, .close_square_bracket, .newline,
            .{ .key = "bat"}, .equals, .{ .string = "a" }, .newline,
    });
    // zig fmt: on
}

test "inline tables" {
    // zig fmt: off
    {
        var table = try testParse(&.{ 
            .{ .key = "foo" }, .equals, .open_curly_brace,
                .{ .key = "bar"}, .equals, .{ .string = "a" }, .comma,
                .{ .key = "baz" }, .equals, .{ .string = "b" },
            .close_curly_brace,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings("a", table.getTable("foo").?.getString("bar").?);
        try testing.expectEqualStrings("b", table.getTable("foo").?.getString("baz").?);
    }

    {
        var table = try testParse(&.{ 
            .{ .key = "foo" }, .equals, .open_curly_brace,
                .{ .key = "bar"}, .dot, .{ .key = "baz"}, .equals, .{ .string = "a" },
            .close_curly_brace,
        });
        defer table.deinit(testing.allocator);
        try testing.expectEqualStrings(
            "a", 
            table.getTable("foo").?.getTable("bar").?.getString("baz").?);
    }
    // zig fmt: on
}

test "fail: inline tables" {
    // zig fmt: off
    try expectErrorParse(error.unexpected_token, &.{
            .{ .key = "foo" }, .equals, .open_curly_brace,
                .{ .key = "bar"}, .equals, .{ .string = "a" }, .comma,
            .close_curly_brace,
    });

    try expectErrorParse(error.unexpected_token, &.{
            .{ .key = "foo" }, .equals, .open_curly_brace, .newline,
                .{ .key = "bar"}, .equals, .{ .string = "a" },
            .close_curly_brace,
    });

    try expectErrorParse(error.unexpected_token, &.{
            .{ .key = "foo" }, .equals, .open_curly_brace,
                .{ .key = "bar"}, .equals, .{ .string = "a" }, .comma, .newline,
            .close_curly_brace,
    });
    // zig fmt: on
}
