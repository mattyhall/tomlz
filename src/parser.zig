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
    table: Table,

    fn dupe(self: *const Value, allocator: std.mem.Allocator) !Value {
        return switch (self.*) {
            .string => |s| Value{ .string = try allocator.dupe(u8, s) },
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
    current_table: Table = .{},

    fn peek(self: *Parser) !lex.TokLoc {
        if (self.peeked) |tokloc| return tokloc;

        self.peeked = try self.lexer.next();
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

    /// parseKey parses a key/value assignent to `key`, followed by either a newline or EOF
    fn parseKey(self: *Parser, loc: lex.Loc, key: []const u8) !void {
        if (self.current_table.hasKey(key)) {
            self.diag = .{
                .msg = "key already exists",
                .loc = loc,
            };
            return error.key_already_exists;
        }

        var dup: []const u8 = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(dup);

        // TODO: parse path (e.g. 'foo.bar.baz')
        try self.expect(.equals, "=");

        const tokloc = try self.pop();
        var val = switch (tokloc.tok) {
            .string => |s| Value{ .string = try self.allocator.dupe(u8, s) },
            else => {
                self.diag = .{
                    .msg = "expected value type",
                    .loc = tokloc.loc,
                };
                return error.unexpected_token;
            },
        };
        errdefer val.deinit(self.allocator);

        try self.current_table.insert(self.allocator, dup, val);
        // we have errdefer'ed freeing dup and val above, so we need to remove them from current_table so we don't try
        // to double free them when we deinit current_table
        errdefer _ = self.current_table.table.remove(dup);

        self.expect(.newline, "\n") catch |err| switch (err) {
            error.eof => return,
            else => return err,
        };
    }

    fn parse(self: *Parser) !Table {
        while (true) {
            const tokloc = self.pop() catch |err| switch (err) {
                error.eof => {
                    const table = self.current_table;
                    self.current_table = .{};
                    return table;
                },
                else => return err,
            };

            switch (tokloc.tok) {
                .key => |k| try self.parseKey(tokloc.loc, k),
                .string => |s| try self.parseKey(tokloc.loc, s),
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
        self.current_table.deinit(self.allocator);
    }
};

const KV = struct { k: []const u8, v: Value };

fn kvsToTable(kvs: []const KV) !Table {
    var table = Table{};
    for (kvs) |entry| {
        const v = try entry.v.dupe(testing.allocator);
        try table.insert(testing.allocator, try testing.allocator.dupe(u8, entry.k), v);
    }
    return table;
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

fn toksToLocToks(toks: []const lex.Tok) ![]lex.TokLoc {
    const loc = lex.Loc{ .line = 1, .col = 1 };

    var al = std.ArrayListUnmanaged(lex.TokLoc){};
    for (toks) |tok| {
        try al.append(testing.allocator, .{ .tok = tok, .loc = loc });
    }

    return al.items;
}

fn expectEqualParses(toks: []const lex.Tok, expected: []const KV) !void {
    var toklocs = try toksToLocToks(toks);
    defer testing.allocator.free(toklocs);

    var lexer = Lexer{ .fake = .{ .toklocs = toklocs } };

    var parser = Parser{ .lexer = lexer, .allocator = testing.allocator };
    var parsed_table = try parser.parse();
    defer parsed_table.deinit(testing.allocator);

    var expected_table = try kvsToTable(expected);
    defer expected_table.deinit(testing.allocator);

    try expectEqualTables(expected_table, parsed_table);
}

fn expectErrorParse(toks: []const lex.Tok) !void {
    var toklocs = try toksToLocToks(toks);
    defer testing.allocator.free(toklocs);

    var lexer = Lexer{ .fake = .{ .toklocs = toklocs } };
    var parser = Parser{ .lexer = lexer, .allocator = testing.allocator };
    defer parser.deinit();

    try testing.expectError(error.unexpected_token, parser.parse());
}

test "default table assignment" {
    try expectEqualParses(
        &.{ .{ .key = "foo" }, .equals, .{ .string = "a" } },
        &.{.{ .k = "foo", .v = .{ .string = "a" } }},
    );

    try expectEqualParses(
        &.{ .{ .string = "foo" }, .equals, .{ .string = "a" }, .newline, .{ .key = "bar" }, .equals, .{ .string = "b" } },
        &.{
            .{ .k = "foo", .v = .{ .string = "a" } },
            .{ .k = "bar", .v = .{ .string = "b" } },
        },
    );
}

test "fail assignment" {
    try expectErrorParse(&.{.equals});
    try expectErrorParse(&.{ .{ .key = "foo" }, .newline });
    try expectErrorParse(&.{ .{ .string = "foo" }, .equals, .{ .string = "a" }, .{ .key = "bar" }, .equals, .{ .string = "b" } });
}
