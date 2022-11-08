const std = @import("std");
const testing = std.testing;

pub const Loc = struct {
    line: u64,
    col: u64,
};

pub const Tok = union(enum) {
    equals,
    newline,
    dot,
    open_square_bracket,
    close_square_bracket,
    open_curly_brace,
    close_curly_brace,
    comma,

    key: []const u8,

    string: []const u8,
    integer: i64,
    float: f64,
    boolean: bool,

    pub fn dupe(self: Tok, allocator: std.mem.Allocator) !Tok {
        return switch (self) {
            .key => |k| Tok{ .key = try allocator.dupe(u8, k) },
            .string => |k| Tok{ .string = try allocator.dupe(u8, k) },
            else => self,
        };
    }

    pub fn deinit(self: Tok, allocator: std.mem.Allocator) void {
        switch (self) {
            .key => |k| allocator.free(k),
            .string => |s| allocator.free(s),
            else => {},
        }
    }
};

pub const TokLoc = struct { tok: Tok, loc: Loc };

pub const Diagnostic = struct {
    loc: Loc,
    msg: []const u8,
};

/// Lexer splits its given source TOML file into tokens
pub const Lexer = struct {
    arena: std.heap.ArenaAllocator,
    source: []const u8,
    loc: Loc,
    index: usize,
    diag: ?Diagnostic,

    pub const Error = error{ eof, unexpected_char, OutOfMemory };

    pub fn init(allocator: std.mem.Allocator, src: []const u8) Lexer {
        var arena = std.heap.ArenaAllocator.init(allocator);
        return .{ .arena = arena, .source = src, .loc = .{ .line = 1, .col = 1 }, .diag = null, .index = 0 };
    }

    fn peek(self: *Lexer) Error!u8 {
        if (self.index >= self.source.len) {
            self.diag = .{
                .loc = .{ .line = self.loc.line + 1, .col = 1 },
                .msg = "end of file",
            };
            return error.eof;
        }

        return self.source[self.index];
    }

    fn pop(self: *Lexer) Error!u8 {
        if (self.index >= self.source.len) {
            self.diag = .{
                .loc = .{ .line = self.loc.line + 1, .col = 1 },
                .msg = "end of file",
            };
            return error.eof;
        }

        const c = self.source[self.index];
        self.index += 1;
        if (c == '\n') {
            self.loc.line += 1;
            self.loc.col = 0;
        }

        return c;
    }

    /// parseEscapeChar parses the valid escape codes allowed in TOML (i.e. those allowed in a string beginning with a
    /// backslash)
    fn parseEscapeChar(self: *Lexer) Error!u8 {
        defer _ = self.pop() catch unreachable;

        return switch (try self.peek()) {
            'b' => 8,
            't' => '\t',
            'n' => '\n',
            'r' => '\r',
            'f' => 10,
            '"' => '"',
            '\\' => '\\',
            else => {
                self.diag = Diagnostic{ .loc = self.loc, .msg = "unexpected escape character" };
                return error.unexpected_char;
            },
        };
    }

    fn parseString(self: *Lexer) Error!TokLoc {
        const loc = self.loc;
        var al = std.ArrayListUnmanaged(u8){};
        while (true) {
            switch (try self.pop()) {
                '"' => return TokLoc{ .loc = loc, .tok = .{ .string = al.items } },
                '\\' => try al.append(self.arena.allocator(), try self.parseEscapeChar()),
                else => |c| try al.append(self.arena.allocator(), c),
            }
        }
    }

    /// parseKey parses a key. A key can only contain [A-Za-z0-9-_]
    fn parseKey(self: *Lexer) Error!TokLoc {
        const loc = self.loc;
        var al = std.ArrayListUnmanaged(u8){};
        while (true) {
            const c = self.peek() catch |err| {
                switch (err) {
                    error.eof => return TokLoc{ .loc = loc, .tok = .{ .key = al.items } },
                    else => return err,
                }
            };

            if ((c >= '0' and c <= '9') or (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or c == '-' or c == '_') {
                _ = try self.pop();
                try al.append(self.arena.allocator(), c);
                continue;
            }

            if (std.mem.indexOf(u8, " \t\r.=]", &.{c}) == null) {
                self.diag = Diagnostic{
                    .loc = self.loc,
                    .msg = "expected one of '\t', '\r', ' ', '.', '=', ']' after a key",
                };
                return error.unexpected_char;
            }

            return TokLoc{ .loc = loc, .tok = .{ .key = al.items } };
        }
    }

    fn skipComment(self: *Lexer) Error!void {
        while (true) {
            const c = try self.peek();
            if (c == '\n') return;

            const loc = self.loc;
            _ = self.pop() catch unreachable;
            // From the spec: "Control characters other than tab (U+0000 to U+0008, U+000A to U+001F, U+007F) are not
            // permitted in comments."
            if ((c >= 0x0 and c <= 0x8) or (c >= 0xA and c <= 0x1f) or c == 0x7f) {
                self.diag = .{ .loc = loc, .msg = "unexpected control character in comment" };
                return error.unexpected_char;
            }
        }
    }

    /// skipWhitespace skips any non significant (i.e. not a newline) whitespace
    fn skipWhitespaceAndComment(self: *Lexer) Error!void {
        while (true) {
            const c = try self.peek();
            if (c == '#') {
                _ = self.pop() catch unreachable;
                return try self.skipComment();
            }

            if (c == '\n' or !std.ascii.isSpace(c)) return;

            _ = self.pop() catch unreachable;
        }
    }

    pub fn parseNumber(self: *Lexer) Error!TokLoc {
        var sign: i64 = 1;
        var base: u8 = 10;
        switch (self.peek() catch unreachable) {
            '-' => {
                sign = -1;
                _ = self.pop() catch unreachable;
            },
            '+' => _ = self.pop() catch unreachable,
            else => {},
        }

        var loc = self.loc;
        var c = try self.pop();
        if (c == '0') {
            const radix = self.peek() catch |err| switch (err) {
                error.eof => return TokLoc{ .tok = .{ .integer = 0 }, .loc = loc },
                else => return err,
            };
            switch (radix) {
                'b' => base = 2,
                'o' => base = 8,
                'x' => base = 16,
                else => {
                    if (std.ascii.isWhitespace(radix)) return TokLoc{ .tok = .{ .integer = 0 }, .loc = self.loc };

                    self.diag = .{ .msg = "expected 'b', 'o' or 'x' after '0'", .loc = self.loc };
                    return error.unexpected_char;
                },
            }
            _ = self.pop() catch unreachable;
        }

        var i: i64 = 0;
        while (true) {
            var digit = std.fmt.parseInt(i64, &.{c}, base) catch {
                self.diag = .{ .msg = "expected number", .loc = loc };
                return error.unexpected_char;
            };
            i = i * base + digit;

            loc = self.loc;
            c = self.peek() catch |err| switch (err) {
                error.eof => return TokLoc{ .tok = .{ .integer = i * sign }, .loc = loc },
                else => return err,
            };
            if (!std.ascii.isAlphanumeric(c))
                return TokLoc{ .tok = .{ .integer = i * sign }, .loc = loc };

            _ = self.pop() catch unreachable;
        }
    }

    fn parseKeyword(self: *Lexer, rest: []const u8, tok: Tok) Error!TokLoc {
        const loc = self.loc;

        const full_len = rest.len + 1;
        if (self.source.len - self.index < full_len) return try self.parseKey();
        if (!std.mem.eql(u8, rest, self.source[self.index + 1 .. self.index + full_len])) return try self.parseKey();

        // The character after rest must be either whitespace or an equals for this to be a keyword
        if (self.source.len - self.index >= full_len + 1 and
            (!std.ascii.isWhitespace(self.source[self.index + full_len])) and self.source[self.index + full_len] != '=')
            return try self.parseKey();

        self.index += full_len;
        self.loc.col += full_len;
        return TokLoc{ .loc = loc, .tok = tok };
    }
    
    fn consume(self: *Lexer, tok: Tok, loc: Loc) TokLoc {
        _ = self.pop() catch unreachable;
        return .{ .tok = tok, .loc = loc };
    }

    /// next gives the next token, or null if there are none left.
    ///
    /// NOTE: any memory returned in TokLoc (e.g. the []const u8 array in a key/string) is only valid until the next
    /// call of next
    pub fn next(self: *Lexer) Error!?TokLoc {
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.arena.child_allocator);

        self.skipWhitespaceAndComment() catch |err| switch (err) {
            error.eof => return null,
            else => return err,
        };

        const c = self.peek() catch |err| switch (err) {
            error.eof => return null,
            else => return err,
        };
        const loc = self.loc;

        switch (c) {
            '=' => return self.consume(.equals, loc),
            '.' => return self.consume(.dot, loc),
            ',' => return self.consume(.comma, loc),
            '[' => return self.consume(.open_square_bracket, loc),
            ']' => return self.consume(.close_square_bracket, loc),
            '{' => return self.consume(.open_curly_brace, loc),
            '}' => return self.consume(.close_curly_brace, loc),
            '\n' => return self.consume(.newline, loc),
            '"' => {
                _ = self.pop() catch unreachable;
                return try self.parseString();
            },
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => return try self.parseNumber(),
            't' => return try self.parseKeyword("rue", .{ .boolean = true }),
            'f' => return try self.parseKeyword("alse", .{ .boolean = false }),
            else => return try self.parseKey(),
        }
    }
};

pub const Fake = struct {
    toklocs: []TokLoc,
    index: usize = 0,

    pub fn next(self: *Fake) Lexer.Error!?TokLoc {
        if (self.index >= self.toklocs.len) return null;

        self.index += 1;
        return self.toklocs[self.index - 1];
    }
};

fn readAllTokens(src: []const u8) ![]const Tok {
    var lexer = Lexer.init(testing.allocator, src);
    var al = std.ArrayListUnmanaged(Tok){};
    while (try lexer.next()) |tok_loc| {
        try al.append(testing.allocator, try tok_loc.tok.dupe(std.testing.allocator));
    }

    return al.items;
}

fn testTokens(src: []const u8, expected: []const Tok) !void {
    var toks = try readAllTokens(src);
    defer testing.allocator.free(toks);

    try testing.expectEqual(expected.len, toks.len);

    {
        var i: usize = 0;
        while (i < expected.len) : (i += 1) {
            const actual = toks[i];
            const exp = expected[i];
            try testing.expectEqual(std.meta.activeTag(exp), std.meta.activeTag(actual));

            switch (exp) {
                .key => |k| try testing.expectEqualStrings(k, actual.key),
                .string => |s| try testing.expectEqualStrings(s, actual.string),
                else => try testing.expectEqual(exp, actual),
            }

            actual.deinit(testing.allocator);
        }
    }
}

test "normal keys" {
    try testTokens("foo", &.{.{ .key = "foo" }});
    try testTokens("foo-bar", &.{.{ .key = "foo-bar" }});
    try testTokens("foo_bar", &.{.{ .key = "foo_bar" }});
    try testTokens("1234", &.{.{ .integer = 1234 }});
    try testTokens("foo.bar", &.{ .{ .key = "foo" }, .dot, .{ .key = "bar" } });
}

test "quotation work" {
    try testTokens("\"foo\"", &.{.{ .string = "foo" }});
    try testTokens("\"!!!\"", &.{.{ .string = "!!!" }});
    try testTokens("\"foo bar baz\"", &.{.{ .string = "foo bar baz" }});
    try testTokens("\"foo.bar.baz\"", &.{.{ .string = "foo.bar.baz" }});

    try testTokens(
        \\"foo \"bar\" baz"
    , &.{.{ .string = 
    \\foo "bar" baz
    }});

    try testTokens("\"foo \\n bar\"", &.{.{ .string = "foo \n bar" }});
    try testTokens("\"foo \\t bar\"", &.{.{ .string = "foo \t bar" }});
}

test "key/value" {
    try testTokens("foo = \"hi\"", &.{ .{ .key = "foo" }, .equals, .{ .string = "hi" } });
    try testTokens("foo    = \"hi\"", &.{ .{ .key = "foo" }, .equals, .{ .string = "hi" } });
    try testTokens("foo \t=\"hi\"", &.{ .{ .key = "foo" }, .equals, .{ .string = "hi" } });
    try testTokens("foo=\"hi\"", &.{ .{ .key = "foo" }, .equals, .{ .string = "hi" } });
    try testTokens("foo=\"hi\"\n", &.{ .{ .key = "foo" }, .equals, .{ .string = "hi" }, .newline });
    try testTokens("\"foo bar baz\"=\"hi\"\n", &.{ .{ .string = "foo bar baz" }, .equals, .{ .string = "hi" }, .newline });
    try testTokens("foo.bar=\"hi\"", &.{ .{ .key = "foo" }, .dot, .{ .key = "bar" }, .equals, .{ .string = "hi" } });
}

test "comments" {
    try testTokens("# foo bar baz\n", &.{.newline});
    try testTokens("# foo bar\tbaz\n", &.{.newline});
    try testTokens("foo # comment\n", &.{ .{ .key = "foo" }, .newline });
}

test "integers" {
    try testTokens("0", &.{.{ .integer = 0 }});
    try testTokens("147", &.{.{ .integer = 147 }});
    try testTokens("+147", &.{.{ .integer = 147 }});
    try testTokens("-147", &.{.{ .integer = -147 }});
    try testTokens("0x147abc", &.{.{ .integer = 0x147abc }});
    try testTokens("0o147", &.{.{ .integer = 0o147 }});
    try testTokens("-0x147abc", &.{.{ .integer = -0x147abc }});
    try testTokens("-0o147", &.{.{ .integer = -0o147 }});
    try testTokens("+0x147abc", &.{.{ .integer = 0x147abc }});
    try testTokens("+0o147", &.{.{ .integer = 0o147 }});
    try testTokens("0b10010011", &.{.{ .integer = 0b10010011 }});
}

test "booleans" {
    try testTokens("true", &.{.{ .boolean = true }});
    try testTokens("true ", &.{.{ .boolean = true }});
    try testTokens("true=", &.{ .{ .boolean = true }, .equals });
    try testTokens("false", &.{.{ .boolean = false }});
    try testTokens("false ", &.{.{ .boolean = false }});
    try testTokens("false=", &.{ .{ .boolean = false }, .equals });
    try testTokens("truee", &.{.{ .key = "truee" }});
    try testTokens("falsee", &.{.{ .key = "falsee" }});
    try testTokens("tr ue", &.{ .{ .key = "tr" }, .{ .key = "ue" } });
    try testTokens("fal se", &.{ .{ .key = "fal" }, .{ .key = "se" } });
}

test "square brackets" {
    try testTokens("[]", &.{ .open_square_bracket, .close_square_bracket });
    try testTokens("[foo]", &.{ .open_square_bracket, .{ .key = "foo" }, .close_square_bracket });
    try testTokens("[[foo]]", &.{
        .open_square_bracket,
        .open_square_bracket,
        .{ .key = "foo" },
        .close_square_bracket,
        .close_square_bracket,
    });
    try testTokens(
        "[1,2]",
        &.{ .open_square_bracket, .{ .integer = 1 }, .comma, .{ .integer = 2 }, .close_square_bracket },
    );
}

test "curly braces" {
    try testTokens("{}", &.{ .open_curly_brace, .close_curly_brace });
    try testTokens("{foo = 10}", &.{ .open_curly_brace, .{ .key = "foo" }, .equals, .{ .integer = 10 }, .close_curly_brace });
}
