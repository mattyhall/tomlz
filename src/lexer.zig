const std = @import("std");
const testing = std.testing;

const AllocError = std.mem.Allocator.Error;

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

    pub fn dupe(self: Tok, allocator: std.mem.Allocator) AllocError!Tok {
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

const Quotation = enum { single, double };

/// Lexer splits its given source TOML file into tokens
pub const Lexer = struct {
    arena: std.heap.ArenaAllocator,
    source: []const u8,
    loc: Loc,
    index: usize,
    diag: ?Diagnostic,

    pub const Error = error{
        EOF,
        UnexpectedChar,
        OutOfMemory,
        StringNotEnded,
        InvalidCodepoint,
    };

    pub fn init(allocator: std.mem.Allocator, src: []const u8) !Lexer {
        if (!std.unicode.utf8ValidateSlice(src))
            return error.NotUTF8;

        const arena = std.heap.ArenaAllocator.init(allocator);
        return Lexer{ .arena = arena, .source = src, .loc = .{ .line = 1, .col = 1 }, .diag = null, .index = 0 };
    }

    fn peek(self: *Lexer) Error!u8 {
        if (self.index >= self.source.len) {
            self.diag = .{
                .loc = .{ .line = self.loc.line + 1, .col = 1 },
                .msg = "end of file",
            };
            return error.EOF;
        }

        return self.source[self.index];
    }

    fn pop(self: *Lexer) Error!u8 {
        if (self.index >= self.source.len) {
            self.diag = .{
                .loc = .{ .line = self.loc.line + 1, .col = 1 },
                .msg = "end of file",
            };
            return error.EOF;
        }

        const c = self.source[self.index];
        self.index += 1;
        self.loc.col += 1;
        if (c == '\n') {
            self.loc.line += 1;
            self.loc.col = 1;
        }

        return c;
    }

    fn peek2(self: *Lexer) ?[]const u8 {
        if (self.index + 1 >= self.source.len) return null;

        return self.source[self.index .. self.index + 2];
    }

    fn peek3(self: *Lexer) ?[]const u8 {
        if (self.index + 2 >= self.source.len) return null;

        return self.source[self.index .. self.index + 3];
    }

    fn parseUnicode(self: *Lexer, len: u4, al: *std.ArrayListUnmanaged(u8)) Error!void {
        var i: usize = 1;
        var codepoint: u21 = 0;
        var buf: [16]u8 = undefined;
        while (true) : (i += 1) {
            const c = try self.peek();
            if (c != '0') break;
            _ = self.pop() catch unreachable;
        }

        while (true) : (i += 1) {
            const c = try self.peek();
            const n = std.fmt.parseInt(u21, &.{c}, 16) catch {
                if (i == @as(usize, @intCast(len)) + 1) {
                    const written = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                    try al.appendSlice(self.arena.allocator(), buf[0..written]);
                    return;
                }

                return error.UnexpectedChar;
            };

            _ = self.pop() catch unreachable;

            if (std.math.maxInt(u21) < 16 * @as(u64, @intCast(codepoint)) + n) return error.InvalidCodepoint;

            codepoint = codepoint * 16 + n;
            if (i == len) {
                const written = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                try al.appendSlice(self.arena.allocator(), buf[0..written]);
                return;
            }
        }
    }

    /// parseEscapeChar parses the valid escape codes allowed in TOML (i.e. those allowed in a string beginning with a
    /// backslash)
    fn parseEscapeChar(self: *Lexer, al: *std.ArrayListUnmanaged(u8)) Error!void {
        const c: u8 = switch (try self.peek()) {
            'b' => 8,
            't' => '\t',
            'n' => '\n',
            'r' => '\r',
            'f' => '\x0c',
            'e' => '\x1b',
            'u' => {
                _ = self.pop() catch unreachable;
                return try self.parseUnicode(4, al);
            },
            'U' => {
                _ = self.pop() catch unreachable;
                return try self.parseUnicode(8, al);
            },
            '"' => '"',
            '\'' => '\'',
            '\\' => '\\',
            else => {
                self.diag = Diagnostic{ .loc = self.loc, .msg = "unexpected escape character" };
                return error.UnexpectedChar;
            },
        };

        _ = self.pop() catch unreachable;

        try al.append(self.arena.allocator(), c);
    }

    fn parseMultiline(self: *Lexer, typ: Quotation) Error!TokLoc {
        const loc = self.loc;
        var al = std.ArrayListUnmanaged(u8){};
        var first = true;
        while (true) {
            const c = self.pop() catch |err| switch (err) {
                error.EOF => return error.StringNotEnded,
                else => return err,
            };

            if (c == '\n' and first) continue;

            first = false;

            switch (c) {
                '"' => if (typ == .single) {
                    try al.append(self.arena.allocator(), c);
                } else {
                    if (self.peek2()) |two| {
                        if (!std.mem.eql(u8, two, "\"\"")) {
                            try al.append(self.arena.allocator(), c);
                            continue;
                        }
                    }

                    // If we are here we have at least three quotes in a row. We need to keep taking quotes until we are
                    // at the last three
                    var i: usize = 0;
                    while (self.peek3()) |three| {
                        if (three[2] != c) break;

                        _ = try self.pop();
                        try al.append(self.arena.allocator(), c);

                        i += 1;
                        if (i == 2) break;
                    }

                    _ = try self.pop();
                    _ = try self.pop();

                    return TokLoc{ .loc = loc, .tok = .{ .string = al.items } };
                },
                '\'' => if (typ == .double) {
                    try al.append(self.arena.allocator(), c);
                } else {
                    if (self.peek2()) |two| {
                        if (!std.mem.eql(u8, two, "''")) {
                            try al.append(self.arena.allocator(), c);
                            continue;
                        }
                    }

                    // If we are here we have at least three quotes in a row. We need to keep taking quotes until we are
                    // at the last three
                    var i: usize = 0;
                    while (self.peek3()) |three| {
                        if (three[2] != c) break;

                        _ = try self.pop();
                        try al.append(self.arena.allocator(), c);

                        i += 1;
                        if (i == 2) break;
                    }

                    _ = try self.pop();
                    _ = try self.pop();

                    return TokLoc{ .loc = loc, .tok = .{ .string = al.items } };
                },
                '\\' => {
                    if (typ == .single) {
                        try al.append(self.arena.allocator(), c);
                        continue;
                    }

                    // A trailing slash in a multiline string means trim up to the next non-whitespace character
                    var p = try self.peek();

                    if (std.ascii.isWhitespace(p)) {
                        try self.skipWhitespaceAndComment();
                        if (self.pop() catch unreachable != '\n') return error.UnexpectedChar;

                        while (true) {
                            p = try self.peek();
                            if (std.ascii.isWhitespace(p)) {
                                _ = self.pop() catch unreachable;
                                continue;
                            }
                            break;
                        }
                        continue;
                    }

                    try self.parseEscapeChar(&al);
                },
                else => {
                    if ((c >= 0x0 and c <= 0x8) or c == 0x0B or c == 0x0C or (c >= 0xE and c <= 0x1f) or c == 0x7f) {
                        self.diag = .{ .loc = loc, .msg = "unexpected control character in string" };
                        return error.UnexpectedChar;
                    }

                    try al.append(self.arena.allocator(), c);
                },
            }
        }
    }

    fn parseString(self: *Lexer, typ: Quotation, force_key: bool) Error!TokLoc {
        if (self.peek2()) |two| {
            if ((typ == .double and std.mem.eql(u8, two, "\"\"")) or (std.mem.eql(u8, two, "''"))) {
                if (force_key) {
                    self.diag = .{ .msg = "cannot use a multiline string as a key", .loc = self.loc };
                    return error.UnexpectedChar;
                }

                _ = self.pop() catch unreachable;
                _ = self.pop() catch unreachable;
                return try self.parseMultiline(typ);
            }
        }

        const loc = self.loc;
        var al = std.ArrayListUnmanaged(u8){};
        while (true) {
            const c = self.pop() catch |err| switch (err) {
                error.EOF => return error.StringNotEnded,
                else => return err,
            };
            switch (c) {
                '"' => if (typ == .single)
                    try al.append(self.arena.allocator(), c)
                else
                    return TokLoc{ .loc = loc, .tok = .{ .string = al.items } },
                '\'' => if (typ == .double)
                    try al.append(self.arena.allocator(), c)
                else
                    return TokLoc{ .loc = loc, .tok = .{ .string = al.items } },
                '\\' => {
                    if (typ == .single) {
                        try al.append(self.arena.allocator(), c);
                        continue;
                    }
                    try self.parseEscapeChar(&al);
                },
                else => {
                    if ((c >= 0x0 and c <= 0x8) or (c >= 0xA and c <= 0x1f) or c == 0x7f) {
                        self.diag = .{ .loc = loc, .msg = "unexpected control character in string" };
                        return error.UnexpectedChar;
                    }

                    try al.append(self.arena.allocator(), c);
                },
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
                    error.EOF => return TokLoc{ .loc = loc, .tok = .{ .key = al.items } },
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
                return error.UnexpectedChar;
            }

            return TokLoc{ .loc = loc, .tok = .{ .key = al.items } };
        }
    }

    fn skipComment(self: *Lexer) Error!void {
        while (true) {
            var c = try self.peek();
            if (c == '\r') {
                _ = self.pop() catch unreachable;
                c = try self.peek();
                if (c != '\n') {
                    self.diag = .{
                        .loc = self.loc,
                        .msg = "\\r can only appear at the end of a comment",
                    };
                    return error.UnexpectedChar;
                }
            }

            if (c == '\n') return;

            const loc = self.loc;
            _ = self.pop() catch unreachable;
            // From the spec: "Control characters other than tab (U+0000 to U+0008, U+000A to U+001F, U+007F) are not
            // permitted in comments."
            if ((c >= 0x0 and c <= 0x8) or (c >= 0xA and c <= 0x1f) or c == 0x7f) {
                self.diag = .{ .loc = loc, .msg = "unexpected control character in comment" };
                return error.UnexpectedChar;
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

            if (c == '\r') {
                _ = self.pop() catch unreachable;
                const p = self.peek() catch |err| switch (err) {
                    error.EOF => {
                        self.diag = .{ .msg = "expected \\n after \\r", .loc = self.loc };
                        return error.UnexpectedChar;
                    },
                    else => return err,
                };

                if (p != '\n') {
                    self.diag = .{ .msg = "expected \\n after \\r", .loc = self.loc };
                    return error.UnexpectedChar;
                }

                return;
            }

            if (c == '\n' or !std.ascii.isWhitespace(c)) return;

            _ = self.pop() catch unreachable;
        }
    }

    pub fn parseNumber(self: *Lexer) Error!TokLoc {
        var explicit_sign = false;
        var base: u8 = 10;
        const original_index = self.index;
        switch (self.peek() catch unreachable) {
            '-' => {
                explicit_sign = true;
                _ = self.pop() catch unreachable;
            },
            '+' => {
                explicit_sign = true;
                _ = self.pop() catch unreachable;
            },
            else => {},
        }

        if (self.source.len - self.index > 3) {
            if (std.mem.eql(u8, self.source[self.index .. self.index + 3], "nan")) {
                _ = self.pop() catch unreachable;
                _ = self.pop() catch unreachable;
                _ = self.pop() catch unreachable;
                return TokLoc{ .tok = .{ .float = std.math.nan(f64) }, .loc = self.loc };
            }
            if (std.mem.eql(u8, self.source[self.index .. self.index + 3], "inf")) {
                _ = self.pop() catch unreachable;
                _ = self.pop() catch unreachable;
                _ = self.pop() catch unreachable;
                return TokLoc{ .tok = .{ .float = std.math.inf(f64) }, .loc = self.loc };
            }
        }

        var c = try self.pop();
        var had_number = false;
        var is_float = false;
        if (c == '0') {
            had_number = true;
            const radix = self.peek() catch |err| switch (err) {
                error.EOF => return TokLoc{ .tok = .{ .integer = 0 }, .loc = self.loc },
                else => return err,
            };
            switch (radix) {
                'b' => base = 2,
                'o' => base = 8,
                'e', 'E' => is_float = true,
                'x' => base = 16,
                '.' => is_float = true,
                else => {
                    if (std.ascii.isWhitespace(radix)) return TokLoc{ .tok = .{ .integer = 0 }, .loc = self.loc };

                    self.diag = .{ .msg = "expected 'b', 'o' or 'x' after '0'", .loc = self.loc };
                    return error.UnexpectedChar;
                },
            }

            if (explicit_sign and base != 10) {
                self.diag = .{ .msg = "only base 10 numbers can have an explicit sign", .loc = self.loc };
                return error.UnexpectedChar;
            }

            c = try self.pop();
        }

        var last_c: ?u8 = null;
        while (true) {
            if (c == '.') {
                if (!had_number) {
                    self.diag = .{ .msg = "cannot have leading '.' in float", .loc = self.loc };
                    return error.UnexpectedChar;
                }

                is_float = true;
            }

            if ((c == 'e' or c == 'E') and base != 16) {
                if (last_c == @as(u8, '.')) {
                    self.diag = .{ .msg = "number must follow dot", .loc = self.loc };
                    return error.UnexpectedChar;
                }

                is_float = true;
            }

            last_c = c;
            c = self.peek() catch |err| switch (err) {
                error.EOF => {
                    if (last_c != @as(u8, '_')) break;

                    self.diag = .{ .msg = "trailing underscores not allowed", .loc = self.loc };
                    return error.UnexpectedChar;
                },
                else => return err,
            };

            if (c == @as(u8, '_') and last_c == @as(u8, '_')) {
                self.diag = .{ .msg = "double underscores not allowed", .loc = self.loc };
                return error.UnexpectedChar;
            }

            if ((c == '+' or c == '-') and last_c != @as(u8, 'e') and last_c != @as(u8, 'E')) {
                self.diag = .{ .msg = "'+' and '-' can only follow an 'e'", .loc = self.loc };
                return error.UnexpectedChar;
            }

            if (!std.ascii.isAlphanumeric(c) and c != '.' and c != '_' and c != '+' and c != '-') {
                if (last_c != @as(u8, '_')) break;

                self.diag = .{ .msg = "trailing underscores not allowed", .loc = self.loc };
                return error.UnexpectedChar;
            }

            had_number = true;
            _ = self.pop() catch unreachable;
        }

        const slice = self.source[original_index..self.index];
        if (slice[slice.len - 1] == '.') {
            self.diag = .{ .msg = "trailing dot not allowed", .loc = self.loc };
            return error.UnexpectedChar;
        }

        if (is_float) return TokLoc{
            .tok = .{ .float = std.fmt.parseFloat(f64, slice) catch {
                return error.UnexpectedChar;
            } },
            .loc = self.loc,
        };

        return TokLoc{
            .tok = .{ .integer = std.fmt.parseInt(i64, slice, 0) catch {
                return error.UnexpectedChar;
            } },
            .loc = self.loc,
        };
    }

    fn parseKeyword(self: *Lexer, rest: []const u8, tok: Tok) Error!TokLoc {
        const loc = self.loc;

        const full_len = rest.len + 1;
        if (self.source.len - self.index < full_len) return try self.parseKey();
        if (!std.mem.eql(u8, rest, self.source[self.index + 1 .. self.index + full_len])) return try self.parseKey();

        // The character after rest must be either whitespace, an equals, a comma or a close curly brace for this to be
        // a keyword
        if (self.source.len - self.index >= full_len + 1 and
            !std.ascii.isWhitespace(self.source[self.index + full_len]) and
            self.source[self.index + full_len] != '=' and self.source[self.index + full_len] != ',' and
            self.source[self.index + full_len] != '}' and self.source[self.index + full_len] != ']')
            return try self.parseKey();

        self.index += full_len;
        self.loc.col += full_len;
        return TokLoc{ .loc = loc, .tok = tok };
    }

    fn consume(self: *Lexer, tok: Tok) TokLoc {
        const loc = self.loc;
        _ = self.pop() catch unreachable;
        return .{ .tok = tok, .loc = loc };
    }

    /// next gives the next token, or null if there are none left. Force key ensures that numbers/keywords are parsed as
    /// keys rather than their normal type. This is needed as TOML allows these values to be used as keys in assignments.
    ///
    /// NOTE: any memory returned in TokLoc (e.g. the []const u8 array in a key/string) is only valid until the next
    /// call of next
    pub fn next(self: *Lexer, force_key: bool) Error!?TokLoc {
        const child = self.arena.child_allocator;

        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(child);

        self.skipWhitespaceAndComment() catch |err| switch (err) {
            error.EOF => return null,
            else => return err,
        };

        const c = self.peek() catch |err| switch (err) {
            error.EOF => return null,
            else => return err,
        };

        const loc = self.loc;

        switch (c) {
            '=' => return self.consume(.equals),
            '.' => return self.consume(.dot),
            ',' => return self.consume(.comma),
            '[' => return self.consume(.open_square_bracket),
            ']' => return self.consume(.close_square_bracket),
            '{' => return self.consume(.open_curly_brace),
            '}' => return self.consume(.close_curly_brace),
            '\n' => return self.consume(.newline),
            '\r' => {
                _ = self.pop() catch unreachable;

                if (try self.peek() == '\n') return self.consume(.newline);

                self.diag = .{ .msg = "expect a \\n after a \\r", .loc = loc };
                return error.UnexpectedChar;
            },
            '"' => {
                _ = self.pop() catch unreachable;
                return try self.parseString(.double, force_key);
            },
            '\'' => {
                _ = self.pop() catch unreachable;
                return try self.parseString(.single, force_key);
            },
            '-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' => {
                if (force_key) return try self.parseKey();
                return try self.parseNumber();
            },
            't' => {
                if (force_key) return try self.parseKey();
                return try self.parseKeyword("rue", .{ .boolean = true });
            },
            'f' => {
                if (force_key) return try self.parseKey();
                return try self.parseKeyword("alse", .{ .boolean = false });
            },
            'n' => {
                if (force_key) return try self.parseKey();
                return try self.parseKeyword("an", .{ .float = std.math.nan(f64) });
            },
            'i' => {
                if (force_key) return try self.parseKey();
                return try self.parseKeyword("nf", .{ .float = std.math.inf(f64) });
            },
            else => return try self.parseKey(),
        }
    }

    pub fn deinit(self: *Lexer) void {
        self.arena.deinit();
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
    var lexer = try Lexer.init(testing.allocator, src);
    defer lexer.deinit();

    var al = std.ArrayListUnmanaged(Tok){};
    while (try lexer.next(false)) |tok_loc| {
        try al.append(testing.allocator, try tok_loc.tok.dupe(testing.allocator));
    }

    return al.toOwnedSlice(testing.allocator);
}

fn testTokens(src: []const u8, expected: []const Tok) !void {
    const toks = try readAllTokens(src);
    defer {
        for (toks) |tok| {
            tok.deinit(testing.allocator);
        }
        testing.allocator.free(toks);
    }

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

test "quotation works" {
    try testTokens("\"foo\"", &.{.{ .string = "foo" }});
    try testTokens("\"!!!\"", &.{.{ .string = "!!!" }});
    try testTokens("\"foo bar baz\"", &.{.{ .string = "foo bar baz" }});
    try testTokens("\"foo.bar.baz\"", &.{.{ .string = "foo.bar.baz" }});
    try testTokens("'foo'", &.{.{ .string = "foo" }});
    try testTokens("'!!!'", &.{.{ .string = "!!!" }});
    try testTokens("'foo bar baz'", &.{.{ .string = "foo bar baz" }});
    try testTokens("'foo.bar.baz'", &.{.{ .string = "foo.bar.baz" }});

    try testTokens(
        \\"foo \"bar\" baz"
    , &.{.{ .string = 
    \\foo "bar" baz
    }});

    try testTokens("\"foo \\n bar\"", &.{.{ .string = "foo \n bar" }});
    try testTokens("\"foo \\t bar\"", &.{.{ .string = "foo \t bar" }});

    try testTokens(
        "'This string has a \\b backspace character.'",
        &.{.{ .string = "This string has a \\b backspace character." }},
    );

    try testTokens("\"\"\" foo bar baz \"\"\"", &.{.{ .string = " foo bar baz " }});
    try testTokens("''' foo bar baz '''", &.{.{ .string = " foo bar baz " }});

    try testTokens("\"\"\"\"\"\"\"", &.{.{ .string = "\"" }});
    try testTokens("'''''''", &.{.{ .string = "'" }});
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

    try testTokens("0o147", &.{.{ .integer = 0o147 }});
    try testTokens("0x147abc", &.{.{ .integer = 0x147abc }});
    try testTokens("0b10010011", &.{.{ .integer = 0b10010011 }});

    try testTokens("1_000_000", &.{.{ .integer = 1000000 }});
    try testTokens("-1_000_000", &.{.{ .integer = -1000000 }});
}

test "floats" {
    try testTokens("3.14", &.{.{ .float = 3.14 }});
    try testTokens("0.1", &.{.{ .float = 0.1 }});
    try testTokens("-3.14", &.{.{ .float = -3.14 }});
    try testTokens("-0.1", &.{.{ .float = -0.1 }});
    try testTokens("0e0", &.{.{ .float = 0.0 }});
    try testTokens("-1E-1", &.{.{ .float = -0.1 }});
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
