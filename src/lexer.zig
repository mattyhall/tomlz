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

const Quotation = enum { single, double };

/// Lexer splits its given source TOML file into tokens
pub const Lexer = struct {
    arena: std.heap.ArenaAllocator,
    source: []const u8,
    loc: Loc,
    index: usize,
    diag: ?Diagnostic,

    pub const Error = error{ eof, unexpected_char, OutOfMemory, string_not_ended, invalid_codepoint };

    pub fn init(allocator: std.mem.Allocator, src: []const u8) !Lexer {
        if (!std.unicode.utf8ValidateSlice(src))
            return error.not_utf8;

        var arena = std.heap.ArenaAllocator.init(allocator);
        return Lexer{ .arena = arena, .source = src, .loc = .{ .line = 1, .col = 1 }, .diag = null, .index = 0 };
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
                if (i == @intCast(usize, len) + 1) {
                    const written = std.unicode.utf8Encode(codepoint, &buf) catch return error.invalid_codepoint;
                    try al.appendSlice(self.arena.allocator(), buf[0..written]);
                    return;
                }

                return error.unexpected_char;
            };

            _ = self.pop() catch unreachable;

            if (std.math.maxInt(u21) < 16 * @intCast(u64, codepoint) + n) return error.invalid_codepoint;

            codepoint = codepoint * 16 + n;
            if (i == len) {
                const written = std.unicode.utf8Encode(codepoint, &buf) catch return error.invalid_codepoint;
                try al.appendSlice(self.arena.allocator(), buf[0..written]);
                return;
            }
        }
    }

    /// parseEscapeChar parses the valid escape codes allowed in TOML (i.e. those allowed in a string beginning with a
    /// backslash)
    fn parseEscapeChar(self: *Lexer, al: *std.ArrayListUnmanaged(u8)) Error!void {
        var c: u8 = switch (try self.peek()) {
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
                return error.unexpected_char;
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
                error.eof => return error.string_not_ended,
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

                    _ = self.pop() catch unreachable;
                    _ = self.pop() catch unreachable;

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

                    _ = self.pop() catch unreachable;
                    _ = self.pop() catch unreachable;

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
                        if (self.pop() catch unreachable != '\n') return error.unexpected_char;

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
                        return error.unexpected_char;
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
                    return error.unexpected_char;
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
                error.eof => return error.string_not_ended,
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
                        return error.unexpected_char;
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
            var c = try self.peek();
            if (c == '\r') {
                _ = self.pop() catch unreachable;
                c = try self.peek();
                if (c != '\n') {
                    self.diag = .{
                        .loc = self.loc,
                        .msg = "\\r can only appear at the end of a comment",
                    };
                    return error.unexpected_char;
                }
            }

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

            if (c == '\r') {
                _ = self.pop() catch unreachable;
                var p = self.peek() catch |err| switch (err) {
                    error.eof => {
                        self.diag = .{ .msg = "expected \\n after \\r", .loc = self.loc };
                        return error.unexpected_char;
                    },
                    else => return err,
                };

                if (p != '\n') {
                    self.diag = .{ .msg = "expected \\n after \\r", .loc = self.loc };
                    return error.unexpected_char;
                }

                return;
            }

            if (c == '\n' or !std.ascii.isSpace(c)) return;

            _ = self.pop() catch unreachable;
        }
    }

    fn parseExponent(self: *Lexer, base: f64) Error!TokLoc {
        var pow: usize = 0;
        var had_number = false;
        var sign: f64 = 1.0;

        var c = self.peek() catch {
            self.diag = .{ .msg = "expected number after e", .loc = self.loc };
            return error.unexpected_char;
        };
        switch (c) {
            '+' => _ = self.pop() catch unreachable,
            '-' => {
                _ = self.pop() catch unreachable;
                sign = -1.0;
            },
            else => {},
        }

        while (true) {
            c = self.peek() catch |err| switch (err) {
                error.eof => break,
                else => return err,
            };

            if (c == '_') {
                if (!had_number) break;

                _ = self.pop() catch unreachable;
                continue;
            }

            if (c < '0' or c > '9') break;

            _ = self.pop() catch unreachable;
            had_number = true;
            pow = pow * 10 + (std.fmt.parseInt(usize, &.{c}, 10) catch unreachable);
        }

        if (!had_number) {
            self.diag = .{ .msg = "expected number after e", .loc = self.loc };
            return error.unexpected_char;
        }

        return TokLoc{
            .tok = .{ .float = base * std.math.pow(f64, 10, sign * @intToFloat(f64, pow)) },
            .loc = self.loc,
        };
    }

    fn parseFloat(self: *Lexer, loc: Loc, sign: i64, base: u8, whole: i64) Error!TokLoc {
        if (base != 10) {
            self.diag = .{ .msg = "only decimal numbers can be floats", .loc = loc };
            return error.unexpected_char;
        }

        var fractional: f64 = 0.0;
        var i: f64 = 1.0;
        var new_loc = loc;
        const s = @intToFloat(f64, sign);
        const n = @intToFloat(f64, whole);
        var had_number = false;

        while (true) {
            const c = self.peek() catch |err| switch (err) {
                error.eof => {
                    if (had_number) return TokLoc{ .tok = .{ .float = n + s * fractional }, .loc = new_loc };

                    self.diag = .{ .msg = "expected number after dot", .loc = new_loc };
                    return error.unexpected_char;
                },
                else => return err,
            };

            if (had_number and (c == 'e' or c == 'E')) {
                _ = self.pop() catch unreachable;
                return try self.parseExponent(n + s * fractional);
            }

            if (c == '_') {
                if (!had_number) {
                    self.diag = .{ .msg = "expected number after dot", .loc = new_loc };
                    return error.unexpected_char;
                }

                _ = self.pop() catch unreachable;
                continue;
            }

            if (c < '0' or c > '9') {
                if (had_number) return TokLoc{ .tok = .{ .float = n + s * fractional }, .loc = new_loc };

                self.diag = .{ .msg = "expected number after dot", .loc = new_loc };
                return error.unexpected_char;
            }

            _ = try self.pop();
            had_number = true;

            var digit = std.fmt.parseFloat(f64, &.{c}) catch {
                self.diag = .{ .msg = "expected number", .loc = new_loc };
                return error.unexpected_char;
            };
            fractional += digit * std.math.pow(f64, 10.0, -i);
            i += 1.0;
        }
    }

    pub fn parseNumber(self: *Lexer) Error!TokLoc {
        var explicit_sign = false;
        var sign: i64 = 1;
        var base: u8 = 10;
        switch (self.peek() catch unreachable) {
            '-' => {
                explicit_sign = true;
                sign = -1;
                _ = self.pop() catch unreachable;
            },
            '+' => {
                explicit_sign = true;
                _ = self.pop() catch unreachable;
            },
            else => {},
        }

        var loc = self.loc;
        var c = try self.pop();
        var had_number = false;
        if (c == '0') {
            had_number = true;
            const radix = self.peek() catch |err| switch (err) {
                error.eof => return TokLoc{ .tok = .{ .integer = 0 }, .loc = loc },
                else => return err,
            };
            switch (radix) {
                'b' => base = 2,
                'o' => base = 8,
                'e', 'E' => {
                    _ = self.pop() catch unreachable;
                    return try self.parseExponent(0.0);
                },
                'x' => base = 16,
                '.' => {
                    _ = try self.pop();
                    return try self.parseFloat(loc, sign, base, 0);
                },
                else => {
                    if (std.ascii.isWhitespace(radix)) return TokLoc{ .tok = .{ .integer = 0 }, .loc = self.loc };

                    self.diag = .{ .msg = "expected 'b', 'o' or 'x' after '0'", .loc = self.loc };
                    return error.unexpected_char;
                },
            }

            if (explicit_sign and radix != 10) {
                self.diag = .{ .msg = "only base 10 numbers can have an explicit sign", .loc = self.loc };
                return error.unexpected_char;
            }

            _ = self.pop() catch unreachable;

            c = try self.pop();
        }

        var i: i64 = 0;
        var last_c: ?u8 = null;
        while (true) {
            if (c == '.') {
                if (last_c == @as(u8, '_')) {
                    self.diag = .{ .msg = "dot after underscore not allowed", .loc = loc };
                    return error.unexpected_char;
                }

                if (!had_number) {
                    self.diag = .{ .msg = "expected number (leading dot not allowed)", .loc = loc };
                    return error.unexpected_char;
                }

                return try self.parseFloat(loc, sign, base, i);
            }

            if ((c == 'e' or c == 'E') and base != 16) return try self.parseExponent(@intToFloat(f64, i));

            if (c == '_') {
                if (last_c == @as(u8, '_')) {
                    self.diag = .{ .msg = "double underscores not allowed", .loc = loc };
                    return error.unexpected_char;
                }

                if (last_c == null) {
                    self.diag = .{ .msg = "underscore not allowed after base", .loc = loc };
                    return error.unexpected_char;
                }
            } else {
                had_number = true;

                var digit = std.fmt.parseInt(i64, &.{c}, base) catch {
                    self.diag = .{ .msg = "expected number", .loc = loc };
                    return error.unexpected_char;
                };

                i = i * base + sign * digit;
            }

            loc = self.loc;
            last_c = c;
            c = self.peek() catch |err| switch (err) {
                error.eof => {
                    if (last_c != @as(u8, '_')) return TokLoc{ .tok = .{ .integer = i }, .loc = loc };

                    self.diag = .{ .msg = "trailing underscores not allowed", .loc = loc };
                    return error.unexpected_char;
                },
                else => return err,
            };

            if (!std.ascii.isAlphanumeric(c) and c != '.' and c != '_') {
                if (last_c != @as(u8, '_')) return TokLoc{ .tok = .{ .integer = i }, .loc = loc };

                self.diag = .{ .msg = "trailing underscores not allowed", .loc = loc };
                return error.unexpected_char;
            }

            _ = self.pop() catch unreachable;
        }
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

    fn consume(self: *Lexer, tok: Tok, loc: Loc) TokLoc {
        _ = self.pop() catch unreachable;
        return .{ .tok = tok, .loc = loc };
    }

    /// next gives the next token, or null if there are none left. Force key ensures that numbers/keywords are parsed as
    /// keys rather than their normal type. This is needed as TOML allows these values to be used as keys in assignments.
    ///
    /// NOTE: any memory returned in TokLoc (e.g. the []const u8 array in a key/string) is only valid until the next
    /// call of next
    pub fn next(self: *Lexer, force_key: bool) Error!?TokLoc {
        var child = self.arena.child_allocator;

        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(child);

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
            '\r' => {
                _ = self.pop() catch unreachable;

                if (try self.peek() == '\n') return self.consume(.newline, loc);

                self.diag = .{ .msg = "expect a \\n after a \\r", .loc = loc };
                return error.unexpected_char;
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

    return al.items;
}

fn testTokens(src: []const u8, expected: []const Tok) !void {
    var toks = try readAllTokens(src);
    defer {
        for (toks) |tok| {
            tok.deinit(testing.allocator);
        }
        defer testing.allocator.free(toks);
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
