const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");

test "refAllDecls" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(lexer);
}
