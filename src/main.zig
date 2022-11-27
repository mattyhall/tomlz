const std = @import("std");
const testing = std.testing;
pub const lex = @import("lexer.zig");
pub const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const Value = parser.Value;
pub const Array = parser.Array;
pub const Table = parser.Table;

test "refAllDecls" {
    const integration_tests = @import("integration_tests.zig");

    std.testing.refAllDecls(lex);
    std.testing.refAllDecls(parser);
    std.testing.refAllDecls(integration_tests);
}
