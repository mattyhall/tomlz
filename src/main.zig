const std = @import("std");
const testing = std.testing;
pub const lex = @import("lexer.zig");
pub const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const Value = parser.Value;
pub const Array = parser.Array;
pub const Table = parser.Table;

test "refAllDecls" {
    const end2end = @import("end_to_end.zig");
    const standard_tests = @import("standard_tests.zig");

    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(lex);
    std.testing.refAllDecls(parser);
    std.testing.refAllDecls(end2end);
    std.testing.refAllDecls(standard_tests);
}
