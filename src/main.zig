const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    var l = lexer.Lexer.init(gpa, "foo =");
    while (try l.next()) |tok| {
        std.debug.print("{}", .{tok});
    }
}

test "refAllDecls" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(lexer);
    std.testing.refAllDecls(parser);
}
