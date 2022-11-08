const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    var table = try parser.parse(gpa,
        \\[[foo]]
        \\a = 1
        \\[[foo]]
        \\b = 2
    );
    defer table.deinit(gpa);

    const e2e = @import("end_to_end.zig");
    var json = try e2e.tableToJson(gpa, &table);

    var al = std.ArrayList(u8).init(gpa);
    defer al.deinit();

    try json.jsonStringify(.{ .whitespace = .{} }, al.writer());
    std.debug.print("{s}", .{al.items});
}

test "refAllDecls" {
    const end2end = @import("end_to_end.zig");

    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(lexer);
    std.testing.refAllDecls(parser);
    std.testing.refAllDecls(end2end);
}
