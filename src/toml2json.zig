const std = @import("std");
const testing = std.testing;
const lex = @import("lexer.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    if (std.os.argv.len != 2) {
        std.debug.print("Please pass a TOML file as the second argument", .{});
    }

    var gpa = std.heap.page_allocator;

    var f = try std.fs.openFileAbsoluteZ(std.os.argv[1], .{});
    defer f.close();

    var contents = try f.reader().readAllAlloc(gpa, 5 * 1024 * 1024);
    defer gpa.free(contents);

    var lexer = parser.Lexer{ .real = try lex.Lexer.init(gpa, contents) };
    var p = try parser.Parser.init(gpa, lexer);
    defer p.deinit();

    var table = p.parse() catch |err| {
        std.debug.print("error parsing {s}: {}\n", .{ std.os.argv[1], err });
        std.debug.print("{?}\n", .{p.diag});
        return err;
    };
    defer table.deinit(gpa);

    const integration = @import("integration_tests.zig");
    var json = try integration.tableToJson(gpa, &table);

    var al = std.ArrayList(u8).init(gpa);
    defer al.deinit();

    try json.jsonStringify(.{ .whitespace = .{} }, al.writer());
    std.debug.print("{s}", .{al.items});
}
