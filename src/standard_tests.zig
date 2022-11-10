const std = @import("std");
const testing = std.testing;
const lex = @import("lexer.zig");
const parser = @import("parser.zig");

const failing_invalid_tests = [_][]const u8{
    "integer/negative-bin.toml",
    "integer/negative-hex.toml",
    "integer/incomplete-bin.toml",
    "integer/positive-hex.toml",
    "integer/incomplete-hex.toml",
    "integer/positive-bin.toml",
    "integer/incomplete-oct.toml",
    "string/bad-multiline.toml",
    "array/extending-table.toml",
    "encoding/bad-utf8-in-comment.toml",
    "encoding/bad-utf8-in-string.toml",
    "inline-table/add.toml",
    "control/string-del.toml",
    "control/string-null.toml",
    "control/string-us.toml",
    "control/string-lf.toml",
    "control/string-bs.toml",
    "table/rrbrace.toml",
    "table/llbrace.toml",
};

fn testInvalid(dir: *const std.fs.Dir, path: []const u8, basename: []const u8) !bool {
    for (failing_invalid_tests) |skip_path| if (std.mem.eql(u8, path, skip_path)) return false;

    var f = try dir.openFile(basename, .{});
    defer f.close();

    var full_path = try dir.realpathAlloc(testing.allocator, basename);
    defer testing.allocator.free(full_path);

    var contents = try f.reader().readAllAlloc(testing.allocator, 5 * 1024 * 1024);
    defer testing.allocator.free(contents);

    var tbl = parser.parse(testing.allocator, contents) catch return false;
    defer tbl.deinit(testing.allocator);

    std.debug.print("{s} successfully parsed\n", .{full_path});
    return true;
}

test "invalid" {
    var dir = try std.fs.cwd().makeOpenPathIterable("tests/invalid", .{});
    defer dir.close();

    var fail = false;

    var walker = try dir.walk(testing.allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .File) continue;

        fail = fail or try testInvalid(&entry.dir, entry.path, entry.basename);
    }

    if (fail) return error.InvalidDidNotFail;
}
