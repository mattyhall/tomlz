const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

fn parseInvalid(gpa: std.mem.Allocator, dir: *const std.fs.Dir, path: []const u8) !void {
    var f = try dir.openFile(path, .{});
    defer f.close();
    
    var full_path = try dir.realpathAlloc(gpa, path);
    defer gpa.free(full_path);
    
    var contents = try f.reader().readAllAlloc(gpa, 5 * 1024 * 1024);
    defer gpa.free(contents);
    
    var tbl = parser.parse(gpa, contents) catch return;
    defer tbl.deinit(gpa);
    
    std.debug.print("{s}\n", .{full_path});
}

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    
    var dir = try std.fs.cwd().makeOpenPathIterable("tests/invalid", .{});
    defer dir.close();
    
    var walker = try dir.walk(gpa);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .File) continue;
        
        try parseInvalid(gpa, &entry.dir, entry.basename);
    }
}
