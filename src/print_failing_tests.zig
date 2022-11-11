const std = @import("std");
const testing = std.testing;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const e2e = @import("end_to_end.zig");
const standard_tests = @import("standard_tests.zig");

fn testFile(gpa: std.mem.Allocator, dir: *const std.fs.Dir, basename: []const u8) !parser.Table {
    var f = try dir.openFile(basename, .{});
    defer f.close();

    var contents = try f.reader().readAllAlloc(gpa, 5 * 1024 * 1024);
    defer gpa.free(contents);

    return try parser.parse(gpa, contents);
}

fn parseInvalid(gpa: std.mem.Allocator, dir: *const std.fs.Dir, basename: []const u8) !bool {
    var full_path = try dir.realpathAlloc(gpa, basename);
    defer gpa.free(full_path);

    var tbl = testFile(gpa, dir, basename) catch return false;
    defer tbl.deinit(gpa);

    std.debug.print("{s}\n", .{full_path});
    return true;
}

fn parseValid(gpa: std.mem.Allocator, dir: *const std.fs.Dir, basename: []const u8) !bool {
    var full_path = try dir.realpathAlloc(gpa, basename);
    defer gpa.free(full_path);

    var tbl = testFile(gpa, dir, basename) catch {
        std.debug.print("{s}\n", .{full_path});
        return true;
    };
    defer tbl.deinit(gpa);

    var value = .{ .table = tbl };
    var actual = try e2e.tomlValueToJson(gpa, &value);

    var json_path = try gpa.dupe(u8, basename);
    defer gpa.free(json_path);
    std.mem.copy(u8, json_path[basename.len - 4 ..], "json");

    var f = try dir.openFile(json_path, .{});
    defer f.close();

    var contents = try f.reader().readAllAlloc(gpa, 5 * 1024 * 1024);
    defer gpa.free(contents);

    var json_parser = std.json.Parser.init(gpa, false);
    defer json_parser.deinit();

    var expected = try json_parser.parse(contents);
    defer expected.deinit();

    if (!try standard_tests.jsonEquality(gpa, &actual, &expected.root)) {
        std.debug.print("{s}\n", .{full_path});
        return true;
    }

    return false;
}

pub fn main() !void {
    var gpa = std.heap.page_allocator;

    {
        std.debug.print("Invalid\n", .{});
        var dir = try std.fs.cwd().makeOpenPathIterable("tests/invalid", .{});
        defer dir.close();

        var walker = try dir.walk(gpa);
        defer walker.deinit();
        while (try walker.next()) |entry| {
            if (entry.kind != .File) continue;

            _ = try parseInvalid(gpa, &entry.dir, entry.basename);
        }
    }

    {
        std.debug.print("Valid\n", .{});
        var dir = try std.fs.cwd().makeOpenPathIterable("tests/valid", .{});
        defer dir.close();

        var walker = try dir.walk(gpa);
        defer walker.deinit();
        while (try walker.next()) |entry| {
            if (entry.kind != .File) continue;
            if (!std.mem.endsWith(u8, entry.basename, "toml")) continue;

            _ = try parseValid(gpa, &entry.dir, entry.basename);
        }
    }
}
