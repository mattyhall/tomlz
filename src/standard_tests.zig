const std = @import("std");
const testing = std.testing;
const lex = @import("lexer.zig");
const parser = @import("parser.zig");
const e2e = @import("end_to_end.zig");

const failing_invalid_tests = [_][]const u8{
    "string/bad-multiline.toml",
    "array/extending-table.toml",
    "table/rrbrace.toml",
    "table/llbrace.toml",
};

const failing_valid_tests = [_][]const u8{
    "comment/tricky.toml",
    "comment/everywhere.toml",
    "spec-example-1.toml",
    "array/array.toml",
    "implicit-and-explicit-after.toml",
    "example.toml",
    "spec-example-1-compact.toml",
    "float/inf-and-nan.toml",
    "float/float.toml",
    "datetime/milliseconds.toml",
    "datetime/local-date.toml",
    "datetime/timezone.toml",
    "datetime/local-time.toml",
    "datetime/local.toml",
    "datetime/datetime.toml",
};

var dbg = false;

fn jsonValueEquality(actual: *const std.json.Value, expected: *const std.json.Value) bool {
    if (dbg) {
        std.debug.print("====\n", .{});
        actual.dump();
        std.debug.print("\n", .{});
        expected.dump();
        std.debug.print("\n====\n", .{});
    }

    switch (actual.*) {
        .String => |s| return std.mem.eql(u8, s, expected.String),
        .Integer => |i| return i == expected.Integer,
        .Bool => |a| return a == expected.Bool,
        .Float => |f| return switch (expected.*) {
            .Float => |f2| f == f2,
            .Integer => |i| f == @intToFloat(f64, i),
            else => false,
        },
        .Null, .NumberString => return false,
        else => return false,
    }
}

pub fn jsonEquality(gpa: std.mem.Allocator, actual: *const std.json.Value, expected: *const std.json.Value) !bool {
    if (expected.* == .Object and expected.Object.contains("type")) {
        var t = expected.Object.get("type") orelse unreachable;
        var s = expected.Object.get("value") orelse unreachable;

        if (std.mem.eql(u8, t.String, "string")) {
            if (actual.* != .String) return false;

            return std.mem.eql(u8, actual.String, s.String);
        }

        var p = std.json.Parser.init(gpa, false);
        defer p.deinit();

        var tree = p.parse(s.String) catch {
            if (dbg) std.debug.print("could not parse '{s}'", .{s.String});
            return false;
        };
        defer tree.deinit();

        return jsonValueEquality(actual, &tree.root);
    }

    switch (actual.*) {
        .Array => {
            var arr_actual = actual.Array.items;
            var arr_expected = expected.Array.items;
            if (arr_actual.len != arr_expected.len) return false;

            for (arr_actual) |value_a, i| {
                const value_e = arr_expected[i];
                if (dbg) std.debug.print("index: {}\n", .{i});
                if (!try jsonEquality(gpa, &value_a, &value_e)) return false;
            }

            return true;
        },
        .Object => {
            var tbl_a = actual.Object;
            var tbl_e = expected.Object;
            if (tbl_a.count() != tbl_e.count()) {
                if (dbg) std.debug.print("wrong count\n", .{});
                return false;
            }

            var it = tbl_a.iterator();
            while (it.next()) |entry_a| {
                if (dbg) std.debug.print("key: {s}\n", .{entry_a.key_ptr.*});
                const value_e = tbl_e.get(entry_a.key_ptr.*) orelse return false;
                if (!try jsonEquality(gpa, entry_a.value_ptr, &value_e)) return false;
            }

            return true;
        },
        else => {
            if (dbg) std.debug.print("some other type\n", .{});
            return false;
        },
    }
}

fn testFile(dir: *const std.fs.Dir, basename: []const u8) !parser.Table {
    var f = try dir.openFile(basename, .{});
    defer f.close();

    var contents = try f.reader().readAllAlloc(testing.allocator, 5 * 1024 * 1024);
    defer testing.allocator.free(contents);

    return try parser.parse(testing.allocator, contents);
}

fn testInvalid(dir: *const std.fs.Dir, path: []const u8, basename: []const u8) !bool {
    for (failing_invalid_tests) |skip_path| if (std.mem.eql(u8, path, skip_path)) return false;

    var full_path = try dir.realpathAlloc(testing.allocator, basename);
    defer testing.allocator.free(full_path);

    var tbl = testFile(dir, basename) catch return false;
    defer tbl.deinit(testing.allocator);

    std.debug.print("{s} successfully parsed\n", .{full_path});
    return true;
}

fn testValid(dir: *const std.fs.Dir, path: []const u8, basename: []const u8) !bool {
    for (failing_valid_tests) |skip_path| if (std.mem.eql(u8, path, skip_path)) return false;

    var full_path = try dir.realpathAlloc(testing.allocator, basename);
    defer testing.allocator.free(full_path);

    var tbl = testFile(dir, basename) catch |err| {
        std.debug.print("{s} failed to parse {}\n", .{ full_path, err });
        return true;
    };
    defer tbl.deinit(testing.allocator);

    var value = .{ .table = tbl };
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var actual = try e2e.tomlValueToJson(arena.allocator(), &value);

    var json_path = try testing.allocator.dupe(u8, basename);
    defer testing.allocator.free(json_path);
    std.mem.copy(u8, json_path[basename.len - 4 ..], "json");

    var f = try dir.openFile(json_path, .{});
    defer f.close();

    var contents = try f.reader().readAllAlloc(testing.allocator, 5 * 1024 * 1024);
    defer testing.allocator.free(contents);

    var json_parser = std.json.Parser.init(testing.allocator, false);
    defer json_parser.deinit();

    var expected = try json_parser.parse(contents);
    defer expected.deinit();

    if (!try jsonEquality(arena.allocator(), &actual, &expected.root)) {
        std.debug.print("{s}\n", .{full_path});
        return true;
    }

    return false;
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

test "valid" {
    var dir = try std.fs.cwd().makeOpenPathIterable("tests/valid", .{});
    defer dir.close();

    var fail = false;

    var walker = try dir.walk(testing.allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .File) continue;
        if (std.mem.endsWith(u8, entry.basename, "json")) continue;

        fail = fail or try testValid(&entry.dir, entry.path, entry.basename);
    }

    if (fail) return error.ValidDidNotPass;
}
