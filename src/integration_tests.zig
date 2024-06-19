const std = @import("std");
const parser = @import("parser.zig");
const testing = std.testing;

const failing_invalid_tests = [_][]const u8{};

const failing_valid_tests = [_][]const u8{
    "comment/everywhere.toml",
    "spec-example-1.toml",
    "array/array.toml",
    "example.toml",
    "spec-example-1-compact.toml",
    "datetime/milliseconds.toml",
    "datetime/local-date.toml",
    "datetime/timezone.toml",
    "datetime/local-time.toml",
    "datetime/local.toml",
    "datetime/datetime.toml",
};

const dbg = false;

fn jsonValueEquality(actual: *const std.json.Value, expected: *const std.json.Value) bool {
    if (dbg) {
        std.debug.print("====\n", .{});
        actual.dump();
        std.debug.print("\n", .{});
        expected.dump();
        std.debug.print("\n====\n", .{});
    }

    switch (actual.*) {
        .string => |s| return std.mem.eql(u8, s, expected.string),
        .integer => |i| return i == expected.integer,
        .bool => |a| return a == expected.bool,
        .float => |f| return switch (expected.*) {
            .float => |f2| f == f2,
            .integer => |i| f == @as(f64, @floatFromInt(i)),
            else => false,
        },
        .null, .number_string => return false,
        else => return false,
    }
}

pub fn jsonEquality(gpa: std.mem.Allocator, actual: *const std.json.Value, expected: *const std.json.Value) !bool {
    if (expected.* == .object and expected.object.contains("type")) {
        const t = expected.object.get("type") orelse unreachable;
        const s = expected.object.get("value") orelse unreachable;

        if (std.mem.eql(u8, t.string, "string")) {
            if (actual.* != .string) return false;

            return std.mem.eql(u8, actual.string, s.string);
        }

        if (std.mem.eql(u8, t.string, "float")) {
            if (actual.* != .float) return false;

            if (std.mem.eql(u8, s.string, "inf") or (std.mem.eql(u8, s.string, "+inf") or (std.mem.eql(u8, s.string, "-inf"))))
                return std.math.inf(f64) == actual.float;

            if (std.mem.eql(u8, s.string, "nan") or (std.mem.eql(u8, s.string, "+nan") or (std.mem.eql(u8, s.string, "-nan"))))
                return std.math.isNan(actual.float);
        }

        var parsed = std.json.parseFromSlice(std.json.Value, gpa, s.string, .{}) catch {
            if (dbg) std.debug.print("could not parse '{s}'", .{s.string});
            return false;
        };
        defer parsed.deinit();

        return jsonValueEquality(actual, &parsed.value);
    }

    switch (actual.*) {
        .array => {
            const arr_actual = actual.array.items;
            const arr_expected = expected.array.items;
            if (arr_actual.len != arr_expected.len) return false;

            for (arr_actual, 0..) |value_a, i| {
                const value_e = arr_expected[i];
                if (dbg) std.debug.print("index: {}\n", .{i});
                if (!try jsonEquality(gpa, &value_a, &value_e)) return false;
            }

            return true;
        },
        .object => {
            var tbl_a = actual.object;
            var tbl_e = expected.object;
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

pub fn tomlValueToJson(allocator: std.mem.Allocator, v: *parser.Value) !std.json.Value {
    return switch (v.*) {
        .string => |s| std.json.Value{ .string = s },
        .integer => |s| std.json.Value{ .integer = s },
        .float => |f| std.json.Value{ .float = f },
        .boolean => |b| std.json.Value{ .bool = b },
        .array => |*a| b: {
            var al = try std.json.Array.initCapacity(allocator, a.array.items.len);
            for (a.array.items) |*value| {
                al.appendAssumeCapacity(try tomlValueToJson(allocator, value));
            }
            break :b std.json.Value{ .array = al };
        },
        .table => |*t| try tableToJson(allocator, t),
    };
}

pub fn tableToJson(allocator: std.mem.Allocator, table: *parser.Table) error{OutOfMemory}!std.json.Value {
    var obj = std.json.ObjectMap.init(allocator);
    errdefer obj.deinit();

    var it = table.table.iterator();
    while (it.next()) |entry| {
        const v = try tomlValueToJson(allocator, entry.value_ptr);
        try obj.put(entry.key_ptr.*, v);
    }

    return std.json.Value{ .object = obj };
}

fn expectParseEqualToJson(src: []const u8, json: []const u8) !void {
    var table = try parser.parse(testing.allocator, src);
    defer table.deinit(testing.allocator);

    var actual_al = std.ArrayList(u8).init(testing.allocator);
    defer actual_al.deinit();

    var json_writer = std.json.writeStreamArbitraryDepth(
        testing.allocator,
        actual_al.writer(),
        .{ .whitespace = .indent_4 },
    );
    defer json_writer.deinit();

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var actual_json = try tableToJson(arena.allocator(), &table);
    try actual_json.jsonStringify(&json_writer);

    try testing.expectEqualStrings(json, actual_al.items);
}

fn testFile(dir: *const std.fs.Dir, basename: []const u8) !parser.Table {
    var f = try dir.openFile(basename, .{});
    defer f.close();

    const contents = try f.reader().readAllAlloc(testing.allocator, 5 * 1024 * 1024);
    defer testing.allocator.free(contents);

    return try parser.parse(testing.allocator, contents);
}

fn testInvalid(dir: *const std.fs.Dir, path: []const u8, basename: []const u8) !bool {
    for (failing_invalid_tests) |skip_path| if (std.mem.eql(u8, path, skip_path)) return false;

    const full_path = try dir.realpathAlloc(testing.allocator, basename);
    defer testing.allocator.free(full_path);

    var tbl = testFile(dir, basename) catch return false;
    defer tbl.deinit(testing.allocator);

    std.debug.print("{s} successfully parsed\n", .{full_path});
    return true;
}

fn testValid(dir: *const std.fs.Dir, path: []const u8, basename: []const u8) !bool {
    for (failing_valid_tests) |skip_path| if (std.mem.eql(u8, path, skip_path)) return false;

    const full_path = try dir.realpathAlloc(testing.allocator, basename);
    defer testing.allocator.free(full_path);

    var tbl = testFile(dir, basename) catch |err| {
        std.debug.print("{s} failed to parse {}\n", .{ full_path, err });
        return true;
    };
    defer tbl.deinit(testing.allocator);

    var value = .{ .table = tbl };
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var actual = try tomlValueToJson(arena.allocator(), &value);

    var json_path = try testing.allocator.dupe(u8, basename);
    defer testing.allocator.free(json_path);
    std.mem.copyForwards(u8, json_path[basename.len - 4 ..], "json");

    var f = try dir.openFile(json_path, .{});
    defer f.close();

    const contents = try f.reader().readAllAlloc(testing.allocator, 5 * 1024 * 1024);
    defer testing.allocator.free(contents);

    var expected = try std.json.parseFromSlice(std.json.Value, testing.allocator, contents, .{});
    defer expected.deinit();

    if (!try jsonEquality(arena.allocator(), &actual, &expected.value)) {
        std.debug.print("{s}\n", .{full_path});
        return true;
    }

    return false;
}

// standard tests

test "invalid" {
    var dir = try std.fs.cwd().makeOpenPath("tests/invalid", .{.iterate = true});
    defer dir.close();

    var fail = false;

    var walker = try dir.walk(testing.allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;

        fail = fail or try testInvalid(&entry.dir, entry.path, entry.basename);
    }

    if (fail) return error.InvalidDidNotFail;
}

test "valid" {
    var dir = try std.fs.cwd().makeOpenPath("tests/valid", .{.iterate = true});
    defer dir.close();

    var fail = false;

    var walker = try dir.walk(testing.allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        if (std.mem.endsWith(u8, entry.basename, "json")) continue;

        fail = fail or try testValid(&entry.dir, entry.path, entry.basename);
    }

    if (fail) return error.ValidDidNotPass;
}

// fuzz error case tests

test "fuzz" {
    var dir = try std.fs.cwd().makeOpenPath("tests/fuzzing", .{.iterate = true});
    defer dir.close();

    var walker = try dir.walk(testing.allocator);
    defer walker.deinit();
    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;

        const full_path = try entry.dir.realpathAlloc(testing.allocator, entry.basename);
        defer testing.allocator.free(full_path);

        var f = try entry.dir.openFile(full_path, .{});
        defer f.close();

        const contents = try f.reader().readAllAlloc(testing.allocator, 5 * 1024 * 1024);
        defer testing.allocator.free(contents);

        // We just want to make sure we don't crash when parsing these
        var tbl = parser.parse(testing.allocator, contents) catch continue;
        tbl.deinit(testing.allocator);
    }
}

// decode tests

test "decode simple" {
    const S = struct {
        b: bool,
        i1: i32,
        i2: u8,
        f1: f32,
        f2: f64,
    };

    const s = try parser.decode(S, testing.allocator,
        \\b = false
        \\i1 = 147
        \\i2 = 14
        \\f1 = 14.7
        \\f2 = 14.7
    );

    try testing.expectEqual(S{ .b = false, .i1 = 147, .i2 = 14, .f1 = 14.7, .f2 = 14.7 }, s);
}

test "decode optional" {
    const S = struct { a: i64, b: ?bool };

    const s = try parser.decode(S, testing.allocator, "a = 147");

    try testing.expectEqual(S{ .a = 147, .b = null }, s);
}

test "decode array of ints" {
    const S = struct {
        vals: []const i64,
    };

    const s = try parser.decode(S, testing.allocator, "vals = [1, 2, 3, 4, 5]");
    defer testing.allocator.free(s.vals);

    try testing.expectEqualSlices(i64, &.{ 1, 2, 3, 4, 5 }, s.vals);
}

test "decode array of strings" {
    const S = struct {
        vals: []const []const u8,
    };

    const s = try parser.decode(S, testing.allocator,
        \\vals = ["hello", ", ", "world"]
    );
    defer {
        for (s.vals) |str| testing.allocator.free(str);
        testing.allocator.free(s.vals);
    }

    try testing.expectEqual(@as(usize, 3), s.vals.len);
    try testing.expectEqualStrings("hello", s.vals[0]);
    try testing.expectEqualStrings(", ", s.vals[1]);
    try testing.expectEqualStrings("world", s.vals[2]);
}

test "decode array of tables" {
    const B = struct { a: i64 };
    const F = struct { bar: []const B };
    const S = struct { foo: F };

    const s = try parser.decode(S, testing.allocator,
        \\[[foo.bar]]
        \\a = 147
        \\[[foo.bar]]
        \\a = 1
    );
    defer {
        testing.allocator.free(s.foo.bar);
    }

    try testing.expectEqualSlices(B, &.{ .{ .a = 147 }, .{ .a = 1 } }, s.foo.bar);
}

test "decode default value" {
    const S = struct {
        a: []const u8 = "hello world",
        b: i32 = 147,
        c: bool = false,
    };

    const s = try parser.decode(S, testing.allocator,
        \\c = true
    );

    try testing.expectEqualStrings("hello world", s.a);
    try testing.expectEqual(@as(i32, 147), s.b);
    try testing.expect(s.c);
}

// toml2json tests

test "snooker" {
    try expectParseEqualToJson(
        \\name = "snooker"
        \\
        \\[goat]
        \\name = "Ronnie o' Sullivan"
        \\age = 46 # as of Nov 2022
        \\hobbies = ["running", "hustling at pool"]
        \\
        \\[goat.triple-crowns]
        \\worlds = 7
        \\masters = 7
        \\uks = 7
    ,
        \\{
        \\    "name": "snooker",
        \\    "goat": {
        \\        "triple-crowns": {
        \\            "uks": 7,
        \\            "masters": 7,
        \\            "worlds": 7
        \\        },
        \\        "name": "Ronnie o' Sullivan",
        \\        "age": 46,
        \\        "hobbies": [
        \\            "running",
        \\            "hustling at pool"
        \\        ]
        \\    }
        \\}
    );
}

test "decode snooker" {
    const TripleCrowns = struct { worlds: i64, masters: i64, uks: i64 };

    const Player = struct {
        name: []const u8,
        age: i64,
        hobbies: []const []const u8,
        triplecrowns: TripleCrowns,

        const Self = @This();

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            gpa.free(self.name);

            for (self.hobbies) |hobby| {
                gpa.free(hobby);
            }
            gpa.free(self.hobbies);
        }
    };

    const Game = struct {
        name: []const u8,
        goat: Player,

        const Self = @This();

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            gpa.free(self.name);
            self.goat.deinit(gpa);
        }
    };

    var s = try parser.decode(Game, testing.allocator,
        \\name = "snooker"
        \\
        \\[goat]
        \\name = "Ronnie o' Sullivan"
        \\age = 46 # as of Nov 2022
        \\hobbies = ["running", "hustling at pool"]
        \\
        \\[goat.triplecrowns]
        \\worlds = 7
        \\masters = 7
        \\uks = 7
    );
    defer s.deinit(testing.allocator);

    try testing.expectEqualStrings("snooker", s.name);

    try testing.expectEqualStrings("Ronnie o' Sullivan", s.goat.name);
    try testing.expectEqual(@as(i64, 46), s.goat.age);
    try testing.expectEqual(@as(usize, 2), s.goat.hobbies.len);
    try testing.expectEqualStrings("running", s.goat.hobbies[0]);
    try testing.expectEqualStrings("hustling at pool", s.goat.hobbies[1]);

    try testing.expectEqual(@as(i64, 7), s.goat.triplecrowns.worlds);
    try testing.expectEqual(@as(i64, 7), s.goat.triplecrowns.masters);
    try testing.expectEqual(@as(i64, 7), s.goat.triplecrowns.uks);
}

test "helix config" {
    try expectParseEqualToJson(
        \\theme = "ayu_dark"
        \\
        \\[editor]
        \\line-number = "relative"
        \\rulers = [80, 120]
        \\scrolloff = 0
        \\true-color = true
        \\
        \\[editor.cursor-shape]
        \\insert = "bar"
        \\normal = "block"
        \\select = "underline"
        \\
        \\[editor.statusline]
        \\center = ["file-name"]
        \\left = ["mode", "spinner"]
        \\right = ["diagnostics", "selections", "position", "position-percentage", "file-encoding", "file-type"]
        \\separator = "│"
        \\
        \\[keys.normal]
        \\"," = "collapse_selection"
        \\";" = "keep_primary_selection"
        \\A-J = "join_selections"
        \\A-K = "remove_selections"
        \\A-k = "keep_selections"
        \\B = "extend_prev_word_start"
        \\E = "extend_next_word_end"
        \\H = "extend_char_left"
        \\J = "extend_line_down"
        \\K = "extend_line_up"
        \\L = "extend_char_right"
        \\N = "extend_search_next"
        \\W = "extend_next_word_start"
        \\X = "extend_line_below"
        \\
        \\[keys.normal.space]
        \\"," = "buffer_picker"
        \\space = "file_picker"
        \\
        \\[keys.normal.space.c]
        \\D = "workspace_diagnostics_picker"
        \\R = "rename_symbol"
        \\S = "workspace_symbol_picker"
        \\a = "code_action"
        \\d = "diagnostics_picker"
        \\s = "symbol_picker"
    ,
        \\{
        \\    "keys": {
        \\        "normal": {
        \\            "A-K": "remove_selections",
        \\            "space": {
        \\                "c": {
        \\                    "a": "code_action",
        \\                    "R": "rename_symbol",
        \\                    "S": "workspace_symbol_picker",
        \\                    "s": "symbol_picker",
        \\                    "D": "workspace_diagnostics_picker",
        \\                    "d": "diagnostics_picker"
        \\                },
        \\                "space": "file_picker",
        \\                ",": "buffer_picker"
        \\            },
        \\            "K": "extend_line_up",
        \\            ";": "keep_primary_selection",
        \\            "H": "extend_char_left",
        \\            ",": "collapse_selection",
        \\            "A-k": "keep_selections",
        \\            "B": "extend_prev_word_start",
        \\            "W": "extend_next_word_start",
        \\            "X": "extend_line_below",
        \\            "L": "extend_char_right",
        \\            "J": "extend_line_down",
        \\            "N": "extend_search_next",
        \\            "E": "extend_next_word_end",
        \\            "A-J": "join_selections"
        \\        }
        \\    },
        \\    "theme": "ayu_dark",
        \\    "editor": {
        \\        "line-number": "relative",
        \\        "true-color": true,
        \\        "statusline": {
        \\            "right": [
        \\                "diagnostics",
        \\                "selections",
        \\                "position",
        \\                "position-percentage",
        \\                "file-encoding",
        \\                "file-type"
        \\            ],
        \\            "left": [
        \\                "mode",
        \\                "spinner"
        \\            ],
        \\            "separator": "│",
        \\            "center": [
        \\                "file-name"
        \\            ]
        \\        },
        \\        "rulers": [
        \\            80,
        \\            120
        \\        ],
        \\        "cursor-shape": {
        \\            "select": "underline",
        \\            "insert": "bar",
        \\            "normal": "block"
        \\        },
        \\        "scrolloff": 0
        \\    }
        \\}
    );
}

test "cargo" {
    try expectParseEqualToJson(
        \\[package]
        \\
        \\name = "tiled"
        \\version = "0.9.3"
        \\description = "A rust crate for loading in maps created by the Tiled editor"
        \\repository = "https://github.com/mattyhall/rs-tiled.git"
        \\# documentation = "http://rust-ci.org/mattyhall/rs-tiled/doc/tiled/"
        \\readme = "README.md"
        \\license = "MIT"
        \\authors = ["Matthew Hall <matthew@quickbeam.me.uk>"]
        \\edition = "2018"
        \\
        \\keywords = ["tiled", "tmx", "map"]
        \\
        \\[features]
        \\default = ["zstd"]
        \\
        \\[lib]
        \\name = "tiled"
        \\path = "src/lib.rs"
        \\
        \\[[example]]
        \\name = "example"
        \\path = "examples/main1.rs"
        \\
        \\[[example]]
        \\name = "example"
        \\path = "examples/main2.rs"
        \\
        \\[dependencies]
        \\base64  = "0.10"
        \\xml-rs  = "0.8"
        \\libflate = "0.1.18"
        \\zstd = { version = "0.5", optional = true }
    ,
        \\{
        \\    "example": [
        \\        {
        \\            "path": "examples/main1.rs",
        \\            "name": "example"
        \\        },
        \\        {
        \\            "path": "examples/main2.rs",
        \\            "name": "example"
        \\        }
        \\    ],
        \\    "dependencies": {
        \\        "libflate": "0.1.18",
        \\        "xml-rs": "0.8",
        \\        "zstd": {
        \\            "version": "0.5",
        \\            "optional": true
        \\        },
        \\        "base64": "0.10"
        \\    },
        \\    "package": {
        \\        "repository": "https://github.com/mattyhall/rs-tiled.git",
        \\        "version": "0.9.3",
        \\        "license": "MIT",
        \\        "keywords": [
        \\            "tiled",
        \\            "tmx",
        \\            "map"
        \\        ],
        \\        "authors": [
        \\            "Matthew Hall <matthew@quickbeam.me.uk>"
        \\        ],
        \\        "description": "A rust crate for loading in maps created by the Tiled editor",
        \\        "name": "tiled",
        \\        "edition": "2018",
        \\        "readme": "README.md"
        \\    },
        \\    "lib": {
        \\        "path": "src/lib.rs",
        \\        "name": "tiled"
        \\    },
        \\    "features": {
        \\        "default": [
        \\            "zstd"
        \\        ]
        \\    }
        \\}
    );
}

test "fruits" {
    try expectParseEqualToJson(
        \\[[fruits]]
        \\name = "apple"
        \\
        \\[fruits.physical]  # subtable
        \\color = "red"
        \\shape = "round"
        \\
        \\[[fruits.varieties]]  # nested array of tables
        \\name = "red delicious"
        \\
        \\[fruits.varieties.rating]
        \\yumminess = 5
        \\appearance = 6
        \\
        \\[[fruits.varieties]]
        \\name = "granny smith"
        \\
        \\
        \\[[fruits]]
        \\name = "banana"
        \\
        \\[[fruits.varieties]]
        \\name = "plantain"
    ,
        \\{
        \\    "fruits": [
        \\        {
        \\            "physical": {
        \\                "color": "red",
        \\                "shape": "round"
        \\            },
        \\            "name": "apple",
        \\            "varieties": [
        \\                {
        \\                    "rating": {
        \\                        "appearance": 6,
        \\                        "yumminess": 5
        \\                    },
        \\                    "name": "red delicious"
        \\                },
        \\                {
        \\                    "name": "granny smith"
        \\                }
        \\            ]
        \\        },
        \\        {
        \\            "name": "banana",
        \\            "varieties": [
        \\                {
        \\                    "name": "plantain"
        \\                }
        \\            ]
        \\        }
        \\    ]
        \\}
    );
}
