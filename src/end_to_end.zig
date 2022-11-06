const std = @import("std");
const parser = @import("parser.zig");
const testing = std.testing;

fn tableToJson(allocator: std.mem.Allocator, table: *parser.Table) !std.json.Value {
    var obj = std.json.ObjectMap.init(allocator);
    errdefer obj.deinit();

    var it = table.table.iterator();
    while (it.next()) |entry| {
        const v = switch (entry.value_ptr.*) {
            .string => |s| std.json.Value{ .String = s },
            .integer => |s| std.json.Value{ .Integer = s },
            .boolean => |b| std.json.Value{ .Bool = b },
            .table => |*t| try tableToJson(allocator, t),
        };
        try obj.put(entry.key_ptr.*, v);
    }

    return std.json.Value{ .Object = obj };
}

fn expectParseEqualToJson(src: []const u8, json: []const u8) !void {
    var table = try parser.parse(testing.allocator, src);
    defer table.deinit(testing.allocator);

    var actual_al = std.ArrayList(u8).init(testing.allocator);
    defer actual_al.deinit();

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var actual_json = try tableToJson(arena.allocator(), &table);
    try actual_json.jsonStringify(.{ .whitespace = .{} }, actual_al.writer());

    try testing.expectEqualStrings(json, actual_al.items);
}

test "snooker" {
    try expectParseEqualToJson(
        \\name = "snooker"
        \\
        \\[goat]
        \\name = "Ronnie o' Sullivan"
        \\age = 42 # as of Nov 2022
    ,
        \\{
        \\    "name": "snooker",
        \\    "goat": {
        \\        "name": "Ronnie o' Sullivan",
        \\        "age": 42
        \\    }
        \\}
    );
}
