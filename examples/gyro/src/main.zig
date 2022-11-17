const std = @import("std");
const tomlz = @import("tomlz");

pub fn main() !void {
    const toml =
        \\[goat]
        \\name = "Ronnie O' Sullivan"
        \\age = 46
        \\world_titles = [2001, 2004, 2008, 2012, 2013, 2020, 2022]
    ;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var table = try tomlz.parse(gpa.allocator(), toml);
    defer table.deinit(gpa.allocator());

    const goat = table.getTable("goat").?;
    std.debug.print("GOAT: {s} (age {})\n", .{ goat.getString("name").?, goat.getInteger("age").? });
    std.debug.print("Number of world titles: {}\n", .{goat.getArray("world_titles").?.items().len});
}
