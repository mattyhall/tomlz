const std = @import("std");
const tomlz = @import("tomlz");

pub fn main() !void {
    // setup a basic allocator
    var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_instance.allocator();
    defer _ = gpa_instance.deinit();

    // serialize a simple struct like this
    const Point = struct {
        x: usize,
        y: usize,
    };

    const my_point = Point{
        .x = 5,
        .y = 5,
    };

    const stdout_writer = std.io.getStdOut().writer();
    try tomlz.serialize(gpa, stdout_writer, my_point);
}
