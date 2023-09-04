const std = @import("std");
const tomlz = @import("tomlz");

pub fn main() !void {
    // setup a basic allocator
    var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_instance.allocator();
    defer _ = gpa_instance.deinit();

    const stdout_writer = std.io.getStdOut().writer();

    // anything that isn't a table(numbers, strings, etc)
    // need to be written with a key
    try stdout_writer.writeAll("# Simple value:\n");
    try tomlz.serializeKeyValue(gpa, stdout_writer, "truth", 42);

    // serialize a simple struct like this
    const my_point = .{
        .x = 5,
        .y = 5,
    };

    // try switching this to a `tomlz.serializeKeyValue` and see why that's
    // a problem!
    try stdout_writer.writeAll("\n# Table:\n");
    try tomlz.serialize(gpa, stdout_writer, my_point);

    // Finally lets look how we can avoid having to use an allocator
    // Every type has a certain "depth" to it, describing how many of its
    // fields need to be written in table form
    const my_nested_type = .{
        .number1 = 1, // depth 1(default)
        .child = .{ // depth 2, this is a table
            .number2 = 2,
            .child = .{ // depth 3, we're even deeper
                .number3 = 3,
            },
        },
        .otherchild = .{ // depth 2, this is also a table
            .number4 = 4,
        },
    };
    // We can see the highest depth is 3, so that's what we need to prepare for

    try stdout_writer.writeAll("\n# No allocator:\n");
    try tomlz.serializer.serializeFixedDepth(
        3, // specify the depth here
        stdout_writer,
        my_nested_type,
    );

    // In a lot of cases you can just use a "big" number like 64 here and it'll work
    // but we can't make that assumption for you, so the default uses an allocator to
    // allow for arbitrarily deep types.
}
