const std = @import("std");
const tomlz = @import("tomlz");

pub fn main() !void {
    // setup a basic allocator
    var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_instance.allocator();
    defer _ = gpa_instance.deinit();

    const stdout_writer = std.io.getStdOut().writer();

    // structs and unions can have their default serialize function overwritten like this
    // see `tomlz.serializer.Writer` for documentation on the writer
    const MyCustomStruct = struct {
        my_fancy_number: usize,
        string1: []const u8,
        string2: []const u8,

        pub fn tomlzSerialize(self: *const @This(), writer: anytype) !void {
            // When writing a struct, you always need to do this.
            try writer.beginTable();

            try writer.writeKeyValue("number", self.my_fancy_number); // can be used to rename fields for example

            // If you want to e.g. stitch two string together you'd normally need an allocator,
            // but sadly we don't have one. Instead use the underlying stream to do so.
            // You have to be VERY cautious with this, since it could easily produce an invalid
            // output.

            // You also need to push a key where this can be written to
            try writer.pushKey("string");
            defer writer.popKey(); //...and remove it when you're done

            // This writes "key = " to the stream, you now have to fill in the value
            try writer.beginAssignment();

            // The underlying stream is just a standard stdlib writer
            // (you can put all of this into a single `out_stream.print`, it's just easier to explain like this)
            try writer.out_stream.writeByte('"'); // don't forget the quotation marks!
            try writer.out_stream.print("{s}{s}", .{ self.string1, self.string2 });
            try writer.out_stream.writeByte('"');
            try writer.out_stream.writeByte('\n'); // you're also in charge of ending the line
            // yes this is a very raw API
        }
    };

    const test_struct = MyCustomStruct{
        .my_fancy_number = 42,
        .string1 = "the",
        .string2 = "truth",
    };

    try stdout_writer.writeAll("# Custom serializer:\n");
    try tomlz.serialize(gpa, stdout_writer, test_struct);

    // Finally lets have a look how to implement custom serialize logic for a type you dont own
    // (Such as `std.HashMap`)

    try stdout_writer.writeAll("\n# Custom serializer for foreign type:\n");

    var my_map = std.StringHashMap(usize).init(gpa);
    defer my_map.deinit();
    try my_map.put("key1", 1);
    try my_map.put("key2", 2);

    // We need to use the internall write stream, since we need
    // to access it directly and not just write a value.
    var stream = tomlz.serializer.writeStream(gpa, stdout_writer);
    defer stream.deinit();

    var map_iter = my_map.iterator();
    while (map_iter.next()) |entry| {
        try stream.writeKeyValue(entry.key_ptr.*, entry.value_ptr.*);
    }

    // Note:
    // If you'd want to properly wrap the type, you should create a wrapper struct.
    // See the example for "owned" types above.
    //
    // If you're now wondering "But what if it's a very big foreign type,
    // but only in a few cases the default doesnt work?". Sadly you're out of luck
    // and need to handle the whole type :/
}
