const std = @import("std");
const ascci = std.ascii;

const Allocator = std.mem.Allocator;

pub const StringifyError = error{
    NoKey,
    NotRepresentable,
};

pub fn WriteStream(comptime OutStream: type) type {
    return struct {
        const Self = @This();

        const Error = OutStream.Error || StringifyError;

        stream: OutStream,

        key_stack: std.ArrayList([]const u8),

        array_depth: usize = 0,

        pub fn writeKeyValue(self: *Self, key: []const u8, value: anytype) Error!void {
            try self.pushKey(key);
            try self.write(value);
            self.popKey();
        }

        pub fn write(self: *Self, value: anytype) Error!void {
            const T = @TypeOf(value);

            return switch (@typeInfo(T)) {
                .Int,
                .ComptimeInt,
                .Float,
                .ComptimeFloat,
                .Bool,
                .Enum,
                .EnumLiteral,
                .ErrorSet,
                => {
                    try self.writeAssignment();
                    try self.writeInline(value);
                    try self.stream.writeByte('\n');
                },
                .Optional => {
                    if (value) |payload| {
                        return self.write(payload);
                    }
                },
                .Struct => {
                    if (comptime std.meta.trait.hasFn("tomlzStringify")(T)) {
                        return value.tomlzStringify(self);
                    }
                    return self.writeTable(value);
                },
                .Union => {
                    if (comptime std.meta.trait.hasFn("tomlzStringify")(T)) {
                        return value.tomlzStringify(self);
                    }

                    const info = @typeInfo(T).Union;
                    if (info.tag_type) |UnionTagType| {
                        inline for (info.fields) |u_field| {
                            if (value == @field(UnionTagType, u_field.name)) {
                                try self.write(@field(value, u_field.name));
                                break;
                            }
                        } else {
                            unreachable; // No active tag?
                        }
                        return;
                    } else {
                        @compileError("Unable to stringify untagged union '" ++ @typeName(T) ++ "'");
                    }
                },
                .Pointer => |ptr_info| switch (ptr_info.size) {
                    .One => switch (@typeInfo(ptr_info.child)) {
                        .Array => {
                            // Coerce `*[N]T` to `[]const T`.
                            const Slice = []const std.meta.Elem(ptr_info.child);
                            return self.write(@as(Slice, value));
                        },
                        else => {
                            return self.write(value.*);
                        },
                    },
                    .Many, .Slice => {
                        if (ptr_info.size == .Many and ptr_info.sentinel == null)
                            @compileError("Unable to stringify type '" ++ @typeName(T) ++ "' without sentinel");
                        const slice = if (ptr_info.size == .Many) std.mem.span(value) else value;

                        if (comptime canInline(T)) {
                            try self.writeAssignment();
                            try self.writeInline(slice);
                            try self.stream.writeByte('\n');
                            return;
                        }

                        self.array_depth += 1;

                        for (slice) |elem| {
                            try self.write(elem);
                        }

                        self.array_depth -= 1;
                    },
                    else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'."),
                },
                .Array => {
                    // Coerce `[N]T` to `*const [N]T` (and then to `[]const T`).
                    return self.write(&value);
                },
                .Vector => |info| {
                    const array: [info.len]info.child = value;
                    return self.write(&array);
                },
                .Void => {},
                else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'."),
            };
        }

        fn writeInline(self: *Self, value: anytype) Error!void {
            const T = @TypeOf(value);

            // TODO: custom inline function e.g. for datetimes

            return switch (@typeInfo(T)) {
                .Int => |info| {
                    if (info.bits > 64) {
                        return error.NotRepresentable;
                    }

                    return self.stream.print("{}", .{value});
                },
                .ComptimeInt => return self.writeInline(@as(std.math.IntFittingRange(value, value), value)),
                .Float, .ComptimeFloat => {
                    if (@as(f64, @floatCast(value)) != value) {
                        return error.NotRepresentable;
                    }

                    return self.stream.print("{}", .{value});
                },
                .Bool => return self.stream.print("{}", .{value}),

                .Enum, .EnumLiteral => {
                    return self.stream.print("\"{s}\"", .{@tagName(value)});
                },
                .ErrorSet => return self.stream.print("\"{s}\"", .{@errorName(value)}),
                .Array => {
                    // Coerce `[N]T` to `*const [N]T` (and then to `[]const T`).
                    return self.writeInline(&value);
                },
                .Vector => |info| {
                    const array: [info.len]info.child = value;
                    return self.writeInline(&array);
                },
                .Pointer => |ptr_info| switch (ptr_info.size) {
                    .One => switch (@typeInfo(ptr_info.child)) {
                        .Array => {
                            // Coerce `*[N]T` to `[]const T`.
                            const Slice = []const std.meta.Elem(ptr_info.child);
                            return self.writeInline(@as(Slice, value));
                        },
                        else => {
                            return self.writeInline(value.*);
                        },
                    },
                    .Many, .Slice => {
                        if (ptr_info.size == .Many and ptr_info.sentinel == null)
                            @compileError("Unable to stringify type '" ++ @typeName(T) ++ "' without sentinel");
                        const slice = if (ptr_info.size == .Many) std.mem.span(value) else value;

                        // This is a []const u8, or some similar Zig string.
                        if (ptr_info.child == u8 and std.unicode.utf8ValidateSlice(slice)) {
                            return self.stream.print("\"{s}\"", .{value});
                        }

                        try self.stream.writeByte('[');

                        var i: usize = 0;
                        while (i < slice.len - 1) : (i += 1) {
                            try self.writeInline(slice[i]);
                            try self.stream.writeAll(", ");
                        }

                        try self.writeInline(slice[i]);
                        try self.stream.writeByte(']');
                    },
                    else => @compileError("Inlining value of type '" ++ @typeName(T) ++ "' is not supported"),
                },
                else => @compileError("Inlining value of type '" ++ @typeName(T) ++ "' is not supported"),
            };
        }

        fn writeTable(self: *Self, value: anytype) Error!void {
            const T = @TypeOf(value);
            const S = @typeInfo(T).Struct;

            if (S.fields.len == 1 and @typeInfo(S.fields[0].type) == .Struct) {
                const field = S.fields[0];
                try self.pushKey(field.name);
                try self.writeTable(@field(value, field.name));
                self.popKey();
                return;
            }

            try self.beginTable();

            inline for (S.fields) |Field| {
                if (comptime !canInline(Field.type)) continue;

                try self.pushKey(Field.name);
                try self.writeAssignment();
                try self.writeInline(@field(value, Field.name));
                try self.stream.writeByte('\n');
                self.popKey();
            }

            inline for (S.fields) |Field| {
                if (comptime canInline(Field.type)) continue;

                try self.pushKey(Field.name);
                try self.write(@field(value, Field.name));
                self.popKey();
            }
        }

        fn pushKey(self: *Self, key: []const u8) !void {
            try self.key_stack.append(key);
        }

        fn popKey(self: *Self) void {
            _ = self.key_stack.pop();
        }

        fn writeKey(self: *Self, key: []const u8) Error!void {
            var is_bare = true;
            for (key) |char| {
                if (ascci.isAlphanumeric(char)) continue;

                if (char != '_' and char != '-') {
                    is_bare = false;
                    break;
                }
            }

            if (!is_bare) {
                try self.stream.writeByte('"');
            }

            try self.stream.writeAll(key);

            if (!is_bare) {
                try self.stream.writeByte('"');
            }
        }

        fn writeAssignment(self: *Self) Error!void {
            if (self.key_stack.items.len == 0) return error.NoKey;
            try self.writeKey(self.key_stack.getLast());
            try self.stream.writeAll(" = ");
        }

        pub fn beginTable(self: *Self) Error!void {
            // this is the root table
            if (self.key_stack.items.len == 0) return;

            if (self.array_depth > 0) {
                try self.stream.writeAll("[[");
            } else {
                try self.stream.writeByte('[');
            }

            var i: usize = 0;
            while (i < self.key_stack.items.len - 1) : (i += 1) {
                try self.writeKey(self.key_stack.items[i]);
                try self.stream.writeAll(".");
            }
            try self.writeKey(self.key_stack.items[i]);

            if (self.array_depth > 0) {
                try self.stream.writeAll("]]");
            } else {
                try self.stream.writeByte(']');
            }

            try self.stream.writeByte('\n');
        }

        pub fn deinit(self: *Self) void {
            self.key_stack.deinit();

            self.* = undefined;
        }
    };
}

fn canInline(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Int,
        .ComptimeInt,
        .Float,
        .ComptimeFloat,
        .Bool,
        .Enum,
        .ErrorSet,
        => true,
        .Pointer => |info| canInline(info.child),
        .Array => |info| canInline(info.child),
        .Vector => |info| canInline(info.child),
        else => false,
    };
}

const testing = std.testing;

fn testWriteStream(value: anytype, key: ?[]const u8, expected: []const u8) !void {
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    var writer = buffer.writer();

    var stream = WriteStream(@TypeOf(writer)){
        .stream = writer,
        .key_stack = std.ArrayList([]const u8).init(testing.allocator),
    };
    defer stream.deinit();

    if (key) |payload| {
        try stream.pushKey(payload);
    }
    try stream.write(value);

    try testing.expectEqualStrings(expected, buffer.items);
}

fn testWriteStreamFailure(value: anytype, key: ?[]const u8, err: anyerror) !void {
    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    var writer = buffer.writer();

    var stream = WriteStream(@TypeOf(writer)){
        .stream = writer,
        .key_stack = std.ArrayList([]const u8).init(testing.allocator),
    };
    defer stream.deinit();

    if (key) |payload| {
        try stream.pushKey(payload);
    }
    try stream.write(value);

    try testing.expectError(err, stream.write(value));
}

test "stringify basic types" {
    // integers
    try testWriteStream(42, "truth", "truth = 42\n"); // this tests comptime_int
    try testWriteStream(@as(u16, 42), "truth", "truth = 42\n");
    // unrepresentable integers fail at compile time

    // floats
    try testWriteStream(@as(f64, 13.37), "value", "value = 1.337e+01\n");
    // unrepresentable floats fail at compile time

    // bools
    try testWriteStream(false, "truth", "truth = false\n");

    // enums
    const SomeEnum = enum { SomeState };
    try testWriteStream(SomeEnum.SomeState, "value", "value = \"SomeState\"\n");
    try testWriteStream(.SomeState, "value", "value = \"SomeState\"\n");

    // error sets
    const SomeError = error{TerriblyWrong};
    try testWriteStream(SomeError.TerriblyWrong, "value", "value = \"TerriblyWrong\"\n");

    // pointers
    try testWriteStream(&&5, "value", "value = 5\n");

    // optionals
    try testWriteStream(@as(?u16, 42), "value", "value = 42\n");

    // strings
    try testWriteStream("test", "value", "value = \"test\"\n");
}

test "stringify arrays" {
    const test_array = [_]u16{ 1, 2, 3 };
    // arrays
    try testWriteStream(test_array, "value", "value = [1, 2, 3]\n");

    // slices
    try testWriteStream(test_array[0..], "value", "value = [1, 2, 3]\n");

    // vectors
    try testWriteStream(@Vector(3, u16){ 1, 2, 3 }, "value", "value = [1, 2, 3]\n");

    // array of arrays
    const array_of_arrays = [_][3]u16{ test_array, test_array };
    try testWriteStream(array_of_arrays, "value", "value = [[1, 2, 3], [1, 2, 3]]\n");
}

test "stringify union" {
    const MyUnion = union(enum) {
        one: u16,
        two: u16,
    };

    try testWriteStream(MyUnion{ .two = 2 }, "value", "value = 2\n");
}

test "stringify table" {
    // empty table
    try testWriteStream(.{}, "empty", "[empty]\n");

    // "root" table
    try testWriteStream(.{
        .field1 = 1,
        .field2 = 2,
    }, null,
        \\field1 = 1
        \\field2 = 2
        \\
    );

    // nested table
    try testWriteStream(.{
        .field1 = 1,
        .child = .{
            .field3 = 3,
        },
        .field2 = 2,
    }, null,
        \\field1 = 1
        \\field2 = 2
        \\[child]
        \\field3 = 3
        \\
    );

    // nested table with transparent ancestor
    try testWriteStream(.{ .parent = .{ .child = .{} } }, null,
        \\[parent.child]
        \\
    );
}

test "stringify array of tables" {
    const MyStruct = struct {
        field1: u16 = 1,
        field2: u16 = 2,
    };

    const array_of_tables = [_]MyStruct{ .{}, .{} };

    try testWriteStream(array_of_tables, "arr",
        \\[[arr]]
        \\field1 = 1
        \\field2 = 2
        \\[[arr]]
        \\field1 = 1
        \\field2 = 2
        \\
    );
}

test "stringify array of nested tables" {
    const A = struct { content: []const u8 };

    const B = struct {
        a: A,
    };

    // array of tables with tables as fields
    const array_of_tables = [_]B{
        .{ .a = .{ .content = "never" } },
        .{ .a = .{ .content = "gonna" } },
    };

    try testWriteStream(array_of_tables, "arr",
        \\[[arr.a]]
        \\content = "never"
        \\[[arr.a]]
        \\content = "gonna"
        \\
    );

    // array of tables with tables as fields and arrays of tables as fields(ultimate stress test)
    const C = struct {
        b: B,
        as: [3]A,
    };

    const stress_test = [_]C{
        .{
            .b = .{ .a = .{ .content = "give" } },
            .as = [_]A{
                .{ .content = "you" },
                .{ .content = "up" },
                .{ .content = "never" },
            },
        },
        .{
            .b = .{ .a = .{ .content = "gonna" } },
            .as = [_]A{
                .{ .content = "let" },
                .{ .content = "you" },
                .{ .content = "down" },
            },
        },
    };

    try testWriteStream(stress_test, "arr",
        \\[[arr]]
        \\[[arr.b.a]]
        \\content = "give"
        \\[[arr.as]]
        \\content = "you"
        \\[[arr.as]]
        \\content = "up"
        \\[[arr.as]]
        \\content = "never"
        \\[[arr]]
        \\[[arr.b.a]]
        \\content = "gonna"
        \\[[arr.as]]
        \\content = "let"
        \\[[arr.as]]
        \\content = "you"
        \\[[arr.as]]
        \\content = "down"
        \\
    );
}

test "stringify with custom function" {
    const A = struct {
        pub fn tomlzStringify(self: *const @This(), stream: anytype) !void {
            _ = self;
            try stream.beginTable();
            try stream.writeKeyValue("i_dont", "exist");
        }
    };

    try testWriteStream(A{}, null,
        \\i_dont = "exist"
        \\
    );
}

test "stringify tomlz table" {
    const tomlz = @import("main.zig");
    const Table = tomlz.Table;
    const Value = tomlz.Value;

    var table = Table{
        .source = .top_level,
        .closed = false,
    };
    defer table.deinit(testing.allocator);

    try table.table.put(
        testing.allocator,
        try testing.allocator.dupe(u8, "somevalue"),
        Value{ .integer = 42 },
    );
    try table.table.put(
        testing.allocator,
        try testing.allocator.dupe(u8, "somestring"),
        Value{ .string = try testing.allocator.dupe(u8, "notastring") },
    );

    try testWriteStream(table, null,
        \\somestring = "notastring"
        \\somevalue = 42
        \\
    );
}

test "stringify correctly quote keys" {
    try testWriteStream(42, "ASCII_encoded-key42", "ASCII_encoded-key42 = 42\n");
    try testWriteStream(42, "mr🐢turtle", "\"mr🐢turtle\" = 42\n");
}
