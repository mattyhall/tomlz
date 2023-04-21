const std = @import("std");

pub fn build(b: *std.build.Builder) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "tomlz",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_main_tests = b.addRunArtifact(main_tests);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);

    const fuzz_exe = b.addExecutable(.{ .name = "fuzz", .root_source_file = .{ .path = "src/fuzz.zig" } });
    fuzz_exe.linkLibC();
    b.installArtifact(fuzz_exe);
    const fuzz_compile_run = b.step("fuzz", "Build executable for fuzz testing afl-fuzz");
    fuzz_compile_run.dependOn(&fuzz_exe.step);
}
