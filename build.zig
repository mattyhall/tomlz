const std = @import("std");

pub fn build(b: *std.build.Builder) !void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("tomlz", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    const main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const fuzz_exe = b.addExecutable("fuzz", "src/fuzz.zig");
    fuzz_exe.setBuildMode(mode);
    fuzz_exe.linkLibC();
    fuzz_exe.install();
    const fuzz_compile_run = b.step("fuzz", "Build executable for fuzz testing afl-fuzz");
    fuzz_compile_run.dependOn(&fuzz_exe.install_step.?.step);
}
