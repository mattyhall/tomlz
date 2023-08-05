const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

<<<<<<< HEAD
    const exe = b.addExecutable(.{
        .name = "simple",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
||||||| parent of 8ee9b97 (purge gyro and zigmod, update README)
    const mode = b.standardReleaseOptions();
=======
    const exe = b.addExecutable(.{
        .name = "simple",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    
    // If we have the project in our repository then we can just add it as a module
    const tomlz = b.addModule("tomlz", .{
        .source_file = .{ .path = "../../src/main.zig"},
    });
>>>>>>> 8ee9b97 (purge gyro and zigmod, update README)

<<<<<<< HEAD
    // If we have the project in our repository then we can just add it as a module
    const tomlz = b.addModule("tomlz", .{
        .source_file = .{ .path = "../../src/main.zig" },
    });
||||||| parent of 8ee9b97 (purge gyro and zigmod, update README)
    const exe = b.addExecutable("simple", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    // If we have the project in our repository then we can just add it as a package
    exe.addPackagePath("tomlz", "../../src/main.zig");
    exe.install();
=======
    exe.addModule("tomlz", tomlz);
>>>>>>> 8ee9b97 (purge gyro and zigmod, update README)

<<<<<<< HEAD
    exe.addModule("tomlz", tomlz);

    const run_cmd = b.addRunArtifact(exe);
||||||| parent of 8ee9b97 (purge gyro and zigmod, update README)
    const run_cmd = exe.run();
=======
    const run_cmd = b.addRunArtifact(exe);
>>>>>>> 8ee9b97 (purge gyro and zigmod, update README)
    run_cmd.step.dependOn(b.getInstallStep());
<<<<<<< HEAD

||||||| parent of 8ee9b97 (purge gyro and zigmod, update README)
=======
    
>>>>>>> 8ee9b97 (purge gyro and zigmod, update README)
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
