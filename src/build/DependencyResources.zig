const std = @import("std");
const Build = std.Build;
const Step = Build.Step;

const utils = @import("utils.zig");

const DependencyResources = @This();

steps: []*Step,

pub fn init(
    b: *Build,
    target: Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    cppflags: []const []const u8,
    exe: *Step.Compile,
) !DependencyResources {
    var steps = std.ArrayList(*Step).init(b.allocator);
    errdefer steps.deinit();

    var lib_paths = std.StringHashMap([]const u8).init(b.allocator);
    errdefer {
        var it = lib_paths.iterator();
        while (it.next()) |entry| {
            b.allocator.free(entry.key_ptr.*);
            b.allocator.free(entry.value_ptr.*);
        }
        lib_paths.deinit();
    }

    // CLI11
    const cli11_src = b.dependency("CLI11", .{
        .target = target,
        .optimize = optimize,
    });
    const cli11_lib = cli11_src.path("./include");
    try lib_paths.put(
        try b.allocator.dupe(u8, "CLI11"),
        try b.allocator.dupe(u8, cli11_src.path(".").getPath(b)),
    );

    // FMT
    const fmt_src = b.dependency("fmt", .{
        .target = target,
        .optimize = optimize,
    });
    const fmt_lib_path = fmt_src.path("./include");
    const fmt = b.addStaticLibrary(.{
        .name = "fmt",
        .optimize = optimize,
        .target = target,
    });
    fmt.linkLibCpp();
    fmt.addIncludePath(fmt_lib_path);
    fmt.installHeadersDirectory(
        fmt_lib_path,
        "",
        .{
            .include_extensions = &[_][]const u8{".h"},
        },
    );
    try lib_paths.put(
        try b.allocator.dupe(u8, "fmt"),
        try b.allocator.dupe(u8, fmt_src.path(".").getPath(b)),
    );

    // Find FMT source files
    const fmt_source_files = try utils.findDependencyFiles(
        b,
        fmt_src.path("./src"),
        &[_][]const u8{ ".c", ".cpp", ".cxx", ".c++", ".cc" },
    );

    for (fmt_source_files) |file| {
        fmt.addCSourceFile(.{
            .file = file,
            .flags = cppflags,
        });
    }
    try steps.append(&fmt.step);

    // Tabulate
    const tabulate_src = b.dependency("tabulate", .{
        .target = target,
        .optimize = optimize,
    });
    const tabulate_lib = tabulate_src.path("./include");
    try lib_paths.put(
        try b.allocator.dupe(u8, "tabulate"),
        try b.allocator.dupe(u8, tabulate_src.path(".").getPath(b)),
    );

    exe.addIncludePath(cli11_lib);
    exe.addIncludePath(tabulate_lib);
    exe.linkLibrary(fmt);
    // TODO: Use a dependency for this

    // b.installDirectory(.{
    //     .source_dir = b.path("resources"),
    //     .install_dir = .bin,
    //     .install_subdir = "resources",
    // });

    // b.installDirectory(.{
    //     .source_dir = b.path("assets"),
    //     .install_dir = .bin,
    //     .install_subdir = "assets",
    // });

    // Add dependency on all steps
    for (steps.items) |step| {
        exe.step.dependOn(step);
    }

    return .{
        .steps = steps.items,
    };
}
