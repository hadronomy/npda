-- Build npda with xmake. Use LLVM Clang 23 from Homebrew.
-- Keep one flag set for all files. Do not add per-file flags.
-- Pin cli11 to v2.5.0. It matches the Zig build output byte for byte.
-- Move to v2.6.2 or later only with the cli11 module step.
add_rules("mode.debug", "mode.release")
set_languages("c++23")
set_toolchains("llvm")

add_requires("fmt 12.2.0", "cli11 v2.5.0")

-- Track headers used in module global fragments.
-- xmake rebuilds a BMI only when the .cppm file itself changes, so a
-- changed header would reuse a stale BMI. Push header edits into the
-- .cppm timestamp before the build starts. The header list comes from
-- the quoted includes in each .cppm file, so it cannot drift.
before_build(function (target)
    for _, cppm in ipairs(os.files("src/modules/*.cppm")) do
        local cppm_mtime = os.mtime(cppm)
        local text = io.readfile(cppm) or ""
        for header in text:gmatch('#include%s+"([^"]+)"') do
            local full = path.join("include", header)
            if os.isfile(full) and os.mtime(full) > cppm_mtime then
                io.writefile(cppm, text)
                break
            end
        end
    end
end)

target("cc")
    set_kind("binary")
    add_files("src/**.cc", "src/modules/*.cppm")
    add_includedirs("include")
    add_packages("fmt", "cli11")
    add_cxxflags(
        "-DASIO_HAS_THREADS",
        "-fcolor-diagnostics",
        "-Wall",
        "-Wextra",
        "-fexperimental-library",
        "-Wpedantic",
        "-Wno-deprecated-declarations",
        "-Wno-unqualified-std-cast-call",
        "-Wno-bitwise-instead-of-logical",
        "-fno-sanitize=undefined",
        "-U_LIBCPP_ENABLE_CXX17_REMOVED_UNEXPECTED_FUNCTIONS"
    )
