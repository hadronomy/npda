add_rules("mode.debug", "mode.release")
set_languages("c++23")
set_toolchains("llvm")
set_runtimes("c++_shared")
add_requires("cli11 v2.7.2")
add_requires("catch2 v3.15.2", {system = false})
add_requires("reproc v14.2.7", {system = false})

add_includedirs("include", "src")
add_cxxflags("-Wall", "-Wextra", "-Wpedantic")

option("sanitize")
    set_default(false)
    set_showmenu(true)
    set_description("Enable AddressSanitizer and UndefinedBehaviorSanitizer")
option_end()

if has_config("sanitize") then
    add_cxxflags("-fsanitize=address,undefined", "-fno-omit-frame-pointer", {force = true})
    add_ldflags("-fsanitize=address,undefined", {force = true})
end

target("automata")
    set_kind("static")
    add_files("src/parsing/*.cc", "src/npda/machine.cc", "src/npda/parser.cc", "src/npda/search.cc",
              "src/turing/machine.cc", "src/turing/parser.cc", "src/turing/execution.cc", "src/turing/config_parser.cc",
              "src/turing/transition_text.cc", "src/prf/function.cc", "src/prf/trace.cc",
              "src/diagnostics/diagnostic.cc", "src/diagnostics/source.cc")

target("presentation")
    set_kind("static")
    add_deps("automata")
    add_files("src/npda/trace.cc", "src/turing/trace.cc", "src/turing/graphviz.cc",
              "src/prf/trace_render.cc", "src/diagnostics/render.cc", "src/terminal/*.cc")

target("cc")
    set_kind("binary")
    add_deps("presentation", "automata")
    add_packages("cli11")
    add_files("src/*.cc", "src/cli/*.cc")

target("domain_tests")
    set_kind("binary")
    add_deps("automata")
    add_packages("catch2", {components = {"main", "lib"}})
    add_files("tests/npda_tests.cc", "tests/turing_tests.cc",
              "tests/parser_tests.cc", "tests/prf_tests.cc")
    add_tests("domain")

target("presentation_tests")
    set_kind("binary")
    add_deps("presentation", "automata")
    add_packages("catch2", {components = {"main", "lib"}})
    add_includedirs("tests")
    add_files("tests/presentation_tests.cc", "tests/support/files.cc")
    add_tests("presentation")

target("verification_tests")
    set_kind("binary")
    add_deps("cc")
    add_packages("catch2", {components = {"main", "lib"}})
    add_packages("reproc")
    add_includedirs("tests")
    add_files("tests/cli_tests.cc", "tests/source_layout_tests.cc", "tests/support/*.cc")
    after_load(function (target)
        target:add("runenvs", "NPDA_SOURCE_ROOT", os.projectdir())
        target:add("runenvs", "NPDA_CLI_BINARY", path.absolute(target:dep("cc"):targetfile()))
    end)
    add_tests("verification")

-- Each generated translation unit contains one project header and no other includes.
target("header_checks")
    set_kind("object")
    add_packages("cli11")
    add_includedirs("tests")
    on_load(function (target)
        for _, pattern in ipairs({"include/**.h", "src/**.h", "tests/**.h"}) do
            for _, header in ipairs(os.files(pattern)) do
                local source = path.join(target:autogendir(), "headers", header .. ".cc")
                local contents = '#include "' .. path.absolute(header) .. '"\n'
                if not os.isfile(source) or io.readfile(source) ~= contents then
                    io.writefile(source, contents)
                end
                target:add("files", source)
            end
        end
    end)
