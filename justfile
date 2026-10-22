project_name := "PR2-TURING-2627"
dir_path := `realpath .`
dir_name := `basename $(realpath .)`
bin_name := "cc"

_default:
    just --list -u

# Build the project.
build *ARGS:
    xmake build {{ARGS}}

# Build the project and run the tests. The arguments go to xmake test.
test *ARGS: build
    xmake test {{ARGS}}

# Draw the graphs of the two machines of the Turing assignment. Needs Graphviz.
graphs:
    just run turing -config examples/turing/anbm.turing -graph examples/turing/anbm.png
    just run turing -config examples/turing/count_replace.turing -graph examples/turing/count_replace.png

# Remove the build files.
clean:
    xmake clean

# Format the C++ files of the project.
format:
    find src include tests -type f \( -name '*.h' -o -name '*.cc' \) -print0 | xargs -0 "${CLANG_FORMAT:-clang-format}" -i

# Find formatting errors. Do not change the files.
format-check:
    find src include tests -type f \( -name '*.h' -o -name '*.cc' \) -print0 | xargs -0 "${CLANG_FORMAT:-clang-format}" --dry-run --Werror

# Build, then test the format, the headers, the CLI output, and the examples.
verify: build format-check
    xmake project -k compile_commands
    xmake test

# Do the verify steps with the address and undefined behavior sanitizers.
sanitize:
    xmake f -m debug --sanitize=y -y
    just verify

# Make a tarball of the project.
tar:
    cd .. && tar cvfz ./{{dir_name}}/{{project_name}}.tar.gz --exclude-from={{dir_name}}/.gitignore {{dir_name}}

# Run the executable. Build it again only if the source changed. Paths in the
# arguments are relative to the folder where you run just.
run *ARGS:
    xmake run --workdir={{invocation_directory()}} {{bin_name}} {{ARGS}}
