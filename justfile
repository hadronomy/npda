project_name := "PR3-PRF-2526"
dir_path := `realpath .`
dir_name := `basename $(realpath .)`
bin_name := "cc"

_default:
    just --list -u

# Build the project
build *ARGS:
    xmake build {{ARGS}}

# Build and run tests. Pass arguments through to xmake test.
test *ARGS: build
    xmake test {{ARGS}}

# Clean build artifacts
clean:
    xmake clean

# Format project-owned C++ files.
format:
    find src include tests -type f \( -name '*.h' -o -name '*.cc' \) -print0 | xargs -0 "${CLANG_FORMAT:-clang-format}" -i

# Check formatting without changing files.
format-check:
    find src include tests -type f \( -name '*.h' -o -name '*.cc' \) -print0 | xargs -0 "${CLANG_FORMAT:-clang-format}" --dry-run --Werror

# Check interfaces, source layout, CLI output, and the example corpus.
verify: build format-check
    xmake project -k compile_commands
    xmake test

# Run the same checks with address and undefined behavior sanitizers.
sanitize:
    xmake f -m debug --sanitize=y -y
    just verify

# Create a tarball of the project
tar:
    cd .. && tar cvfz ./{{dir_name}}/{{project_name}}.tar.gz --exclude-from={{dir_name}}/.gitignore {{dir_name}}

# Run the executable, only rebuilding if source has changed
run *ARGS:
    xmake run {{bin_name}} {{ARGS}}
