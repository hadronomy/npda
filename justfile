project_name := "PR3-PRF-2526"
dir_path := `realpath .`
dir_name := `basename $(realpath .)`
bin_name := "cc"

_default:
    just --list -u

# Build the project
build *ARGS:
    xmake build {{ARGS}}

# Clean build artifacts
clean:
    xmake clean

# Run the behavior proof (Fails when output changes)
verify:
    ./tests/proof.sh $(find build -name cc -type f | head -n 1)

# Create a tarball of the project
tar:
    cd .. && tar cvfz ./{{dir_name}}/{{project_name}}.tar.gz --exclude-from={{dir_name}}/.gitignore {{dir_name}}

# Run the executable, only rebuilding if source has changed
run *ARGS:
    xmake run {{bin_name}} {{ARGS}}
