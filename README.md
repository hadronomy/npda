<div align="center">
  <img src="/.github/images/github-header-image.webp" alt="GitHub Header Image" />

  <!-- Badges -->
  <p></p>
  <a href="https://ull.es">
    <img
      alt="License"
      src="https://img.shields.io/badge/ULL-5C068C?style=for-the-badge&logo=gitbook&labelColor=302D41"
    />
  </a>
  <a href="https://github.com/hadronomy/PR5-DAA-2425/blob/main/LICENSE">
    <img
      alt="License"
      src="https://img.shields.io/badge/MIT-EE999F?style=for-the-badge&logo=starship&label=LICENSE&labelColor=302D41"
    />
  </a>
  <p></p>
  <!-- TOC -->
  <a href="#docs">Docs</a> •
  <a href="#requirements">Requirements</a> •
  <a href="#build">Build</a> •
  <a href="#usage">Usage</a> •
  <a href="#license">License</a>
  <hr />
</div>

## Docs

This project implements a NPDA (Non-Deterministic Push Down Automata). 
See the [docs](/docs/CC_2526_Practica1.pdf) pdf for more information about the assignment.

The automata is implemented to work with, empty stack finalization and with final state finalization.

> [!IMPORTANT] Location of the compiled binary
> The compiled binary lives in `./zig-out/bin`

## Requirements

This project uses [mise](https://github.com/jdx/mise) for dependency management. Mise provides a unified interface to manage runtime versions and project-specific tools.

To set up the development environment:

1. Install mise following the [official instructions](https://mise.jdx.dev/getting-started.html)
2. Run `mise install` in the project root to automatically install all dependencies defined in `mise.toml`

The project dependencies will be automatically installed and configured according to the project specifications.

After installing the requirements, you can run `just` without arguments to see a list of available tasks:

```bash
just
```

This will display all available commands defined in the justfile that you can use to build, test, and run the project.

## Build

```bash
just build
```

## Usage

```bash
just run --help
```

### Available Commands

- `run <file_path> <strings...>` - See if the given automata accepts the given string

### Examples

```bash
# Benchmark greedy CV generator algorithm
npda run ./examples/APf-1 "aabb"
```

## License

This project is licensed under the MIT License -
see the [LICENSE](/LICENSE) file for details.