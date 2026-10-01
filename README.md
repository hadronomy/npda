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

This project implements Turing machines, nondeterministic pushdown automata (NPDA), and primitive recursive functions.
See the [docs](/docs/CC_2526_Practica2.pdf) and [npda docs](/docs/CC_2526_Practica1.pdf) pdf for more information about the assignment.

Turing machines support multiple tapes, both operation modes, and configurable tape movement.
The file configuration selects these settings.

The input file format is as follows

### Turing Machine File Format

What this file describes
- A Turing Machine: its states, symbols, start/blank choices, optional accepting states, and the transition rules it follows.
- Supports single-tape or multi-tape machines.
- Comments start with # and continue to end of line.

Basic layout (in order)
1) Line of states (Q)
2) Line of input symbols (Σ)
3) Line of tape symbols (Γ)
4) Line with the start state (q0)
5) Line with the blank symbol (b)
6) Optional line of accepting states (F)
7) Transition lines (one per line) until the end

> [!NOTE]
>- Items are words separated by spaces.
>- The F line is optional. If the next line looks like a transition, it’s treated as a transition (not F).

Configuration block (optional)
- Set options like number of tapes anywhere in the file:
```
# /// config
# num_tapes = 2
# tape_direction = right-only
# operation_mode = independent
# allow_stay = true
# ///
```

`tape_direction` accepts `right-only` or `bidirectional`. `operation_mode` accepts
`independent` or `simultaneous`. `allow_stay` accepts `true` or `false`.

Single-tape transitions
- Format: from read to write move
  - from/to are states (from Q)
  - read/write are tape symbols (from Γ)
  - move is L (left), R (right), or S (stay)
- Example:
```
q0 0 q1 1 R
```

Multi-tape transitions (if num_tapes = N)
- Format: from read1 ... readN to write1 move1 write2 move2 ... writeN moveN
  - One read symbol per tape before “to”
  - Then, for each tape: write symbol and move (L/R/S)
- Example for 2 tapes:
```
q0 0 1 q1 1 R 1 R
```

Minimal single-tape example
```
# states
q0 q1 qaccept
# input alphabet
0 1
# tape alphabet
0 1 _
# start state
q0
# blank symbol
_
# accepting states
qaccept
# transitions
q0 0 q1 1 R
q1 1 qaccept 1 S
```

Minimal two-tape example (with config)
```
# /// config
# num_tapes = 2
# operation_mode = simultaneous
# allow_stay = true
# ///
q0 q1 qf
0 1
0 1 _
q0
_
qf
# transitions: from r1 r2 to w1 m1 w2 m2
q0 0 1 q1 1 R 1 R
q1 _ _ qf _ S _ S
```

> [!WARNING]
>- Symbols in transitions must be listed in Γ (tape symbols).
>- q0 and b lines must have exactly one item.
>- If the first line after b looks like a transition, it won’t be treated as F.
>- Use # to add comments; they’re ignored (except special config lines).


NPDA execution supports acceptance by final state, empty stack, both, or either.

> [!IMPORTANT] Location of the compiled binary
> The build writes the binary under `./build/<platform>/<arch>/<mode>/cc`. Run `find build -name cc -type f` to locate the file.

## Requirements

The project uses C++23 with ordinary headers and source files. It uses
`std::expected`, ranges, `std::format`, `std::print`, and `std::jthread`.
The build uses LLVM Clang 23 and libc++. Named C++ modules are not required.

Install LLVM and xmake from the system package manager. On macOS:

```bash
brew install llvm xmake
```

On Ubuntu 24.04, install LLVM 23 and its libc++ packages from apt.llvm.org.
The CI workflow lists the required packages.

Run `mise install` for `just`. Xmake installs Catch2 for all tests and reproc++
for CLI process capture. Header checks compile through Xmake. The verification
path needs no Python runtime. Formatting requires clang-format 20 or later. Use
clang-format 23 to match CI. Graphviz is optional and is required only for
Turing machine image generation.

## Build

```bash
just build
```

The command runs `xmake build`. To pass options to xmake, add them after the command.

## Usage

```bash
just run --help
```

If the sources changed, the command rebuilds the binary. Then it runs the binary with the given arguments.

### Available Commands

- `turing <file_path> <strings...>`: run a Turing machine on the input strings.
- `npda <file_path> <strings...>`: run an NPDA on the input strings.
- `prf <base> <exponent>`: evaluate exponentiation as a primitive recursive function.
- `explain <code>`: explain a parser diagnostic.

Turing machine configuration comes from the input file. The CLI has no separate
tape configuration overrides. Run `just run <command> --help` for execution and
trace options.

### Examples

The examples below call the binary as `cc`. Replace `cc` with `just run` or with the full path.

```bash
cc turing ./examples/turing/count-replace.turing "aabb"
```

> [!IMPORTANT]
> If you use the `-g` flag without providing any input string
> it will generate a visualization of the given turing machine
> **You WILL need to install `graphviz` in your environment for this to work**


```bash
cc turing ./examples/turing/count-replace.turing -g
```

---

```bash
cc prf 2 3
```

This evaluates `pow(2, 3)` and shows the call counts. Add `--mode full` for the call tree.

---

```bash
cc npda ./examples/APf/APf-1.txt "aabb"
```

## Formatting and verification

```bash
just format
just verify
just sanitize
```

`just format` formats all project C++ headers and sources. The configuration
starts with Google style and sets two spacing options:

```yaml
SeparateDefinitionBlocks: Always
WrapNamespaceBodyWithEmptyLines: Always
```

The formatter inserts blank lines between function and class definitions,
around namespace bodies, and before a definition after a group of `using`
declarations. `SeparateDefinitionBlocks` requires clang-format 14 or later.
`WrapNamespaceBodyWithEmptyLines` requires clang-format 20 or later.
`just format-check` checks formatting without changing files.

To select the Homebrew formatter explicitly:

```bash
CLANG_FORMAT="$(brew --prefix llvm)/bin/clang-format" just format
```

`just verify` builds the project, checks formatting and source layout, compiles
all headers on their own, and runs the Catch2 domain and presentation tests. It also
compares full CLI output against fixtures and runs the example corpus.
`just sanitize` runs these checks in a debug build with AddressSanitizer and
UndefinedBehaviorSanitizer. It leaves the debug configuration active.

To return to a release build:

```bash
xmake f -m release --sanitize=n -y
just verify
```

## License

This project is licensed under the MIT License -
see the [LICENSE](/LICENSE) file for details.
