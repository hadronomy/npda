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

This project implements a Turing Machine and a NPDA (Non-Deterministic Push Down Automata), and Primitive Recursive Functions.
See the [docs](/docs/CC_2526_Practica2.pdf) and [npda docs](/docs/CC_2526_Practica1.pdf) pdf for more information about the assignment.

The turing machine is implemented to work primaraly in simultaneous mode, with any type of expansion, and with or
without the stay movement.

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
# tape_direction = right-only        # or "bidirectional"
# operation_mode = independent       # or "simultaneous"
# allow_stay = true                  # true or false
# ///
```

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


The automata is implemented to work with, empty stack finalization and with final state finalization.

> [!IMPORTANT] Location of the compiled binary
> The build writes the binary under `./build/<platform>/<arch>/<mode>/cc`. Run `find build -name cc -type f` to locate the file.

## Requirements

This project builds with xmake and LLVM Clang 23. The code uses C++23 modules and `import std`. Apple Clang and older LLVM releases do not work.

Install the toolchain first. On macOS, run `brew install llvm xmake`. On Ubuntu 24.04, install `clang-23` and `libc++-23-dev` from apt.llvm.org. Then install xmake.

Then install the remaining tools. Run `mise install` in the project root. This command provides `just`, the task runner.

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

- `turing <file_path> <strings...>` - Executes and sees if the given turing machine accepts the given strings
- `npda <file_path> <strings...>` - See if the given automata accepts the given string

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

This will execute `pow(2, 3)` and show the trace.

---

```bash
# Benchmark greedy CV generator algorithm
cc npda ./examples/APf-1 "aabb"
```

### Behavior proof

```bash
just verify
```

The command runs the six behavior cases plus the full example corpus. When output changes, the command fails. Run it before you push.

## Modules

The source is split into C++20 modules. Each domain area owns one module file under `src/modules/`. Third-party code stays in global fragments, except CLI11, which ships its own module file under `third_party/cli11/`. The build uses one flag set for every module step. Different flags break BMI loads.

## License

This project is licensed under the MIT License -
see the [LICENSE](/LICENSE) file for details.