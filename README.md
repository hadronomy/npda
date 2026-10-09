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

This project simulates nondeterministic pushdown automata (NPDA). It supports
acceptance by final state (APf) and by empty stack (APv). The assignment is in
the [NPDA statement](/docs/CC_2627_Practica1.pdf).

### NPDA File Format

```
q1 q2 q3      # Q: states
a b           # Σ: input alphabet
S A           # Γ: stack alphabet
q1            # q0: start state
S             # Z0: initial stack symbol
q3            # F: accepting states (APf only)
q1 a S q1 AS  # (q1, AS) ∈ δ(q1, a, S)
```

Each transition line is `from input top to push`. A `.` is ε. As the input, it
gives a move that reads no symbol. As the push string, it pops the top.

The stack top is always a symbol from Γ, because each move replaces it. The
push string starts with the new top. Thus, `AS` puts `A` on top of `S`.

The F line sets the acceptance mode. If the file has an F line, the automaton
accepts by final state (APf). If the file has no F line, the automaton accepts
by empty stack (APv). In both modes, the automaton must read all of the input.

The simulator does a breadth-first search. Thus, if an accepting path exists,
it finds a shortest one. After 100000 expansions, the search stops and the
input is undecided. Thus, an ε-loop that pushes symbols cannot make the
program run forever.

## Requirements

The project uses C++23 with usual headers and source files. It uses
`std::expected`, ranges, `std::format`, and `std::print`. The build uses LLVM
Clang 23 and libc++. The project does not use named C++ modules.

Install LLVM and xmake with the system package manager. On macOS:

```bash
brew install llvm xmake
```

On Ubuntu 24.04, install LLVM 23 and its libc++ packages from apt.llvm.org.
The CI workflow lists the necessary packages.

Run `mise install` to get `just`. Xmake installs Catch2 for the tests and
reproc++ for the CLI tests. The tests do not need Python. Formatting needs
clang-format 20 or later. CI uses clang-format 23.

## Build

```bash
just build
```

The command runs `xmake build`. To give options to xmake, add them after the
command.

## Usage

```bash
just run -help
```

If the sources changed, this command builds the binary again. Then it runs the
binary with the arguments. The examples below call the binary `cc`. Replace
`cc` with `just run` or with the full path. Each command accepts `-help`.

> [!IMPORTANT]
> The build writes the binary to `./build/<platform>/<arch>/<mode>/cc`. To
> find the file, run `find build -name cc -type f`.

### Commands

| Command | Use |
| --- | --- |
| `cc npda -config <file> [options] [inputs...]` | Run a pushdown automaton. |
| `cc explain <code>` | Show the meaning of a diagnostic code and how to correct the problem. |

The `npda` command accepts the options of the assignment statement and these
additional options:

| Option | Use |
| --- | --- |
| `-config <file>` | The machine definition. This option is necessary. |
| `-trace y\|n` | Show how the machine runs each input. The default is `y`. |
| `-trace-view <view>` | `detailed`, `compact`, or `tree`. The default is `detailed`. |
| `-trace-limit <n>` | Show a maximum of `n` configurations for each input. The default is 200. |
| `-explain` | Explain each transition in words. |
| `-in <file>` | Read one input from each line. The program skips blank lines. A line with only `.` is the empty string. |
| `-out <file>` | Write the trace to this file. The results stay on the screen. |
| `-verbose` | Show all of the diagnostics with the same code, not a summary. |

The inputs come from the command line and from `-in`. If these give no input,
the program reads the keyboard until the end of the file (Ctrl-D).

The exit status is 0 if the program accepts or rejects each input. The exit
status is 1 if the definition has errors, if the program cannot read a file,
or if an input stays undecided.

### Results

Each input ends with one result line: `accept` or `reject`, the input, and the
details. For an NPDA, the details of an accepted input are the transitions of
the accepting path. The numbers of the transitions, `δ1`, `δ2`, and more,
follow the order of the file.

```console
$ cc npda -config examples/APf/APf-1.txt -trace n aabb aab
APf-1.txt · final state F = {q3} · q0 = q1 · Z0 = S · 5 transitions

accept  aabb  δ1 δ2 δ3 δ4 δ5
reject  aab

2 inputs · 1 accepted · 1 rejected
```

### Traces

A trace starts with a list of the numbered transitions. Then it shows each
configuration that the run visits. An NPDA run is a breadth-first search. The
search gives each configuration an id `#n` in the order of its visits. The
trace writes each rule in full where it uses the rule. Thus, you can read the
trace from top to bottom.

The detailed view draws each configuration as a block. The block shows the
state, the input and the stack as cells, and each move out of the
configuration. Each move shows the configuration that it gives. The `▲` mark
is below the next input symbol. The top of the stack is on the left.

```console
$ cc npda -config examples/APv/APv-2.txt 0110
...
#2  from #0 by δ1  (p, 0, S) → (p, 0S)
    state   p
            ┌───┬───┬───┬───┐
    input   │ 0 │ 1 │ 1 │ 0 │
            └───┴───┴───┴───┘
                  ▲
            ┌───┬───┐
    stack   │ 0 │ S │  top first
            └───┴───┘
    moves   δ8   (p, ε, 0) → (q, 0)   ⇒ #4
            δ5   (p, 1, 0) → (p, 10)  ⇒ #5
```

The compact view shows one line for each configuration, with the moves out of
it below the line. The `▸` mark divides the input that the automaton read from
the input that remains. A move with `(seen)` goes to a configuration that the
search found before.

```console
$ cc npda -config examples/APv/APv-2.txt -trace-view compact 0110
...
trace 0110
  #    state  input  stack
  #0   p      ▸0110  S
    ├─ δ7   (p, ε, S) → (q, S)   ⇒ #1
    └─ δ1   (p, 0, S) → (p, 0S)  ⇒ #2
  #1   q      ▸0110  S
    └─ δ12  (q, ε, S) → (q, ε)   ⇒ #3
  #2   p      0▸110  0 S
  ...
```

If the search explored other branches before it accepted, the detailed view
and the compact view end with the accepting path.

The tree view draws the search as a tree. Each configuration is below the
configuration that gave it, with the transition between them.

```console
$ cc npda -config examples/APv/APv-2.txt -trace-view tree 0110
...
trace 0110
                         state  input  stack
  start                  p      ▸0110  S
  ├─ δ7                  q      ▸0110  S
  │  └─ δ12              q      ▸0110  ε          dead end
  └─ δ1                  p      0▸110  0 S
     ├─ δ8               q      0▸110  0 S        dead end
     └─ δ5               p      01▸10  1 0 S
        ├─ δ9            q      01▸10  1 0 S
        │  └─ δ11        q      011▸0  0 S
        │     └─ δ10     q      0110▸  S
        │        └─ δ12  q      0110▸  ε          accept
        └─ δ6            p      011▸0  1 1 0 S
  ...
```

## Formatting and verification

```bash
just format
just verify
just sanitize
```

`just format` formats the C++ headers and sources. `just format-check` finds
formatting errors and does not change the files.

`just verify` builds the project and finds formatting and layout errors. It
compiles each header alone and runs the Catch2 domain tests and presentation
tests. It also compares the full CLI output with the fixtures and runs all the
examples. `just sanitize` runs the same steps in a debug build with
AddressSanitizer and UndefinedBehaviorSanitizer. After this command, the debug
build stays active.

To go back to a release build:

```bash
xmake f -m release --sanitize=n -y
just verify
```

## License

This project uses the MIT License. The [LICENSE](/LICENSE) file gives the
details.
