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

This project simulates nondeterministic pushdown automata (NPDA), Turing
machines, and primitive recursive functions. The assignments are in the
[NPDA statement](/docs/CC_2627_Practica1.pdf) and the
[Turing machine statement](/docs/CC_2526_Practica2.pdf).

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

### Turing Machine File Format

A Turing machine file gives the states, the symbols, the start state, the
blank symbol, the optional accepting states, and the transition rules. A
machine can have one tape or more. A comment starts with `#` and continues to
the end of the line. Spaces divide the items on a line.

The lines come in this order:

1. The states (Q)
2. The input symbols (Σ)
3. The tape symbols (Γ)
4. The start state (q0)
5. The blank symbol (b)
6. The accepting states (F). This line is optional.
7. The transitions, one on each line, until the end of the file

> [!NOTE]
> If the line after b has the shape of a transition, the parser reads it as a
> transition. Otherwise, it reads the optional F line.

#### The config block

The config block sets the number of tapes and other settings. It can be at any
position in the file:

```
# /// config
# num_tapes = 2
# tape_direction = right-only
# operation_mode = independent
# allow_stay = true
# ///
```

| Key | Values |
| --- | --- |
| `num_tapes` | A positive integer. The default is 1. |
| `tape_direction` | `bidirectional` (the default) or `right-only` |
| `operation_mode` | `independent` (the default) or `simultaneous` |
| `allow_stay` | `true` (the default) or `false` |

#### Transitions

A transition for one tape has the form `from read to write move`:

- `from` and `to` are states from Q.
- `read` and `write` are tape symbols from Γ.
- `move` is `L` (left), `R` (right), or `S` (stay).

```
q0 0 q1 1 R
```

A transition for N tapes has the form
`from read1 ... readN to write1 move1 ... writeN moveN`. It has one read symbol
for each tape before `to`. After `to`, it has a write symbol and a move for
each tape. This is a transition for two tapes:

```
q0 0 1 q1 1 R 1 R
```

#### Examples

A machine with one tape:

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

A machine with two tapes:

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
> - All symbols in transitions must be in Γ.
> - The q0 line and the b line must have exactly one item.
> - The parser ignores comments, but it reads the lines of the config block.

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
clang-format 20 or later. CI uses clang-format 23. Graphviz is optional. Only
the images of Turing machines need it.

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
| `cc turing -config <file> [options] [inputs...]` | Run a Turing machine, or draw it with `-graph`. |
| `cc prf <base> <exponent> [-trace counts\|tree\|off]` | Compute the power function as a primitive recursive function. |
| `cc explain <code>` | Show the meaning of a diagnostic code and how to correct the problem. |

The `npda` and `turing` commands accept the options of the assignment
statement, and some more:

| Option | Use |
| --- | --- |
| `-config <file>` | The machine definition. This option is necessary. |
| `-trace y\|n` | Show how the machine runs each input. The default is `y`. |
| `-trace-view <view>` | `detailed`, `compact`, or `tree`. Only `npda` has `tree`. The default is `detailed`. |
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

For a Turing machine, the details are the first tape and the number of steps.
The tape starts at the first cell that is not blank and ends at the last one.
The cell below the head is in brackets, for example `ab[c]d`.

```console
$ cc turing -config examples/turing/anbm.turing -trace n aabb abb
anbm.turing · q0 = q0 · b = _ · F = {qacc} · 1 tape · bidirectional · independent · stay allowed · 23 transitions

reject  aabb  XXYY[_]  28 steps
accept  abb   XYb[_]  15 steps

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

A Turing machine has only one path. Thus, its views show one step after the
other. The detailed view draws each tape as cells, with a mark below the head.
The compact view writes each tape as `ab[c]d`.

### Graphs

`-graph machine.dot` writes the Graphviz source of a Turing machine. With a
different extension, for example `-graph machine.svg`, the program draws the
image with the Graphviz `dot` program. Graphviz must be installed.

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
