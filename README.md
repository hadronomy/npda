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

This project simulates deterministic Turing machines with one tape or more.
The assignment is in the
[Turing machine statement](/docs/CC_2627_Practica2.pdf).

### Turing Machine Assignment

The statement asks for two machines. Each machine has a definition file for
the simulator and an image of its graph:

| Machine | Definition | Graph |
| --- | --- | --- |
| 1. L = { aⁿbᵐ \| m ≥ n, n > 0 } | [`examples/turing/anbm.turing`](/examples/turing/anbm.turing) | [`examples/turing/anbm.png`](/examples/turing/anbm.png) |
| 2. The number of b, a blank, and the number of a, with n = 1ⁿ | [`examples/turing/count_replace.turing`](/examples/turing/count_replace.turing) | [`examples/turing/count_replace.png`](/examples/turing/count_replace.png) |

The comments of each definition file explain the algorithm and each state.
Machine 2 writes `0` for a count of zero. Thus, `abbabaabb` gives
`11111_1111`, `aa` gives `0_11`, and `bb` gives `11_0`. The head stops on the
first symbol of the result.

To draw the two graphs again, run `just graphs`. This command needs Graphviz.

#### Variants

The statement has three rows of variants. The simulator has both options of
each row. The config block of a file selects the options. If the file does not
give an option, the simulator uses the default:

| Row of the statement | Default | Other option | Config key |
| --- | --- | --- | --- |
| Write and move | Simultaneous | Independent | `operation_mode` |
| Head moves | L, R, and S | L and R | `allow_stay` |
| Tape | Infinite in both directions | Infinite to the right | `tape_direction` |

The two machines of the assignment use the default variant.

- **Simultaneous.** Each transition writes a symbol and then moves the head.
- **Independent.** Each transition writes a symbol or moves the head, not
  both. A write keeps the head on its cell. A move keeps the symbol of the
  cell. With more tapes, each tape gets its own action.
- **L and R.** A transition with `S` is an error. In the independent mode, a
  write keeps the head on its cell, so a write is always legal.
- **Infinite to the right.** The tape has a left end at the first cell of the
  input. A move to the left from this cell keeps the head on the cell.

#### Runs

At the start, the input is on the first tape, and the head is on the first
symbol of the input. The other tapes are blank. The machine stops only when no
transition applies. It accepts if it stops in a state of F. Thus, a state of F
with transitions out of it does not stop the machine. The result of the run is
on the first tape.

If an input has a symbol that is not in Σ, the machine does not run, and the
input is rejected. After 100000 steps, the run stops and the input is
undecided.

### Turing Machine File Format

The file format is the format of the statement. A comment starts with `#` and
continues to the end of the line. Spaces divide the items on a line. The lines
come in this order:

```
q0 q1 q2      # Q: states
a b           # Σ: input alphabet
a b X _       # Γ: tape alphabet, one character for each symbol
q0            # s: start state
_             # b: blank symbol
q2            # F: accepting states
1             # number of tapes
q0 a q1 X R   # δ(q0, a) = (q1, X, R)
```

After the number of tapes, each line is one transition. If F is empty, write
no F line. The simulator reads the line after b as the number of tapes if it
is one positive integer and not a state.

The simulator makes sure that the definition obeys the formal definition:

- s is in Q, and F has only states of Q.
- b is in Γ and not in Σ. Each symbol of Σ is in Γ.
- Each symbol of Γ is one character.
- Each transition uses states of Q and symbols of Γ, with one action for each
  tape.
- Each move is legal in the variant of the file.
- The machine is deterministic: a state has only one transition for each
  read.

Each error has a code and points to its position in the file. To show the
meaning of a code and how to correct the error, run `cc explain <code>`.

#### The config block

The config block selects the variant. It can be at any position in the file:

```
# /// config
# tape_direction = bidirectional
# operation_mode = simultaneous
# allow_stay = true
# ///
```

| Key | Values |
| --- | --- |
| `tape_direction` | `bidirectional` (the default) or `right-only` |
| `operation_mode` | `simultaneous` (the default) or `independent` |
| `allow_stay` | `true` (the default) or `false` |

#### Transitions

In the simultaneous mode, a transition for one tape has the form
`from read to write move`:

- `from` and `to` are states from Q.
- `read` and `write` are tape symbols from Γ.
- `move` is `L` (left), `R` (right), or `S` (stay).

A transition for N tapes has the form
`from read1 ... readN to write1 move1 ... writeN moveN`. It has one read symbol
for each tape before `to`. After `to`, it has a write symbol and a move for
each tape. This is a transition for two tapes:

```
q0 a _ q0 a R a R
```

In the independent mode, a transition has the form
`from read1 ... readN to action1 ... actionN`. Each action is a symbol of Γ to
write, or a move. Thus, in this mode, Γ cannot contain `L`, `R`, or `S`. These
transitions move right on `1`, and write `1` on a blank:

```
q0 1 q0 R
q0 _ q1 1
```

#### Other examples

| File | What it shows |
| --- | --- |
| [`palindrome.turing`](/examples/turing/palindrome.turing) | Two tapes: the palindromes over {a, b} |
| [`successor.turing`](/examples/turing/successor.turing) | The independent mode: 1ⁿ → 1ⁿ⁺¹ |
| [`simple_tm.turing`](/examples/turing/simple_tm.turing) | A tape with a left end, and no `S` |
| [`example_1.turing`](/examples/turing/example_1.turing) | The example of the course: an odd number of 0 |
| [`anbm_bad.turing`](/examples/turing/anbm_bad.turing) | The diagnostics for a definition with errors |

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
reproc++ for the CLI tests. Formatting needs
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
| `cc turing -config <file> [options] [inputs...]` | Run a Turing machine, or draw it with `-graph`. |
| `cc explain <code>` | Show the meaning of a diagnostic code and how to correct the problem. |

The `turing` command accepts the options of the assignment statement, and some
more:

| Option | Use |
| --- | --- |
| `-config <file>` | The machine definition. This option is necessary. |
| `-trace y\|n` | Show how the machine runs each input. The default is `y`. |
| `-trace-view <view>` | `detailed` or `compact`. The default is `detailed`. |
| `-trace-limit <n>` | Show a maximum of `n` configurations for each input. The default is 200. |
| `-explain` | Explain each transition in words. |
| `-in <file>` | Read one input from each line. The program skips blank lines. A line with only `.` is the empty string. |
| `-out <file>` | Write the trace to this file. The results stay on the screen. |
| `-verbose` | Show all of the diagnostics with the same code, not a summary. |
| `-graph <file>` | Write the graph of the machine. See [Graphs](#graphs). |

The inputs come from the command line and from `-in`. If these give no input,
the program reads the keyboard until the end of the file (Ctrl-D).

The exit status is 0 if the program accepts or rejects each input. The exit
status is 1 if the definition has errors, if the program cannot read a file,
or if an input stays undecided.

### Results

Each input ends with one result line: `accept` or `reject`, the input, and the
details. The details are the first tape and the number of steps.
The tape starts at the first cell that is not blank and ends at the last one.
The cell below the head is in brackets, for example `ab[c]d`. If the input
has a symbol that is not in Σ, the details give this symbol.

```console
$ cc turing -config examples/turing/anbm.turing -trace n aabb aab abc
anbm.turing · q0 = q0 · b = _ · F = {q4} · 1 tape · bidirectional · simultaneous · stay allowed · 11 transitions

accept  aabb  XXYY[_]  13 steps
reject  aab   XXY[_]  7 steps
reject  abc   c is not in Σ

3 inputs · 1 accepted · 2 rejected
```

```console
$ cc turing -config examples/turing/count_replace.turing -trace n abbabaabb aa bb
count_replace.turing · q0 = q0 · b = _ · F = {q15} · 1 tape · bidirectional · simultaneous · stay allowed · 33 transitions

accept  abbabaabb  [1]1111_1111  74 steps
accept  aa         [0]_11  13 steps
accept  bb         [1]1_0  14 steps

3 inputs · 3 accepted · 0 rejected
```

### Traces

A trace starts with a list of the numbered transitions. The numbers, `δ1`,
`δ2`, and more, follow the order of the file. Then the trace shows each step
of the run. Each step writes in full the rule that gave it. Thus, you can read
the trace from top to bottom.

The detailed view draws each step as a block. The block shows the state and
each tape as cells. The `▲` mark is below the head.

```console
$ cc turing -config examples/turing/anbm.turing ab
...
#1  by δ1  (q0, a) → (q1, X, R)
    state   q1
            ┌───┬───┐
    tape    │ X │ b │
            └───┴───┘
                  ▲
```

The compact view shows one line for each step. It writes each tape as
`ab[c]d`.

```console
$ cc turing -config examples/turing/anbm.turing -trace-view compact abb
...
trace abb
  #   state  tape    by
  #0  q0     [a]bb
  #1  q1     X[b]b   δ1   (q0, a) → (q1, X, R)
  #2  q2     [X]Yb   δ5   (q1, b) → (q2, Y, L)
  ...
```

In the independent mode, a rule shows one action for each tape, as the file
writes it: `(q0, 1) → (q0, R)`.

### Graphs

`-graph machine.dot` writes the Graphviz source of a Turing machine. With a
different extension, for example `-graph machine.png`, the program draws the
image with the Graphviz `dot` program. Graphviz must be installed.

Each edge has the label `reads / writes / moves`. In the independent mode, the
label is `reads / actions`. With more tapes, commas divide the items of the
tapes. The double circle is an accepting state.

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
