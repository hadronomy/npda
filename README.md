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

This project simulates deterministic Turing machines with one or more tapes,
as required by the assignment in the
[Turing machine statement](/docs/CC_2627_Practica2.pdf).

### Turing Machine Assignment

The statement asks for two machines, each with a definition file for the
simulator and an image of its graph:

| Machine | Definition | Graph |
| --- | --- | --- |
| 1. L = { aⁿbᵐ \| m ≥ n, n > 0 } | [`examples/turing/anbm.turing`](/examples/turing/anbm.turing) | [`examples/turing/anbm.png`](/examples/turing/anbm.png) |
| 2. The number of b, a blank, and the number of a, with n = 1ⁿ | [`examples/turing/count_replace.turing`](/examples/turing/count_replace.turing) | [`examples/turing/count_replace.png`](/examples/turing/count_replace.png) |

Each definition file includes comments that explain its algorithm and the role
of each state. Machine 2 writes each count in unary, with `0` for a count of
zero, so `abbabaabb` gives `11111_1111`, `aa` gives `0_11`, and `bb` gives
`11_0`. Once the result is complete, the head stops on its first symbol.

To draw the two graphs again, install Graphviz and run `just graphs`.

#### Variants

The statement groups the variants into three pairs, and the simulator supports
both options in each pair. A file selects its variant through the config block,
with any omitted option taking the default shown below:

| Row of the statement | Default | Other option | Config key |
| --- | --- | --- | --- |
| Write and move | Simultaneous | Independent | `operation_mode` |
| Head moves | L, R, and S | L and R | `allow_stay` |
| Tape | Infinite in both directions | Infinite to the right | `tape_direction` |

The two machines of the assignment use the default variant.

- **Simultaneous.** Each transition writes a symbol and then moves the head.
- **Independent.** Each transition either writes a symbol while the head stays
  on its cell or moves the head while the symbol stays unchanged. With more
  tapes, each tape gets its own action.
- **L and R.** A transition with an `S` move is an error. In the independent
  mode, a write remains legal because it keeps the head on its cell without a
  move action.
- **Infinite to the right.** The tape has a left end at the first cell of the
  input, where a move to the left keeps the head on that cell.

#### Runs

At the start, the input is on the first tape with the head on its first symbol,
while the other tapes are blank. The machine runs until no transition applies
and accepts the input if it stops in a state of F, so entering a state of F
does not stop the machine while a transition still applies. When the run ends,
its result is on the first tape.

An input with a symbol outside Σ is rejected without a run. For inputs over Σ,
the run stops after 100000 steps if the machine has not halted, leaving the
input undecided.

### Turing Machine File Format

Machine files use the format from the statement, with spaces between items and
`#` to start a comment that continues to the end of the line. The lines come in
this order:

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

Each line after the number of tapes defines one transition. If F is empty,
omit its line: the simulator treats the line after b as the number of tapes
when it contains a single positive integer that is not a state.

The simulator checks the file against the formal definition of a Turing
machine:

- s is in Q, and F has only states of Q.
- b is in Γ and not in Σ, and each symbol of Σ is in Γ.
- Each symbol of Γ is one character.
- Each transition uses states of Q and symbols of Γ, with one action for each
  tape.
- Each move is legal in the variant of the file.
- The machine is deterministic: a state has only one transition for each
  read.

Each error points to its position in the file and includes a code that you can
look up with `cc explain <code>` to learn what it means and how to correct it.

#### The config block

The config block selects the variant and can appear anywhere in the file:

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

A transition for N tapes extends this form to
`from read1 ... readN to write1 move1 ... writeN moveN`, with one read symbol
per tape before `to` and one write symbol and move per tape after it. For
example, a transition for two tapes looks like this:

```
q0 a _ q0 a R a R
```

In the independent mode, a transition has the form
`from read1 ... readN to action1 ... actionN`, where each action is either a
symbol of Γ to write or a move. To keep these actions distinct, Γ cannot
contain the move letters `L`, `R`, or `S` in this mode. The following
transitions move right on `1` and write `1` on a blank:

```
q0 1 q0 R
q0 _ q1 1
```

#### Other examples

| File | What it shows |
| --- | --- |
| [`palindrome.turing`](/examples/turing/palindrome.turing) | Recognizes palindromes over {a, b} with two tapes |
| [`successor.turing`](/examples/turing/successor.turing) | The independent mode: 1ⁿ → 1ⁿ⁺¹ |
| [`simple_tm.turing`](/examples/turing/simple_tm.turing) | Uses a tape with a left end and no `S` |
| [`example_1.turing`](/examples/turing/example_1.turing) | Recognizes an odd number of 0, as in the course example |
| [`anbm_bad.turing`](/examples/turing/anbm_bad.turing) | Shows diagnostics for a definition with errors |

## Requirements

The project uses C++23 features such as `std::expected`, ranges, `std::format`,
and `std::print`, and builds with LLVM Clang 23 and libc++. Its code uses
conventional headers and source files without named C++ modules.

Install LLVM and xmake with the system package manager. On macOS:

```bash
brew install llvm xmake
```

On Ubuntu 24.04, install LLVM 23 and its libc++ packages from apt.llvm.org.
The CI workflow lists the necessary packages.

Run `mise install` to get `just`, then let xmake install Catch2 for the tests and
reproc++ for the CLI tests during the build. Formatting requires clang-format
20 or later, with CI using clang-format 23. Graphviz is optional unless you
want to draw images of Turing machines.

## Build

```bash
just build
```

The command runs `xmake build` and passes any options you add after the command
to xmake.

## Usage

```bash
just run -help
```

This command rebuilds the binary if the sources changed, then runs it with the
given arguments. In the examples below, replace the binary name `cc` with
`just run` or the full path to the binary. Each command accepts `-help`.

> [!IMPORTANT]
> The build writes the binary to `./build/<platform>/<arch>/<mode>/cc`, which
> you can find with `find build -name cc -type f`.

### Commands

| Command | Use |
| --- | --- |
| `cc turing -config <file> [options] [inputs...]` | Run a Turing machine, or draw it with `-graph`. |
| `cc explain <code>` | Show the meaning of a diagnostic code and how to correct the problem. |

The `turing` command accepts the options from the assignment statement and adds
options for traces, diagnostics, and graphs:

| Option | Use |
| --- | --- |
| `-config <file>` | Required path to the machine definition. |
| `-trace y\|n` | Show how the machine runs each input. The default is `y`. |
| `-trace-view <view>` | Choose `detailed` (the default) or `compact`. |
| `-trace-limit <n>` | Show a maximum of `n` configurations for each input. The default is 200. |
| `-explain` | Explain each transition in words. |
| `-in <file>` | Read one input per line, skipping blank lines and treating a line with only `.` as the empty string. |
| `-out <file>` | Write the trace to this file while the results stay on the screen. |
| `-verbose` | Show every diagnostic with the same code instead of a summary. |
| `-graph <file>` | Write the graph of the machine. See [Graphs](#graphs). |

The program collects inputs from the command line and from `-in`. If neither
source gives an input, it reads the keyboard until the end of the file
(Ctrl-D).

The program exits with status 0 if it accepts or rejects each input, or with
status 1 if the definition has errors, a file cannot be read, or an input stays
undecided.

### Results

Each input ends with a result line that gives `accept` or `reject`, the input,
and the first tape with the number of steps. The tape text spans the first
through the last nonblank cell and marks the cell under the head with
brackets, as in `ab[c]d`. If the input contains a symbol outside Σ, the details
identify that symbol.

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

A trace starts with the transitions numbered `δ1`, `δ2`, and so on in file
order, then shows each step of the run. Each step includes the full transition
that produced it, so you can read the trace from top to bottom.

In the detailed view, each step occupies a block that shows the state and each
tape as a row of cells, with `▲` below the head.

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

The compact view fits each step on one line by writing each tape as `ab[c]d`.

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

In the independent mode, each transition shows one action per tape in the same
form as the file, for example `(q0, 1) → (q0, R)`.

### Graphs

Use `-graph machine.dot` to write the Graphviz source of a Turing machine. With
any other extension, such as `-graph machine.png`, the program draws the image
in that format with the Graphviz `dot` program, which must be installed.

Each edge uses the label `reads / writes / moves`, or `reads / actions` in the
independent mode. For machines with more tapes, commas separate the items for
each tape within the label. Accepting states appear as double circles.

## Formatting and verification

```bash
just format
just verify
just sanitize
```

Use `just format` to format the C++ headers and sources, or `just format-check`
to find formatting errors without changing the files.

Along with formatting and layout checks, `just verify` builds the project,
compiles each header alone, and runs the Catch2 domain and presentation tests.
It also compares the full CLI output with the fixtures and runs all the
examples. To run the same steps in a debug build with AddressSanitizer and
UndefinedBehaviorSanitizer, use `just sanitize`, which leaves the debug build
active afterward.

To go back to a release build:

```bash
xmake f -m release --sanitize=n -y
just verify
```

## License

This project uses the MIT License, whose terms are in [LICENSE](/LICENSE).
