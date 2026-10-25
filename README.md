<div align="center">
  <img src="/.github/images/github-header-image.webp" alt="GitHub Header Image" />

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
  <a href="#docs">Docs</a> •
  <a href="#requirements">Requirements</a> •
  <a href="#build">Build</a> •
  <a href="#usage">Usage</a> •
  <a href="#license">License</a>
  <hr />
</div>

## Docs

This project simulates nondeterministic pushdown automata (NPDA), Turing
machines, and primitive recursive functions, as required by the assignments in
the [NPDA statement](/docs/CC_2627_Practica1.pdf), the
[Turing machine statement](/docs/CC_2627_Practica2.pdf), and the
[primitive recursive function statement](/docs/CC_2627_Practica3.pdf).

### NPDA file format

```
q1 q2 q3      # Q: states
a b           # Σ: input alphabet
S A           # Γ: stack alphabet
q1            # q0: start state
S             # Z0: initial stack symbol
q3            # F: accepting states (APf only)
q1 a S q1 AS  # (q1, AS) ∈ δ(q1, a, S)
```

Each transition line has the form `from input top to push`, where `.` stands
for ε. As `input`, it selects a move that reads no symbol, while as `push`,
it pops the top without replacing it.

Because each move replaces the stack top, the top is always a symbol from Γ.
The push string starts with the new top, so `AS` puts `A` on top of `S`.

The F line also sets the acceptance mode: its presence selects acceptance by
final state (APf), while its absence selects acceptance by empty stack (APv).
Both modes require the automaton to read all of the input.

The simulator searches breadth-first, so it finds a shortest accepting path
whenever one exists. After 100 000 expansions, the search stops and leaves the
input undecided, which keeps an ε-loop that pushes symbols from running the
program forever.

### Turing machine assignment

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

The simulator supports both variants in each of the statement's three pairs.
Each file selects its variants through a configuration block, with omitted
options taking the defaults shown below:

| Row of the statement | Default | Other option | Configuration key |
| --- | --- | --- | --- |
| Write and move | Simultaneous | Independent | `operation_mode` |
| Head moves | L, R, and S | L and R | `allow_stay` |
| Tape | Infinite in both directions | Infinite to the right | `tape_direction` |

The two machines of the assignment use the default variant.

- **Simultaneous.** Each transition writes a symbol and then moves the head.
- **Independent.** Each transition either writes a symbol while the head stays
  on its cell or moves the head while the symbol stays unchanged. With more
  tapes, each tape gets its own action.
- **L and R.** A transition with an `S` move is an error, while an independent
  write remains legal because it keeps the head on its cell without a move
  action.
- **Infinite to the right.** The tape has a left end at the first cell of the
  input, where a move to the left keeps the head on that cell.

#### Runs

At the start, the input is on the first tape with the head on its first symbol,
while the other tapes are blank. The machine runs until no transition applies,
then accepts the input if its state belongs to F. Entering F therefore does
not stop a run while a transition still applies. When the run ends, its result
is on the first tape.

An input with a symbol outside Σ is rejected without a run. For inputs over Σ,
the run stops after 100 000 steps if the machine has not halted, leaving the
input undecided.

### Turing machine file format

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

#### The configuration block

The configuration block selects the variant and can appear anywhere in the file:

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

### Primitive recursive function assignment

The statement asks for a program that computes the power function xʸ as a
primitive recursive function. To meet this requirement, the program builds xʸ
from the basic functions through combination, composition, and primitive
recursion. The `prf` command evaluates this function with x and y from the `-x`
and `-y` options, then prints the result and the number of function calls:

```console
$ cc prf -x 2 -y 3 -trace off
pow(2, 3) = 8
100 function calls
```

#### Definitions

The program uses the notation of the course slides (Tema 1), in which a
function f: ℕⁿ → ℕᵐ takes n natural numbers and returns m natural numbers.
The program builds its functions from the following basic functions and
operations:

| Function | Definition |
| --- | --- |
| Zero | Z(x) = 0 |
| Successor | S(x) = x + 1 |
| Projection | Pᵢⁿ(x₁, …, xₙ) = xᵢ, with n ≥ 1 and 1 ≤ i ≤ n |
| Combination | (f × g)(X) = (f(X), g(X)) |
| Composition | (f ∘ g)(X) = f(g(X)) |
| Primitive recursion | f(X, 0) = g(X) and f(X, S(y)) = h(X, y, f(X, y)) |

Combination builds a tuple of values, and composition turns that tuple into
the arguments of another function, so the slides' f ∘ (g₁, …, gₘ) is
f ∘ (g₁ × … × gₘ) here. From these parts, the program builds addition,
multiplication, and the power function, each by recursion on its last
argument:

```
add(x, 0)    = P₁¹(x)                                = x
add(x, S(y)) = (S ∘ P₃³)(x, y, add(x, y))            = S(add(x, y))
mul(x, 0)    = Z(x)                                  = 0
mul(x, S(y)) = (add ∘ (P₃³ × P₁³))(x, y, mul(x, y))  = add(mul(x, y), x)
pow(x, 0)    = (S ∘ Z)(x)                            = 1
pow(x, S(y)) = (mul ∘ (P₃³ × P₁³))(x, y, pow(x, y))  = mul(pow(x, y), x)
```

The `-definition` flag prints these equations before the result. Because the
program writes them from the function objects that it evaluates, they always
match the code:

```console
$ cc prf -x 2 -y 3 -definition -trace off
pow(x, 0)    = (S ∘ Z)(x)
pow(x, S(y)) = (mul ∘ (P₃³ × P₁³))(x, y, pow(x, y))
mul(x, 0)    = Z(x)
mul(x, S(y)) = (add ∘ (P₃³ × P₁³))(x, y, mul(x, y))
add(x, 0)    = P₁¹(x)
add(x, S(y)) = (S ∘ P₃³)(x, y, add(x, y))

pow(2, 3) = 8
100 function calls
```

The slides call f(X, 0) = g(X) the limit equation and f(X, S(y)) =
h(X, y, f(X, y)) the recursion equation. In the recursion equation, h takes
the recursive term f(X, y) as its last argument, but the slides leave open how
h passes its arguments to other functions.

Because each recursion runs on its last argument, the steps of `mul` and `pow`
pass the previous value first. Each step of `pow` thus runs `mul` for x steps,
rather than one step per unit of the previous power. If both steps were
reversed to `add(x, mul(x, y))` and `mul(x, pow(x, y))`, both inner recursions
would run on the previous values, so the calls of `pow` would grow with x²ʸ
instead of xʸ, and 2²³ would need about 5·10¹³ calls instead of 5·10⁷.

Because `pow(x, 0)` starts from S ∘ Z, its value is 1 even when x is 0, so
`pow(0, 0)` is 1.

#### Design

The builders in [`include/prf/function.h`](/include/prf/function.h) follow the
structure of the mathematical definitions, so the code for `mul` directly
expresses its definition:

```cpp
// mul(x, 0)    = Z(x)
// mul(x, S(y)) = (add ∘ (P₃³ × P₁³))(x, y, mul(x, y))
return Recursion(
    Zero(),
    Composition(
        Addition(),
        Combination(Projection<3, 3>(), Projection<1, 3>())
    ),
    "mul"
);
```

Each builder returns an `Fn<N, M>`, the type of a function f: ℕᴺ → ℕᴹ, so the
compiler checks that the parts fit. For composition f ∘ g, this check requires
f to take as many arguments as g returns results. It also rejects projections
such as `Projection<4, 3>()`. Because C++ operators have fixed meanings and
none represents ∘, each operation uses a named builder.

Each `Fn` refers to an object of a `prf::Function` subclass, which supplies the
function's name and evaluates it. The basic subclasses represent zero, the
successor, and the projections, while the other three hold the functions that
each operation joins. These objects never change after construction, so larger
functions can share them.

During evaluation, each call receives a `prf::Trace`, which counts calls and
stops evaluation at its call limit. The trace can also record the call count
for each function or the tree of calls, depending on its mode.

#### Calls

Each application counts as one call, whether it applies a basic function or
one that an operation builds. Thus, `(S ∘ P₃³)(x, y, z)` makes three calls:
one to the composition, one to `P₃³`, and one to `S`. To check the total for
`pow`, the tests compare many values of x and y against a formula derived from
these definitions.

The `-trace` option selects what the command shows below the result:

| Option | Shows |
| --- | --- |
| `-trace counts` | The call count for each function, most calls first (the default). |
| `-trace tree` | A tree of all the calls, each with its arguments and results. |
| `-trace off` | Nothing more than the result and the number of calls. |

```console
$ cc prf -x 2 -y 3
pow(2, 3) = 8
100 function calls

  calls  function
     23  P₃³
     15  S
     14  S ∘ P₃³
      9  P₁³
      9  P₃³ × P₁³
      6  P₁¹
      6  add
      6  add ∘ (P₃³ × P₁³)
      4  Z
      3  mul
      3  mul ∘ (P₃³ × P₁³)
      1  S ∘ Z
      1  pow
```

By default, the tree shows the first 200 calls in the order in which they
occur. To change how many calls it shows, use `-trace-limit`:

```console
$ cc prf -x 2 -y 3 -trace tree -trace-limit 4
pow(2, 3) = 8
100 function calls

├─ (S ∘ Z)(2) = 1
│  ├─ Z(2) = 0
│  └─ S(0) = 1
└─ (mul ∘ (P₃³ × P₁³))(2, 0, 1) = 2

  … The trace does not show 95 more calls. Increase -trace-limit to show them.
```

#### Limits

The program stores natural numbers in 64 bits, so it rejects powers that do
not fit, such as 2⁶⁴, before evaluation. Even powers that fit can need too many
calls, because the functions count in steps of one and the number of calls
grows with xʸ. For example, 2⁶³ needs about 10¹⁹ calls.

To limit evaluation time, the program stops with an error after
1 000 000 000 calls, which takes 5 to 7 seconds. This limit allows powers up to
2²⁷, 3¹⁷, and 10⁸. To change it, use `-max-calls`:

```console
$ cc prf -x 2 -y 30 -max-calls 100000
error: pow(2, 30) needs more than 100 000 calls. Increase -max-calls to let it run longer.
```

## Requirements

The project uses C++23 features such as `std::expected`, ranges, `std::format`,
and `std::print`, and it builds with LLVM Clang 23 and libc++. The code uses
conventional headers and source files, without named C++ modules.

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
| `cc npda -config <file> [options] [inputs...]` | Run a pushdown automaton. |
| `cc turing -config <file> [options] [inputs...]` | Run a Turing machine, or draw it with `-graph`. |
| `cc prf -x <n> -y <n> [-definition] [-trace counts\|tree\|off] [-trace-limit <n>] [-max-calls <n>]` | Compute xʸ as a primitive recursive function. |
| `cc explain <code>` | Show the meaning of a diagnostic code and how to correct the problem. |

The `npda` and `turing` commands accept the options from the assignment
statement and add options for traces, diagnostics, and graphs:

| Option | Use |
| --- | --- |
| `-config <file>` | Required path to the machine definition. |
| `-trace y\|n` | Show how the machine runs each input. The default is `y`. |
| `-trace-view <view>` | Choose `detailed` (the default), `compact`, or `tree`, which only `npda` has. |
| `-trace-limit <n>` | Show a maximum of `n` configurations for each input. The default is 200. |
| `-explain` | Explain each transition in words. |
| `-in <file>` | Read one input per line, skipping blank lines and treating a line with only `.` as the empty string. |
| `-out <file>` | Write the trace to this file while the results stay on the screen. |
| `-verbose` | Show every diagnostic with the same code instead of a summary. |
| `-graph <file>` | Write the graph of a Turing machine. See [Graphs](#graphs). |

The program collects inputs from the command line and from `-in`, or reads the
keyboard until end-of-file (Ctrl-D) if neither source gives an input.

A successful command exits with status 0, which for `npda` and `turing` means
that the program accepts or rejects every input. Errors in the command line or
a definition, or files that the program cannot read, instead give status 1.
The program also uses status 1 when an input stays undecided or `prf` cannot
compute the power.

### Results

Each input ends with a result line that gives `accept` or `reject`, the input,
and the details. For an NPDA, the details of an accepted input list the
transitions of the accepting path, numbered `δ1`, `δ2`, and so on in file
order:

```console
$ cc npda -config examples/APf/APf-1.txt -trace n aabb aab
APf-1.txt · final state F = {q3} · q0 = q1 · Z0 = S · 5 transitions

accept  aabb  δ1 δ2 δ3 δ4 δ5
reject  aab

2 inputs · 1 accepted · 1 rejected
```

For a Turing machine, the details give the first tape and the number of steps.
The tape text spans the first through the last nonblank cell and marks the cell
under the head with brackets, as in `ab[c]d`. If the input contains a symbol
outside Σ, the details identify that symbol.

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
order, then shows each configuration that the run visits. For an NPDA's
breadth-first search, the trace numbers each configuration `#n` in visit order.
Alongside each step, it shows the full transition that produced it, so you can
read the trace from top to bottom.

In the detailed view, each configuration occupies a block with its state and
rows of cells for its input and stack. Within those rows, `▲` marks the next
input symbol, while the stack starts with its top on the left. The block also
shows each applicable move and the configuration that it produces.

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

The compact view gives each configuration one line and lists the moves out of
it below that line. The `▸` mark divides the input that the automaton has read
from the input that remains, and a move marked `(seen)` leads to a
configuration that the search found before.

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

If the search explored other branches before it accepted, the detailed and
compact views end with the accepting path.

The tree view draws the search as a tree, placing each configuration below the
configuration that produced it, with the transition between them.

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

A Turing machine follows a single path, so its views show one step after
another. The detailed view draws each tape as a row of cells with `▲` below the
head, while the compact view writes each tape as `ab[c]d`. In the independent
mode, each transition shows one action per tape in the same form as the file,
for example `(q0, 1) → (q0, R)`.

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
Its CLI tests compare full output with fixtures and run all the examples.

To repeat these checks with AddressSanitizer and UndefinedBehaviorSanitizer,
run `just sanitize`. This command leaves the debug build active afterward.

To go back to a release build:

```bash
xmake f -m release --sanitize=n -y
just verify
```

## License

This project uses the MIT License, whose terms are in [LICENSE](/LICENSE).
