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

This project computes the power function xʸ as a primitive recursive function,
as required by the assignment in the
[primitive recursive function statement](/docs/CC_2627_Practica3.pdf).

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

On a color terminal, each role has a hue of its own: the names that a
recursion defines are bold yellow, the functions that compute, `Z` and `S`, are
cyan, the projections that route arguments are magenta, and the operators
recede. The recursive term, such as `pow(x, y)` on the right of its own
equation, therefore stands out, and so do the uses of `mul` and `add` that
link the three definitions. The trace tree and the call counts use the same
colors.

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

The command evaluates `pow` through a `prf::Program`, which compiles the
function objects into a flat array with one node for each object. The program
preserves the object evaluator's calls, call order, and trace, but each call
that the trace does not keep in a tree costs one switch instead of a virtual
call and a trace scope. With the program, `pow(2, 27)` takes about 1.6 seconds
instead of about 4 seconds. To check that the evaluators agree, tests compare
their results, call counts, trees, and errors across the same inputs, limits,
and tree sizes.

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
1 000 000 000 calls, which takes about 2 seconds. This limit allows powers up to
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
20 or later, with CI using clang-format 23.

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
| `cc prf -x <n> -y <n> [options]` | Compute xʸ as a primitive recursive function. |

The `prf` command takes these options:

| Option | Use |
| --- | --- |
| `-x <n>` | Required base, a natural number. |
| `-y <n>` | Required exponent, a natural number. |
| `-definition` | Show the equations that define `pow` before the result. See [Definitions](#definitions). |
| `-trace counts\|tree\|off` | Choose what to show below the result. The default is `counts`. See [Calls](#calls). |
| `-trace-limit <n>` | Show a maximum of `n` calls in the tree. The default is 200. |
| `-max-calls <n>` | Stop with an error after `n` calls. The default is 1 000 000 000. See [Limits](#limits). |

The program exits with status 0 if it computes the power, or with status 1 if
the command line has errors, the power does not fit in 64 bits, or the
evaluation needs more calls than `-max-calls` allows.

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
Its CLI tests compare full output with fixtures.

To repeat these checks with AddressSanitizer and UndefinedBehaviorSanitizer,
run `just sanitize`. This command leaves the debug build active afterward.

To go back to a release build:

```bash
xmake f -m release --sanitize=n -y
just verify
```

## License

This project uses the MIT License, whose terms are in [LICENSE](/LICENSE).
