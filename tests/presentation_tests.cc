#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include <catch2/catch_test_macros.hpp>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/render.h"
#include "diagnostics/source.h"
#include "npda/definition.h"
#include "npda/machine.h"
#include "npda/trace.h"
#include "prf/function.h"
#include "prf/trace.h"
#include "prf/trace_render.h"
#include "support/files.h"
#include "terminal/style.h"
#include "turing/config.h"
#include "turing/definition.h"
#include "turing/graphviz.h"
#include "turing/machine.h"
#include "turing/tape.h"
#include "turing/trace.h"
#include "turing/transition.h"

namespace {

using automata::Intern;
using test_support::TemporaryDirectory;

const terminal::Style kPlain = terminal::Style::Plain();

/// Returns the lines of `text` that contain `needle`.
std::vector<std::string> LinesWith(
    const std::string& text,
    std::string_view needle
) {
  std::vector<std::string> lines;
  std::istringstream stream(text);
  for (std::string line; std::getline(stream, line);) {
    if (line.contains(needle)) lines.push_back(line);
  }
  return lines;
}

/// State q has two moves that read 'a' and one ε-move to a dead end. Only the
/// first move goes to the final state f.
npda::Definition Branching() {
  const auto q = Intern("q");
  const auto a = Intern("a");
  const auto bottom = Intern("S");
  return {
      .start_state = q,
      .initial_stack_symbol = bottom,
      .acceptance = npda::AcceptByFinalState{{Intern("f")}},
      .transitions = {
          {.from = q,
           .input = a,
           .top = bottom,
           .to = Intern("f"),
           .push = {bottom}},
          {.from = q,
           .input = std::nullopt,
           .top = bottom,
           .to = Intern("dead"),
           .push = {bottom}},
          {.from = q,
           .input = Intern("b"),
           .top = bottom,
           .to = Intern("f"),
           .push = {}},
          {.from = q,
           .input = a,
           .top = Intern("A"),
           .to = Intern("f"),
           .push = {}},
          {.from = q,
           .input = a,
           .top = bottom,
           .to = Intern("dead"),
           .push = {}}
      }
  };
}

std::string TraceNpda(
    const npda::Definition& definition,
    std::string_view text,
    npda::TraceView view,
    std::size_t limit = 200
) {
  const npda::Machine machine(definition);
  const auto input = automata::InputSymbols(text);
  std::ostringstream out;
  const auto result = npda::TraceRun(
      machine,
      input,
      1000,
      {.view = view, .limit = limit},
      out,
      kPlain
  );
  REQUIRE(result.has_value());
  return out.str();
}

/// Returns the position of `needle` in `text`. Tests use it to compare the
/// order of lines.
std::size_t Find(const std::string& text, std::string_view needle) {
  const auto position = text.find(needle);
  REQUIRE(position != std::string::npos);
  return position;
}

TEST_CASE(
    "Diagnostic rendering uses source labels without ANSI in plain text",
    "[presentation][diagnostics]"
) {
  auto source = diag::SourceFile::From("test.npda", "q0 qf\na\n");
  diag::DiagnosticSet diagnostics;
  diag::Error("E0007", "unknown state")
      .Label({0, 2}, "use a declared state")
      .Note("check the Q line")
      .Emit(diagnostics);
  std::ostringstream output;
  diag::Render(
      output,
      source,
      diagnostics,
      {.color = diag::ColorMode::kNever, .charset = diag::CharSet::kAscii}
  );
  CHECK(output.str().contains("E0007"));
  CHECK(output.str().contains("test.npda"));
  CHECK(output.str().contains("use a declared state"));
  CHECK(!output.str().contains('\x1b'));
}

TEST_CASE("NPDA transitions read as formal moves", "[presentation][npda]") {
  const auto definition = Branching();
  CHECK(
      npda::FormatTransition(definition.transitions[0], kPlain) ==
      "(q, a, S) → (f, S)"
  );
  CHECK(
      npda::FormatTransition(definition.transitions[1], kPlain) ==
      "(q, ε, S) → (dead, S)"
  );
  CHECK(
      npda::FormatTransition(definition.transitions[2], kPlain) ==
      "(q, b, S) → (f, ε)"
  );
  CHECK(npda::FormatPath(std::vector<std::size_t>{0, 9}, kPlain) == "δ1 δ10");
  CHECK(
      npda::Describe(definition, kPlain) ==
      "final state F = {f} · q0 = q · Z0 = S · 5 transitions"
  );
}

TEST_CASE(
    "NPDA transition lists explain each move on request",
    "[presentation][npda]"
) {
  std::ostringstream out;
  npda::WriteTransitions(out, Branching(), kPlain, /*explain=*/true);
  CHECK(out.str().contains("δ1  (q, a, S) → (f, S)"));
  CHECK(out.str().contains("read a, keep S, go to f"));
  CHECK(out.str().contains("read nothing, keep S, go to dead"));
  CHECK(out.str().contains("read b, pop S, go to f"));
}

TEST_CASE(
    "Compact NPDA traces list the moves that apply, in search order",
    "[presentation][npda][trace]"
) {
  const auto output = TraceNpda(Branching(), "a", npda::TraceView::kCompact);
  CHECK(output.starts_with("\ntrace a\n"));
  CHECK(
      Find(output, "δ2  (q, ε, S) → (dead, S)  ⇒ #1") <
      Find(output, "δ1  (q, a, S) → (f, S)     ⇒ #2")
  );
  CHECK(Find(output, "⇒ #2") < Find(output, "δ5  (q, a, S) → (dead, ε)  ⇒ #3"));
  CHECK_FALSE(output.contains("δ3"));
  CHECK_FALSE(output.contains("δ4"));
  CHECK(output.contains("#1  dead   ▸a     S      dead end"));
  CHECK(output.contains("#2  f      a▸     S      accept"));
  CHECK(output.contains("⇒ #3 (not explored)"));
  CHECK_FALSE(output.contains('\x1b'));
}

TEST_CASE(
    "Detailed NPDA traces draw each configuration as a block",
    "[presentation][npda][trace]"
) {
  const auto output = TraceNpda(Branching(), "a", npda::TraceView::kDetailed);
  CHECK(output.contains("#0  start\n"));
  CHECK(output.contains("#2  from #0 by δ1  (q, a, S) → (f, S)\n"));
  CHECK(output.contains("    input   │ a │  all read\n"));
  CHECK(output.contains("    stack   │ S │  top first\n"));
  CHECK(output.contains("    moves   none (dead end)\n"));
  CHECK(output.contains("    accept  input read in final state f\n"));
}

TEST_CASE(
    "NPDA search trees nest each configuration under its origin",
    "[presentation][npda][trace]"
) {
  const auto output = TraceNpda(Branching(), "a", npda::TraceView::kTree);
  CHECK(output.contains("  start  q      ▸a     S\n"));
  CHECK(output.contains("  ├─ δ2  dead   ▸a     S      dead end\n"));
  CHECK(output.contains("  ├─ δ1  f      a▸     S      accept\n"));
  CHECK(output.contains("  └─ δ5  dead   a▸     ε      not explored\n"));
}

TEST_CASE(
    "NPDA traces end with the accepting path after a wider search",
    "[presentation][npda][trace]"
) {
  const auto output = TraceNpda(Branching(), "a", npda::TraceView::kCompact);
  CHECK(Find(output, "accepting path") > Find(output, "accept\n"));
  CHECK(output.contains("δ1  (q, a, S) → (f, S)  ⇒  f  a▸  S\n"));
}

TEST_CASE(
    "NPDA traces stop at the limit with one notice",
    "[presentation][npda][trace]"
) {
  const auto output = TraceNpda(Branching(), "a", npda::TraceView::kCompact, 2);
  CHECK(LinesWith(output, "does not show 1 more configuration.").size() == 1);
  CHECK_FALSE(output.contains("#2  f"));
}

turing::Definition Sweep() {
  const auto q = Intern("q");
  const auto a = Intern("a");
  const auto blank = Intern("_");
  return {
      .config = {},
      .num_tapes = 1,
      .input_alphabet = {a},
      .start_state = q,
      .blank = blank,
      .accepting_states = {Intern("f")},
      .transitions = {
          {.from = q,
           .tapes = {{.read = a, .write = a, .move = turing::HeadMove::kRight}},
           .to = q},
          {.from = q,
           .tapes =
               {{.read = blank,
                 .write = blank,
                 .move = turing::HeadMove::kStay}},
           .to = Intern("f")}
      }
  };
}

TEST_CASE(
    "Turing tapes print as the course writes them",
    "[presentation][turing]"
) {
  const auto a = Intern("a");
  const auto blank = Intern("_");
  turing::Tape tape{
      .cells = {blank, a, Intern("b"), Intern("c"), blank},
      .head = 3
  };
  CHECK(turing::FormatTape(tape, blank, kPlain) == "ab[c]");
  tape.head = 5;
  CHECK(turing::FormatTape(tape, blank, kPlain) == "abc_[_]");
  tape.head = 2;
  CHECK(turing::FormatTape(tape, blank, kPlain, 0) == "…[b]…");
  CHECK(turing::FormatTape({}, blank, kPlain) == "[_]");
  CHECK(
      turing::FormatTransition(
          Sweep().transitions[0],
          turing::OperationMode::kSimultaneous,
          kPlain
      ) == "(q, a) → (q, a, R)"
  );
  const turing::TapeAction write{
      .read = a,
      .write = Intern("x"),
      .move = turing::HeadMove::kStay
  };
  const turing::TapeAction move{
      .read = a,
      .write = a,
      .move = turing::HeadMove::kLeft
  };
  CHECK(
      turing::FormatTransition(
          {.from = Intern("q"), .tapes = {write, move}, .to = Intern("f")},
          turing::OperationMode::kIndependent,
          kPlain
      ) == "(q, a, a) → (f, x, L)"
  );
}

std::string TraceTuring(std::string_view text, turing::TraceView view) {
  const auto machine = turing::Machine::Create(Sweep());
  REQUIRE(machine.has_value());
  const auto input = automata::InputSymbols(text);
  std::ostringstream out;
  const auto result = turing::TraceRun(
      *machine,
      input,
      1000,
      {.view = view, .limit = 3},
      out,
      kPlain
  );
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  return out.str();
}

TEST_CASE(
    "Compact Turing traces show one row per step up to the limit",
    "[presentation][turing][trace]"
) {
  const auto output = TraceTuring("aaaa", turing::TraceView::kCompact);
  CHECK(output.contains("  #0  q      [a]aaa\n"));
  CHECK(output.contains("  #1  q      a[a]aa  δ1  (q, a) → (q, a, R)\n"));
  CHECK_FALSE(output.contains("aaa[a]"));
  CHECK(LinesWith(output, "does not show 3 more steps.").size() == 1);
}

TEST_CASE(
    "Detailed Turing traces mark the head under the tape",
    "[presentation][turing][trace]"
) {
  const auto output = TraceTuring("a", turing::TraceView::kDetailed);
  CHECK(output.contains("#1  by δ1  (q, a) → (q, a, R)\n"));
  CHECK(output.contains("    tape    │ a │ _ │\n"));
  CHECK(output.contains("                  ▲\n"));
  CHECK(
      output.contains("    accept  no transition reads (_) in final state f\n")
  );
}

TEST_CASE(
    "PRF traces count calls or draw the call tree",
    "[presentation][prf]"
) {
  const auto power = prf::CreatePowerFunction();
  REQUIRE(power.has_value());
  SECTION("Counts put the busiest function first and end with the total") {
    prf::Trace trace(prf::Trace::Mode::kCounts);
    REQUIRE(
        (*power)->Evaluate(std::vector<std::uint64_t>{2, 3}, trace).has_value()
    );
    std::ostringstream out;
    prf::WriteTrace(out, trace, kPlain);
    CHECK(out.str().contains("   23  P_2^3\n"));
    CHECK(out.str().ends_with("   91  total\n"));
  }
  SECTION("The tree starts below the top-level call") {
    prf::Trace trace(prf::Trace::Mode::kTree);
    REQUIRE(
        (*power)->Evaluate(std::vector<std::uint64_t>{1, 1}, trace).has_value()
    );
    std::ostringstream out;
    prf::WriteTrace(out, trace, kPlain);
    CHECK(out.str().starts_with("├─ C(S∘[Z^1])(1) = 1\n"));
    CHECK_FALSE(out.str().contains("pow("));
  }
}

TEST_CASE(
    "Plain styles never emit escapes and keep glyphs",
    "[presentation][style]"
) {
  CHECK(kPlain.Paint(terminal::Tone::kBad, "reject") == "reject");
  CHECK(kPlain.glyphs().epsilon == "ε");
  CHECK(terminal::PadLeft("7", 3) == "  7");
  CHECK(terminal::PadRight("ε", 3) == "ε  ");
  CHECK(terminal::Truncate("abcdef", 4, "…") == "abc…");
  CHECK(terminal::TruncateMiddle("abcdefgh", 5, "…") == "ab…gh");
  CHECK(terminal::TruncateMiddle("abc", 5, "…") == "abc");
}

turing::Definition EmptyTuring(std::string_view start) {
  return {
      .config = {},
      .num_tapes = 1,
      .input_alphabet = {},
      .start_state = Intern(start),
      .blank = Intern("_"),
      .accepting_states = {},
      .transitions = {}
  };
}

TEST_CASE(
    "Graphviz escapes state names in DOT output",
    "[presentation][graphviz]"
) {
  CHECK(turing::ToDot(EmptyTuring("q\"\\")).contains("\"q\\\"\\\\\""));
}

TEST_CASE(
    "Graphviz writes DOT and reports output failures",
    "[presentation][graphviz]"
) {
  const auto definition = EmptyTuring("q");
  const TemporaryDirectory directory;
  SECTION("The output contains the generated DOT") {
    const auto path = directory.Path() / "machine.dot";
    REQUIRE(turing::WriteDot(definition, path).has_value());
    CHECK(test_support::ReadFile(path) == turing::ToDot(definition));
  }
  SECTION("A directory cannot be opened as an output file") {
    CHECK_FALSE(turing::WriteDot(definition, directory.Path()).has_value());
  }
}

TEST_CASE(
    "Graphviz reports missing and failing executables",
    "[presentation][graphviz]"
) {
  const auto definition = EmptyTuring("q");
  const TemporaryDirectory directory;
  const auto path = directory.Path() / "machine.png";
  SECTION("The executable cannot be started") {
    CHECK_FALSE(
        turing::RenderGraph(definition, path, "/missing/graphviz").has_value()
    );
  }
  SECTION("The executable returns failure") {
    CHECK_FALSE(
        turing::RenderGraph(definition, path, "/usr/bin/false").has_value()
    );
  }
}

/// A replacement for the `dot` program of Graphviz. It examines its arguments
/// and copies the DOT input to the output path.
constexpr std::string_view kFakeDot = R"(#!/bin/sh
[ "$#" -eq 6 ] || exit 2
[ "$1" = "-Tpng" ] || exit 3
[ "$3" = "-Goverlap=false" ] || exit 4
[ "$4" = "-Gmodel=subset" ] || exit 5
[ "$5" = "-o" ] || exit 6
cat "$2" > "$6"
)";

TEST_CASE(
    "Graphviz passes quoted paths as single process arguments",
    "[presentation][graphviz]"
) {
  const auto definition = EmptyTuring("q");
  const TemporaryDirectory directory;
  const auto executable = directory.Path() / "fake graph 'viz'";
  test_support::WriteFile(executable, kFakeDot);
  std::filesystem::permissions(executable, std::filesystem::perms::owner_all);
  const auto path = directory.Path() / "machine 'image'.png";
  REQUIRE(
      turing::RenderGraph(definition, path, executable.string()).has_value()
  );
  CHECK(test_support::ReadFile(path) == turing::ToDot(definition));
}

}  // namespace
