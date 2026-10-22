#include <filesystem>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include <catch2/catch_test_macros.hpp>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/render.h"
#include "diagnostics/source.h"
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

TEST_CASE(
    "Diagnostic rendering uses source labels without ANSI in plain text",
    "[presentation][diagnostics]"
) {
  auto source = diag::SourceFile::From("test.turing", "q0 qf\na\n");
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
  CHECK(output.str().contains("test.turing"));
  CHECK(output.str().contains("use a declared state"));
  CHECK(!output.str().contains('\x1b'));
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
