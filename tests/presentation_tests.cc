#include <cstddef>
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
#include "terminal/style.h"

namespace {

using automata::Intern;

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

}  // namespace
