#include <filesystem>
#include <fstream>
#include <optional>
#include <sstream>
#include <string>

#include <catch2/catch_test_macros.hpp>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/render.h"
#include "diagnostics/source.h"
#include "npda/machine.h"
#include "npda/trace.h"
#include "support/files.h"
#include "terminal/ansi.h"
#include "turing/graphviz.h"
#include "turing/machine.h"
#include "turing/trace.h"

namespace {

using test_support::TemporaryDirectory;

TEST_CASE("Diagnostic rendering uses source labels without ANSI in plain text",
          "[presentation][diagnostics]") {
  auto source = diag::SourceFile::From("test.npda", "q0 qf\na\n");
  diag::DiagnosticSet diagnostics;
  diag::Error("E0007", "unknown state")
      .Label({0, 2}, "use a declared state")
      .Note("check the Q line")
      .Emit(diagnostics);
  std::ostringstream output;
  diag::Render(
      output, source, diagnostics,
      {.color = diag::ColorMode::kNever, .charset = diag::CharSet::kAscii});
  CHECK(output.str().contains("E0007"));
  CHECK(output.str().contains("test.npda"));
  CHECK(output.str().contains("use a declared state"));
  CHECK(!output.str().contains('\x1b'));
}

TEST_CASE("NPDA trace rendering caps paths and reports hidden rows",
          "[presentation][trace]") {
  const auto start = automata::Intern("q");
  const auto end = automata::Intern("f");
  const auto symbol = automata::Intern("a");
  const auto bottom = automata::Intern("Z");
  auto machine = npda::Machine::Create(
      {.start_state = start,
       .stack_bottom = bottom,
       .accepting_states = {end},
       .transitions = {{start, symbol, bottom, start, {bottom}},
                       {start, std::nullopt, bottom, end, {}}}});
  REQUIRE(machine.has_value());
  std::string output;
  npda::TraceOptions format{
      .enabled = true,
      .sink = [&](std::string_view text) { output += text; },
      .colors = false,
      .show_full_trace = false,
      .step_limit = 1};
  auto result =
      machine->Run(automata::InputSymbols("aaaaaaaa"),
                   {.observer = [&](const npda::ExecutionEvent& event) {
                     npda::RenderTrace(machine->definition(), event, format);
                   }});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(output.contains("Accepting path found"));
  CHECK(output.contains("hidden"));
  CHECK_FALSE(output.contains('\x1b'));
}

TEST_CASE("Turing trace rendering caps paths and reports hidden rows",
          "[presentation][trace]") {
  const auto start = automata::Intern("q");
  const auto end = automata::Intern("f");
  const auto symbol = automata::Intern("a");
  const auto bottom = automata::Intern("Z");
  turing::Definition definition{
      .config = {},
      .start_state = start,
      .blank_symbol = bottom,
      .accepting_states = {end},
      .transitions = {
          {start, {{symbol, symbol, turing::Direction::kRight}}, start},
          {start, {{bottom, bottom, turing::Direction::kStay}}, end}}};
  auto turing_machine = turing::Machine::Create(definition);
  REQUIRE(turing_machine.has_value());
  std::string output;
  turing::TraceOptions turing_format{
      .enabled = true,
      .sink = [&](std::string_view text) { output += text; },
      .colors = false,
      .show_full_trace = false,
      .step_limit = 1};
  auto turing_result = turing_machine->Run(
      automata::InputSymbols("aaaaaaaa"),
      {.observer = [&](const turing::ExecutionEvent& event) {
        turing::RenderTrace(definition, event, turing_format);
      }});
  REQUIRE(turing_result.has_value());
  CHECK(turing_result->accepted);
  CHECK(output.contains("accepted"));
  CHECK(output.contains("hidden"));
}

TEST_CASE("Graphviz escapes state names in DOT output",
          "[presentation][graphviz]") {
  const turing::Definition definition{.config = {},
                                      .start_state = automata::Intern("q\"\\"),
                                      .blank_symbol = automata::Intern("_"),
                                      .accepting_states = {},
                                      .transitions = {}};
  CHECK(turing::ToGraphvizDot(definition).contains("q\\\"\\\\"));
}

TEST_CASE("Graphviz writes DOT and reports output failures",
          "[presentation][graphviz]") {
  const turing::Definition definition{.config = {},
                                      .start_state = automata::Intern("q"),
                                      .blank_symbol = automata::Intern("_"),
                                      .accepting_states = {},
                                      .transitions = {}};
  const TemporaryDirectory directory;
  SECTION("The output contains the generated DOT") {
    const auto path = directory.Path() / "machine.dot";
    REQUIRE(turing::WriteGraphvizDot(definition, path).has_value());
    std::ifstream file(path);
    REQUIRE(file.is_open());
    std::ostringstream content;
    content << file.rdbuf();
    CHECK(content.str() == turing::ToGraphvizDot(definition));
  }
  SECTION("A directory cannot be opened as an output file") {
    CHECK_FALSE(
        turing::WriteGraphvizDot(definition, directory.Path()).has_value());
  }
}

TEST_CASE("Graphviz reports missing and failing executables",
          "[presentation][graphviz]") {
  const turing::Definition definition{.config = {},
                                      .start_state = automata::Intern("q"),
                                      .blank_symbol = automata::Intern("_"),
                                      .accepting_states = {},
                                      .transitions = {}};
  const TemporaryDirectory directory;
  const auto path = directory.Path() / "machine.png";
  SECTION("The executable cannot be started") {
    CHECK_FALSE(
        turing::ExportGraphvizImage(definition, path, "/missing/graphviz")
            .has_value());
  }
  SECTION("The executable returns failure") {
    CHECK_FALSE(turing::ExportGraphvizImage(definition, path, "/usr/bin/false")
                    .has_value());
  }
}

TEST_CASE("Graphviz passes quoted paths as single process arguments",
          "[presentation][graphviz]") {
  const turing::Definition definition{.config = {},
                                      .start_state = automata::Intern("q"),
                                      .blank_symbol = automata::Intern("_"),
                                      .accepting_states = {},
                                      .transitions = {}};
  const TemporaryDirectory directory;
  const auto executable = directory.Path() / "fake graph 'viz'";
  std::ofstream script(executable);
  REQUIRE(script.is_open());
  // The fixture validates argv and copies the temporary DOT to the output path.
  script << R"(#!/bin/sh
[ "$#" -eq 6 ] || exit 2
[ "$1" = "-Tpng" ] || exit 3
[ "$3" = "-Goverlap=false" ] || exit 4
[ "$4" = "-Gmodel=subset" ] || exit 5
[ "$5" = "-o" ] || exit 6
cat "$2" > "$6"
)";
  script.close();
  std::filesystem::permissions(executable, std::filesystem::perms::owner_all);
  const auto path = directory.Path() / "machine 'image'.png";
  REQUIRE(turing::ExportGraphvizImage(definition, path, executable.string())
              .has_value());
  std::ifstream output(path);
  REQUIRE(output.is_open());
  std::ostringstream content;
  content << output.rdbuf();
  CHECK(content.str() == turing::ToGraphvizDot(definition));
}

TEST_CASE("ANSI styles encode terminal colors and emphasis",
          "[presentation][ansi]") {
  CHECK(ansi::Open({}).empty());
  CHECK(ansi::Open(ansi::Fg(ansi::TerminalColor::kRed)) == "\x1b[31m");
  CHECK(ansi::Open(ansi::Fg(ansi::Rgb{1, 2, 3})) == "\x1b[38;2;001;002;003m");
  CHECK(ansi::Open(ansi::TextStyle{.em = ansi::Emphasis::kBold |
                                         ansi::Emphasis::kUnderline}) ==
        "\x1b[1;4m");
}

}  // namespace
