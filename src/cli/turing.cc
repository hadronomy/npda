#include <cstddef>
#include <expected>
#include <filesystem>
#include <format>
#include <memory>
#include <optional>
#include <print>
#include <string>

#include <CLI/CLI.hpp>

#include "automata/symbol.h"
#include "cli/commands.h"
#include "cli/machine_command.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"
#include "turing/definition.h"
#include "turing/graphviz.h"
#include "turing/machine.h"
#include "turing/parser.h"
#include "turing/run.h"
#include "turing/trace.h"

namespace cli {

namespace {

using terminal::Style;
using terminal::Tone;

constexpr std::size_t kMaxSteps = 100'000;

struct TuringOptions {
  MachineOptions<turing::TraceView> machine;
  std::filesystem::path graph;
};

std::expected<void, turing::Error> ExportGraph(
    const turing::Definition& definition,
    const std::filesystem::path& path
) {
  if (path.extension() == ".dot") return turing::WriteDot(definition, path);
  return turing::RenderGraph(definition, path);
}

/// The result on the first tape, as required by the course, and the number of
/// steps.
std::string Detail(
    const std::expected<turing::RunResult, turing::LimitReached>& result,
    automata::Symbol blank,
    const Style& style
) {
  if (!result) {
    return style.Paint(
        Tone::kMuted,
        std::format("still running after {} steps", result.error().steps)
    );
  }
  return std::format(
      "{}  {}",
      turing::FormatTape(result->halted.tapes.front(), blank, style),
      style.Paint(
          Tone::kMuted,
          std::format(
              "{} {}",
              result->steps,
              result->steps == 1 ? "step" : "steps"
          )
      )
  );
}

Verdict VerdictOf(
    const std::expected<turing::RunResult, turing::LimitReached>& result
) {
  if (!result) return Verdict::kUnknown;
  return result->accepted ? Verdict::kAccept : Verdict::kReject;
}

int RunTuring(const TuringOptions& options) {
  const auto& machine_options = options.machine;
  const auto source = ReadSource(machine_options.config);
  if (!source) {
    ReportError(source.error());
    return 1;
  }
  const auto parsed = turing::Parse(*source);
  ReportDiagnostics(*source, parsed.diagnostics, machine_options.verbose);
  if (!parsed.machine) return 1;
  const auto& machine = *parsed.machine;
  const auto& definition = machine.definition();
  const auto style = Style::For(terminal::OutputStream::kStdout);

  std::println(
      "{}{}{}",
      style.Paint(Tone::kStrong, machine_options.config.filename().string()),
      style.Separator(),
      turing::Describe(definition, style)
  );
  if (!options.graph.empty()) {
    if (auto exported = ExportGraph(definition, options.graph); !exported) {
      ReportError(exported.error().message);
      return 1;
    }
    std::println(
        "{} {}",
        style.Paint(Tone::kMuted, "graph"),
        options.graph.string()
    );
  }

  const auto inputs = CollectInputs(
      machine_options.inputs,
      machine_options.in,
      /*read_keyboard=*/options.graph.empty()
  );
  if (!inputs) {
    ReportError(inputs.error());
    return 1;
  }
  if (inputs->empty()) return 0;
  auto trace =
      TraceOutput::Open(machine_options.trace ? machine_options.out : "");
  if (!trace) {
    ReportError(trace.error());
    return 1;
  }
  if (machine_options.trace || machine_options.explain) {
    turing::WriteTransitions(
        trace->Stream(),
        definition,
        trace->style(),
        machine_options.explain
    );
  }
  if (!machine_options.trace) std::println();

  const turing::TraceOptions trace_options{
      .view = machine_options.view,
      .limit = machine_options.trace_limit,
      .explain = machine_options.explain,
  };
  const auto width = InputColumnWidth(*inputs);
  Tally tally;
  for (const auto& text : *inputs) {
    const auto input = automata::InputSymbols(text);
    // An input with a symbol outside Σ is not a word of the machine, so the
    // machine does not run on it.
    const auto foreign = machine.FindForeignSymbol(input);
    std::optional<std::expected<turing::RunResult, turing::LimitReached>>
        result;
    if (!foreign) {
      result = machine_options.trace
                   ? turing::TraceRun(
                         machine,
                         input,
                         kMaxSteps,
                         trace_options,
                         trace->Stream(),
                         trace->style()
                     )
                   : machine.Run(input, {.max_steps = kMaxSteps});
    }
    const auto verdict = result ? VerdictOf(*result) : Verdict::kReject;
    const auto line = [&](const Style& line_style) {
      const auto detail = result ? Detail(*result, definition.blank, line_style)
                                 : line_style.Paint(
                                       Tone::kMuted,
                                       std::format("{} is not in Σ", *foreign)
                                   );
      return FormatVerdict(verdict, text, width, detail, line_style);
    };
    std::println("{}", line(style));
    if (trace->IsFile()) {
      std::println(trace->Stream(), "{}", line(trace->style()));
    }
    tally.Add(verdict);
  }
  std::println("\n{}", tally.Summary(style));
  return tally.AllDecided() ? 0 : 1;
}

}  // namespace

void RegisterTuring(CLI::App& app, int& exit_code) {
  auto& command = AddCommand(
      app,
      "turing",
      "Run a Turing machine on input strings, or draw it",
      {
          "cc turing -config examples/turing/anbm.turing aabb abb",
          "cc turing -config examples/turing/anbm.turing -trace-view compact "
          "aabb",
          "cc turing -config examples/turing/anbm.turing -graph anbm.svg",
      }
  );
  auto options = std::make_shared<TuringOptions>();
  AddMachineOptions<turing::TraceView>(
      command,
      options->machine,
      {
          {"detailed", turing::TraceView::kDetailed},
          {"compact", turing::TraceView::kCompact},
      }
  );
  command
      .add_option(
          "-graph",
          options->graph,
          "Draw the machine: .dot for Graphviz source, .svg or .png to render"
      )
      ->type_name("FILE");
  command.callback([options, &exit_code] { exit_code = RunTuring(*options); });
}

}  // namespace cli
