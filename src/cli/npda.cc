#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <memory>
#include <print>
#include <span>
#include <string>

#include <CLI/CLI.hpp>

#include "automata/symbol.h"
#include "cli/commands.h"
#include "cli/machine_command.h"
#include "npda/machine.h"
#include "npda/parser.h"
#include "npda/run.h"
#include "npda/trace.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

using terminal::Style;
using terminal::Tone;

using NpdaOptions = MachineOptions<npda::TraceView>;

/// This limit is much larger than the course examples need. It is also small
/// enough that a machine with an ε-loop stops in less than one second.
constexpr std::size_t kMaxExpansions = 100'000;

/// If an accepting path is longer than this, the line ends with a count.
constexpr std::size_t kMaxPathShown = 24;

std::string Detail(
    const std::expected<npda::RunResult, npda::LimitReached>& result,
    const Style& style
) {
  if (!result) {
    return style.Paint(
        Tone::kMuted,
        std::format(
            "stopped after {} expansions",
            style.Number(result.error().expansions)
        )
    );
  }
  if (!result->Accepted()) return {};
  const auto& path = *result->accepting_path;
  if (path.empty()) return style.Paint(Tone::kMuted, "no moves");
  auto text = npda::FormatPath(
      std::span(path).first(std::min(path.size(), kMaxPathShown)),
      style
  );
  if (path.size() > kMaxPathShown) {
    text += style.Paint(
        Tone::kMuted,
        std::format(
            " {} +{}",
            style.glyphs().ellipsis,
            path.size() - kMaxPathShown
        )
    );
  }
  return text;
}

Verdict VerdictOf(
    const std::expected<npda::RunResult, npda::LimitReached>& result
) {
  if (!result) return Verdict::kUnknown;
  return result->Accepted() ? Verdict::kAccept : Verdict::kReject;
}

int RunNpda(const NpdaOptions& options) {
  const auto source = ReadSource(options.config);
  if (!source) {
    ReportError(source.error());
    return 1;
  }
  const auto parsed = npda::Parse(*source);
  ReportDiagnostics(*source, parsed.diagnostics, options.verbose);
  if (!parsed.machine) return 1;
  const auto inputs =
      CollectInputs(options.inputs, options.in, /*read_keyboard=*/true);
  if (!inputs) {
    ReportError(inputs.error());
    return 1;
  }
  auto trace = TraceOutput::Open(options.trace ? options.out : "");
  if (!trace) {
    ReportError(trace.error());
    return 1;
  }

  const auto& machine = *parsed.machine;
  const auto& definition = machine.definition();
  const auto style = Style::For(terminal::OutputStream::kStdout);
  std::println(
      "{}{}{}",
      style.Paint(Tone::kStrong, options.config.filename().string()),
      style.Separator(),
      npda::Describe(definition, style)
  );
  if (options.trace || options.explain) {
    npda::WriteTransitions(
        trace->Stream(),
        definition,
        trace->style(),
        options.explain
    );
  }
  if (!options.trace) std::println();

  const npda::TraceOptions trace_options{
      .view = options.view,
      .limit = options.trace_limit,
      .explain = options.explain,
  };
  const auto width = InputColumnWidth(*inputs);
  Tally tally;
  for (const auto& text : *inputs) {
    const auto input = automata::InputSymbols(text);
    const auto result =
        options.trace ? npda::TraceRun(
                            machine,
                            input,
                            kMaxExpansions,
                            trace_options,
                            trace->Stream(),
                            trace->style()
                        )
                      : machine.Run(input, {.max_expansions = kMaxExpansions});
    const auto verdict = VerdictOf(result);
    std::println(
        "{}",
        FormatVerdict(verdict, text, width, Detail(result, style), style)
    );
    if (trace->IsFile()) {
      const auto& file_style = trace->style();
      std::println(
          trace->Stream(),
          "{}",
          FormatVerdict(
              verdict,
              text,
              width,
              Detail(result, file_style),
              file_style
          )
      );
    }
    tally.Add(verdict);
  }
  std::println("\n{}", tally.Summary(style));
  return tally.AllDecided() ? 0 : 1;
}

}  // namespace

void RegisterNpda(CLI::App& app, int& exit_code) {
  auto& command = AddCommand(
      app,
      "npda",
      "Run a pushdown automaton (APf or APv) on input strings",
      {
          "cc npda -config examples/APf/APf-1.txt aabb aab",
          "cc npda -config examples/APv/APv-2.txt -trace-view tree 0110",
          "cc npda -config examples/APf/APf-1.txt -in inputs.txt -out "
          "trace.txt",
          "cc npda -config examples/APf/APf-1.txt -trace n -in inputs.txt",
      }
  );
  auto options = std::make_shared<NpdaOptions>();
  AddMachineOptions<npda::TraceView>(
      command,
      *options,
      {
          {"detailed", npda::TraceView::kDetailed},
          {"compact", npda::TraceView::kCompact},
          {"tree", npda::TraceView::kTree},
      }
  );
  command.callback([options, &exit_code] { exit_code = RunNpda(*options); });
}

}  // namespace cli
