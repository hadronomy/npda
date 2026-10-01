#include <chrono>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <CLI/CLI.hpp>

#include "automata/symbol.h"
#include "cli/commands.h"
#include "diagnostics/render.h"
#include "diagnostics/source.h"
#include "npda/machine.h"
#include "npda/parser.h"
#include "npda/trace.h"
#include "terminal/activity.h"
#include "terminal/ansi.h"
#include "terminal/ui.h"

namespace cli {

namespace {

struct NpdaOptions {
  std::filesystem::path file_path;
  std::vector<std::string> input_strings;
  bool trace_enabled = false;
  bool explain = false;
  std::size_t trace_limit = 200;
  bool trace_box = false;
  bool trace_tree = false;
  std::filesystem::path input_file;
  std::filesystem::path output_file;
};

int RunNpda(const NpdaOptions& options, const GlobalOptions& ctx) {
  std::filesystem::path filepath = options.file_path;
  std::ifstream file(filepath);

  diag::RenderOptions opt;
  opt.verbose = ctx.verbose;
  terminal::Activity act(std::string("Checking ") +
                         filepath.filename().string());

  diag::SourceCache cache;
  auto result = npda::parse::ParseWithDiagnostics(file, filepath.string());
  const diag::SourceFile& src = cache.Insert(std::move(result.source));

  // Input strings: CLI args, then -in file lines, then keyboard.
  // Empty lines never count as inputs.
  std::vector<std::string> inputs = options.input_strings;
  if (!options.input_file.empty()) {
    std::ifstream inf(options.input_file);
    if (!inf) {
      ui::Error(std::format("cannot open input file '{}'",
                            options.input_file.string()));
      return 1;
    }
    std::string line;
    while (std::getline(inf, line)) {
      if (!line.empty() && line.back() == '\r') line.pop_back();
      if (!line.empty()) inputs.push_back(std::move(line));
    }
  }
  if (inputs.empty()) {
    std::string line;
    while (std::getline(std::cin, line)) {
      if (!line.empty() && line.back() == '\r') line.pop_back();
      if (!line.empty()) inputs.push_back(std::move(line));
    }
  }

  // Trace sink: -out file when given, screen otherwise.
  std::ofstream out;
  if (!options.output_file.empty() && options.trace_enabled) {
    out.open(options.output_file);
    if (!out) {
      ui::Error(std::format("cannot open output file '{}'",
                            options.output_file.string()));
      return 1;
    }
  }
  std::function<void(std::string_view)> sink;
  if (out.is_open()) sink = [&](std::string_view s) { out << s; };

  if (auto& dpa = result.value; result.value.has_value()) {
    bool failed = false;
    std::size_t n_accepted = 0;
    std::size_t n_rejected = 0;
    std::size_t n_errors = 0;
    double total_secs = 0.0;
    auto run = [&](std::string_view s, bool trace, std::size_t num,
                   std::size_t total) {
      const auto t0 = std::chrono::steady_clock::now();
      // Framing first so the trace below belongs to a named input.
      act.Suspend();
      std::cout << ui::Rail(std::format("Input {}/{} , {} : \"{}\"", num, total,
                                        filepath.filename().string(),
                                        ui::TruncateMiddle(s)))
                << "\n";
      std::cout << std::format(
          "config: accept={} start={} bottom={}\n",
          npda::AcceptanceName(dpa->definition().acceptance),
          std::format("{}", dpa->definition().start_state),
          std::format("{}", dpa->definition().stack_bottom));
      act.Resume();
      npda::TraceOptions trace_options{.enabled = trace,
                                       .sink = sink,
                                       .colors = true,
                                       .compact = false,
                                       .explanations = options.explain,
                                       .show_full_trace = true,
                                       .box = options.trace_box,
                                       .tree = options.trace_tree,
                                       .step_limit = options.trace_limit};
      npda::RunOptions run_options{
          .search_order = npda::SearchOrder::kBreadthFirst,
          .max_expansions = 100000,
          .track_witness = true};
      if (trace)
        run_options.observer = [&](const npda::ExecutionEvent& event) {
          npda::RenderTrace(dpa->definition(), event, trace_options);
        };
      auto r = dpa->Run(automata::InputSymbols(s), run_options);
      const double secs =
          std::chrono::duration<double>(std::chrono::steady_clock::now() - t0)
              .count();
      total_secs += secs;
      act.Suspend();
      if (!r) {
        std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kRed),
                                  "FAIL [{:7.3f}s] {} {} error: {}", secs,
                                  ui::TruncateMiddle(s), ui::Arrow(),
                                  ui::TruncateMiddle(r.error().message, 200))
                  << "\n";
        std::cout << "\n";
        ++n_errors;
        failed = true;
        act.Resume();
        return;
      }

      // Format input display - show empty string as "λ" (lambda) for clarity
      std::string input_display = s.empty() ? "λ" : ui::TruncateMiddle(s);

      // PASS means the run worked; the accepted word carries the verdict color.
      const auto verdict =
          r->accepted ? ansi::TerminalColor::kGreen : ansi::TerminalColor::kRed;
      if (r->accepted)
        ++n_accepted;
      else
        ++n_rejected;
      std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen),
                                "PASS [{:7.3f}s] ", secs)
                << ansi::Format(
                       ansi::Fg(verdict), "{} {} accepted={} expansions={}",
                       input_display, ui::Arrow(), r->accepted, r->expansions)
                << "\n";

      if (r->witness) {
        std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kCyan),
                                  " witness_rules=[");
        for (std::size_t i = 0; i < r->witness->size(); ++i) {
          std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kYellow),
                                    "{}", (*r->witness)[i]);
          if (i + 1 < r->witness->size()) {
            std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kCyan),
                                      ",");
          }
        }
        std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kCyan), "]");
      }
      std::cout << "\n";
      // Verdict close. Counts make the run machine-readable.
      std::cout << ui::Rule(std::string("result: ") +
                            (r->accepted ? "ACCEPT" : "REJECT"))
                << "\n";
      if (r->accepted) {
        std::cout << ansi::Format(
            ansi::Fg(ansi::TerminalColor::kGreen),
            "{} accept in {} steps ({} expansions, witness {}, depth {})\n",
            "✓", r->witness ? r->witness->size() : 0, r->expansions,
            r->witness ? r->witness->size() : 0, r->stack_depth);
      } else {
        std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kRed),
                                  "{} reject after {} expansions\n", "✗",
                                  r->expansions);
      }
      std::cout << "\n\n";
      act.Resume();
    };

    act.SetMessage(std::string("Running ") + filepath.filename().string());
    for (std::size_t idx = 0; idx < inputs.size(); ++idx) {
      run(inputs[idx], options.trace_enabled, idx + 1, inputs.size());
    }
    std::cout << std::format(
        "Summary: {} inputs run: {} accepted, {} rejected, {} errors in "
        "{:.3f}s\n",
        inputs.size(), n_accepted, n_rejected, n_errors, total_secs);
    if (failed)
      act.Fail(std::string("Failed ") + filepath.filename().string());
    else
      act.Finish(std::string("Finished ") + filepath.filename().string());
    return 0;
  }
  if (!result.value) {
    act.Dismiss();
    diag::Render(std::cerr, cache, src.filename, result.value.error(), opt);
    return 1;
  }
  act.Dismiss();
  return 0;
}

}  // namespace

void RegisterNpda(CLI::App& app, const GlobalOptions& options, int& exit_code) {
  auto& sub =
      *app.add_subcommand("npda", "execute a given NPDA with a given string");
  sub.fallthrough(false);
  sub.allow_extras(false);

  auto command_options = std::make_shared<NpdaOptions>();
  sub.add_option("file_path", command_options->file_path,
                 "the NPDA description file path")
      ->required()
      ->check(CLI::ExistingFile);
  sub.add_option("input_string", command_options->input_strings,
                 "the string to accept (omit with -in or stdin)")
      ->multi_option_policy(CLI::MultiOptionPolicy::TakeAll);
  sub.add_option("--in", command_options->input_file,
                 "file with input strings, one per line")
      ->check(CLI::ExistingFile);
  sub.add_option("--out", command_options->output_file,
                 "file where the trace is stored");
  sub.add_flag("--trace,!--no-trace", command_options->trace_enabled,
               "Disable trace mode");
  sub.add_flag("--explain", command_options->explain,
               "Enable explanations of the transitions");
  sub.add_flag("--trace-box", command_options->trace_box,
               "Show the stack box table in traces");
  sub.add_flag("--trace-tree", command_options->trace_tree,
               "Show the exploration tree on accept");
  sub.add_option("--trace-limit", command_options->trace_limit,
                 "Max trace steps and rows")
      ->check(CLI::PositiveNumber);
  sub.callback([command_options, &options, &exit_code] {
    exit_code = RunNpda(*command_options, options);
  });
}

}  // namespace cli
