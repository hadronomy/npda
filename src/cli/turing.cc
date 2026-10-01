#include <chrono>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
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
#include "terminal/activity.h"
#include "terminal/ansi.h"
#include "terminal/ui.h"
#include "turing/graphviz.h"
#include "turing/machine.h"
#include "turing/parser.h"
#include "turing/trace.h"

namespace cli {

namespace {

struct TuringOptions {
  std::filesystem::path file_path;
  std::vector<std::string> input_strings;
  bool trace_enabled = false;
  bool explain = false;
  std::size_t trace_limit = 200;
  bool graphviz = false;
  std::string graphviz_exe = "dot";
  bool dot_only = false;
};

int RunTuring(const TuringOptions& options, const GlobalOptions& ctx) {
  std::filesystem::path filepath = options.file_path;
  std::ifstream file(filepath);

  diag::RenderOptions opt;
  opt.verbose = ctx.verbose;
  terminal::Activity act(std::string("Checking ") +
                         filepath.filename().string());

  diag::SourceCache cache;
  auto result = turing::parse::ParseWithDiagnostics(file, filepath.string());
  const diag::SourceFile& src = cache.Insert(std::move(result.source));

  // Show warnings/errors if any
  if (!result.warnings.items.empty()) {
    act.Dismiss();
    diag::Render(std::cerr, cache, src.filename, result.warnings, opt);
  }

  if (!result.value.has_value()) {
    act.Dismiss();
    diag::Render(std::cerr, cache, src.filename, result.value.error(), opt);
    return 1;
  }

  if (auto& tm = result.value; result.value.has_value()) {
    if (options.graphviz) {
      act.Dismiss();
      auto new_path =
          options.file_path.filename().stem().replace_extension("png");
      ui::Info(std::format("Writting graphviz image in {}", new_path.c_str()));
      auto exe = options.graphviz_exe;
      if (options.dot_only) {
        auto res = turing::WriteGraphvizDot(tm->definition(),
                                            new_path.replace_extension("dot"));
        if (!res) {
          ui::Error(res.error().message);
          return -1;
        }
        return 0;
      }
      auto res = turing::ExportGraphvizImage(tm->definition(), new_path, exe);
      if (!res) {
        ui::Error(res.error().message);
        return -1;
      }
      return 0;
    }

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
      act.Resume();
      turing::TraceOptions trace_options{.enabled = trace,
                                         .colors = true,
                                         .explanations = options.explain,
                                         .step_limit = options.trace_limit};
      turing::RunOptions run_options{.max_steps = 100000,
                                     .track_witness = true};
      if (trace)
        run_options.observer = [&](const turing::ExecutionEvent& event) {
          turing::RenderTrace(tm->definition(), event, trace_options);
        };
      auto r = tm->Run(automata::InputSymbols(s), run_options);
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

      // Colorize the result line. PASS means the run worked;
      // the accepted word carries the verdict color.
      const auto verdict =
          r->accepted ? ansi::TerminalColor::kGreen : ansi::TerminalColor::kRed;
      if (r->accepted)
        ++n_accepted;
      else
        ++n_rejected;
      std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen),
                                "PASS [{:7.3f}s] ", secs)
                << ansi::Format(ansi::Fg(verdict), "{} {} accepted={} steps={}",
                                input_display, ui::Arrow(), r->accepted,
                                r->steps)
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

      // Show final tape configuration. Huge tapes print as a summary
      // so one run cannot flood the terminal.
      if (!r->final_tapes.empty() && !r->final_tapes[0].empty()) {
        std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kCyan),
                                  " tape=\"");

        const auto& tape = r->final_tapes[0];
        std::size_t head_pos = r->final_head_positions[0];

        if (tape.size() > 200) {
          std::string plain;
          for (std::size_t i = 0; i < tape.size(); ++i) {
            if (i == head_pos) plain += "[";
            plain += tape[i];
            if (i == head_pos) plain += "]";
          }
          if (head_pos >= tape.size()) plain += "[ ]";
          std::cout << ui::TruncateMiddle(plain, 80)
                    << "\" cells=" << tape.size();
        } else {
          for (std::size_t i = 0; i < tape.size(); ++i) {
            if (i == head_pos) {
              std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kYellow),
                                        "[{}]", tape[i]);
            } else {
              std::cout << tape[i];
            }
          }

          // Show head position if it's beyond the current tape
          if (head_pos >= tape.size()) {
            std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kYellow),
                                      "[ ]");
          }

          std::cout << "\"";
        }
      }

      std::cout << "\n";
      // Verdict close. Counts make the run machine-readable.
      std::cout << ui::Rule(std::string("result: ") +
                            (r->accepted ? "ACCEPT" : "REJECT"))
                << "\n";
      if (r->accepted) {
        if (r->witness) {
          std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen),
                                    "{} accept in {} steps (witness {})\n", "✓",
                                    r->steps, r->witness->size());
        } else {
          std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen),
                                    "{} accept in {} steps\n", "✓", r->steps);
        }
      } else {
        std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kRed),
                                  "{} reject after {} steps\n", "✗", r->steps);
      }
      std::cout << "\n\n";
      act.Resume();
    };

    act.SetMessage(std::string("Running ") + filepath.filename().string());
    act.Suspend();
    turing::ShowConfiguration(tm->definition());
    act.Resume();
    for (std::size_t idx = 0; idx < options.input_strings.size(); ++idx) {
      run(options.input_strings[idx], options.trace_enabled, idx + 1,
          options.input_strings.size());
    }
    std::cout << std::format(
        "Summary: {} inputs run: {} accepted, {} rejected, {} errors in "
        "{:.3f}s\n",
        options.input_strings.size(), n_accepted, n_rejected, n_errors,
        total_secs);
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

void RegisterTuring(CLI::App& app, const GlobalOptions& options,
                    int& exit_code) {
  auto& sub = *app.add_subcommand(
      "turing", "execute a given Turing Machine with a given string");
  sub.fallthrough(false);
  sub.allow_extras(false);

  auto command_options = std::make_shared<TuringOptions>();
  auto grp = sub.add_option_group("mode");
  sub.add_option("file_path", command_options->file_path,
                 "the Turing Machine description file path")
      ->required()
      ->check(CLI::ExistingFile);
  grp->add_option("input_string", command_options->input_strings,
                  "the string to process")
      ->multi_option_policy(CLI::MultiOptionPolicy::TakeAll);
  sub.add_flag("--trace,!--no-trace", command_options->trace_enabled,
               "Enable trace mode");
  sub.add_flag("--explain", command_options->explain,
               "Enable explanations of transitions");
  sub.add_option("--trace-limit", command_options->trace_limit,
                 "Max trace steps and rows")
      ->check(CLI::PositiveNumber);

  grp->add_flag("--graphviz,-g", command_options->graphviz,
                "Export graphviz image");
  sub.add_option("--exe,-e", command_options->graphviz_exe,
                 "The graphviz executable to use")
      ->needs("--graphviz");
  sub.add_flag("--dot-only", command_options->dot_only,
               "Whether to only output the graphviz file and no image")
      ->needs("--graphviz");
  grp->require_option(1, 1);
  sub.callback([command_options, &options, &exit_code] {
    exit_code = RunTuring(*command_options, options);
  });
}

}  // namespace cli
