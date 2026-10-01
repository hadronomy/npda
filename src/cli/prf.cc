#include <chrono>
#include <cstdint>
#include <format>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "prf/function.h"
#include "prf/trace.h"
#include "prf/trace_render.h"
#include "terminal/ansi.h"
#include "terminal/ui.h"

namespace cli {

namespace {

struct PrfOptions {
  std::vector<std::uint64_t> params;
  prf::Trace::Mode mode = prf::Trace::Mode::kCountsOnly;
};

int RunPrf(const PrfOptions& options, const GlobalOptions&) {
  auto power = prf::CreatePowerFunction();
  if (!power) {
    ui::Error(power.error().message);
    return 1;
  }
  prf::Trace trace(options.mode);

  {
    std::vector<std::uint64_t> args = options.params;
    const auto t0 = std::chrono::steady_clock::now();
    auto result = (*power)->Evaluate(args, trace);
    if (!result) {
      ui::Error(result.error().message);
      return 1;
    }
    const auto r = *result;
    const double secs =
        std::chrono::duration<double>(std::chrono::steady_clock::now() - t0)
            .count();
    std::cout << ui::Rail(std::format("pow({})", prf::JoinArguments(args)))
              << "\n";
    std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen),
                              "PASS [{:7.3f}s] pow({}) = {}", secs,
                              prf::JoinArguments(args), r)
              << "\n";
    prf::RenderTrace(trace, std::cout);
    std::cout << ui::Rule("result: DONE") << "\n";
    std::cout << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen),
                              "{} done in {:.3f}s ({} calls)\n", "✓", secs,
                              trace.TotalCalls());
    std::cout << "\n";
  }

  return 0;
}

}  // namespace

void RegisterPrf(CLI::App& app, const GlobalOptions& options, int& exit_code) {
  auto& sub =
      *app.add_subcommand("prf", "execute primitive recursive functions");
  sub.fallthrough(false);
  sub.allow_extras(false);

  auto command_options = std::make_shared<PrfOptions>();
  sub.add_option("params", command_options->params,
                 "the prf parameters for execution")
      ->expected(2, 2)
      ->required();
  const std::map<std::string, prf::Trace::Mode> mode_map{
      {"off", prf::Trace::Mode::kOff},
      {"full", prf::Trace::Mode::kFull},
      {"counts-only", prf::Trace::Mode::kCountsOnly},
  };

  sub.add_option("--mode", command_options->mode, "Trace mode")
      ->transform(CLI::CheckedTransformer(mode_map, CLI::ignore_case)
                      .description("off|full|counts-only"));
  sub.callback([command_options, &options, &exit_code] {
    exit_code = RunPrf(*command_options, options);
  });
}

}  // namespace cli
