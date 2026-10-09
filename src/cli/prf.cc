#include <array>
#include <cstdint>
#include <iostream>
#include <memory>
#include <print>
#include <string>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "cli/machine_command.h"
#include "prf/function.h"
#include "prf/trace.h"
#include "prf/trace_render.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

struct PrfOptions {
  std::array<std::uint64_t, 2> arguments{};
  prf::Trace::Mode trace = prf::Trace::Mode::kCounts;
};

int RunPrf(const PrfOptions& options) {
  const auto power = prf::CreatePowerFunction();
  if (!power) {
    ReportError(power.error().message);
    return 1;
  }
  prf::Trace trace(options.trace);
  const auto result = (*power)->Evaluate(options.arguments, trace);
  if (!result) {
    ReportError(result.error().message);
    return 1;
  }
  const auto style = terminal::Style::For(terminal::OutputStream::kStdout);
  std::println(
      "pow({}) = {}",
      prf::JoinArguments(options.arguments),
      style.Paint(terminal::Tone::kStrong, std::to_string(*result))
  );
  prf::WriteTrace(std::cout, trace, style);
  return 0;
}

}  // namespace

void RegisterPrf(CLI::App& app, int& exit_code) {
  auto& command = AddCommand(
      app,
      "prf",
      "Compute base^exponent as a primitive recursive function",
      {"cc prf 2 3", "cc prf 1 2 -trace tree"}
  );
  auto options = std::make_shared<PrfOptions>();
  command.add_option("base", options->arguments[0], "Base")->required();
  command.add_option("exponent", options->arguments[1], "Exponent")->required();
  command
      .add_option(
          "-trace",
          options->trace,
          "What to show: the calls of each function, a tree of all calls, or "
          "nothing"
      )
      ->transform(
          OneOf<prf::Trace::Mode>(
              {{"counts", prf::Trace::Mode::kCounts},
               {"tree", prf::Trace::Mode::kTree},
               {"off", prf::Trace::Mode::kOff}}
          )
      )
      ->type_name("counts|tree|off")
      ->default_str("counts");
  command.callback([options, &exit_code] { exit_code = RunPrf(*options); });
}

}  // namespace cli
