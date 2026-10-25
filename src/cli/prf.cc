#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <limits>
#include <memory>
#include <print>
#include <string>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "cli/machine_command.h"
#include "prf/function.h"
#include "prf/program.h"
#include "prf/trace.h"
#include "prf/trace_render.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

struct PrfOptions {
  prf::Natural base = 0;
  prf::Natural exponent = 0;
  prf::TraceMode trace = prf::TraceMode::kCounts;
  bool definition = false;
  std::size_t trace_limit = 200;
  /// A call takes about 2 ns, so a run stops after about 2 s at this limit.
  /// Without a limit, powers such as 2⁶³ take centuries because they need
  /// about 10¹⁹ calls, even though their results fit in 64 bits.
  std::uint64_t max_calls = 1'000'000'000;
};

/// Rejects powers that do not fit in 64 bits before evaluation, because the
/// successor needs about 2⁶⁴ calls to reach an overflow during evaluation.
bool PowerFits(prf::Natural base, prf::Natural exponent) {
  if (base <= 1) return true;
  prf::Natural power = 1;
  for (prf::Natural step = 0; step < exponent; ++step) {
    if (power > std::numeric_limits<prf::Natural>::max() / base) return false;
    power *= base;
  }
  return true;
}

int RunPrf(const PrfOptions& options) {
  const auto errors = terminal::Style::For(terminal::OutputStream::kStderr);
  const auto call = std::format(
      "pow({}, {})",
      errors.Number(options.base),
      errors.Number(options.exponent)
  );
  if (!PowerFits(options.base, options.exponent)) {
    ReportError(std::format("{} does not fit in 64 bits", call));
    return 1;
  }
  const prf::Program power(prf::Power());
  const std::array arguments{options.base, options.exponent};
  // Because the result line shows the top-level call, the tree must keep one
  // more call than it shows.
  prf::Trace trace({
      .mode = options.trace,
      .max_calls = options.max_calls,
      .tree_limit = options.trace_limit + 1,
  });
  const auto result = power.Evaluate(arguments, trace);
  if (!result) {
    // PowerFits rules out an overflow, so the call limit ended the evaluation.
    ReportError(
        std::format(
            "{} needs more than {} calls. Increase -max-calls to let it run "
            "longer.",
            call,
            errors.Number(options.max_calls)
        )
    );
    return 1;
  }
  const auto style = terminal::Style::For(terminal::OutputStream::kStdout);
  if (options.definition) {
    prf::WriteDefinition(std::cout, power.function(), style);
    std::println();
  }
  std::println(
      "{}",
      prf::FormatCall(power.function(), arguments, *result, style)
  );
  std::println(
      "{} function call{}",
      style.Paint(terminal::Tone::kStrong, style.Number(trace.Calls())),
      trace.Calls() == 1 ? "" : "s"
  );
  prf::WriteTrace(std::cout, trace, style);
  return 0;
}

}  // namespace

void RegisterPrf(CLI::App& app, int& exit_code) {
  auto& command = AddCommand(
      app,
      "prf",
      "Compute x^y as a primitive recursive function",
      {"cc prf -x 2 -y 3",
       "cc prf -x 2 -y 3 -definition -trace off",
       "cc prf -x 2 -y 2 -trace tree"}
  );
  auto options = std::make_shared<PrfOptions>();
  command.add_option("-x", options->base, "The base, a natural number")
      ->required()
      ->type_name("N");
  command.add_option("-y", options->exponent, "The exponent, a natural number")
      ->required()
      ->type_name("N");
  command.add_flag(
      "-definition",
      options->definition,
      "Show the equations that define pow before the result"
  );
  command
      .add_option(
          "-trace",
          options->trace,
          "What to show below the result: the calls of each function, a tree "
          "of the calls, or nothing"
      )
      ->transform(
          OneOf<prf::TraceMode>(
              {{"counts", prf::TraceMode::kCounts},
               {"tree", prf::TraceMode::kTree},
               {"off", prf::TraceMode::kOff}}
          )
      )
      ->type_name("counts|tree|off")
      ->default_str("counts");
  command
      .add_option("-trace-limit", options->trace_limit, "Calls the tree shows")
      ->check(CLI::PositiveNumber.description(""))
      ->type_name("N")
      ->capture_default_str();
  command
      .add_option(
          "-max-calls",
          options->max_calls,
          "Calls after which the evaluation stops with an error"
      )
      ->check(CLI::PositiveNumber.description(""))
      ->type_name("N")
      ->capture_default_str();
  command.callback([options, &exit_code] { exit_code = RunPrf(*options); });
}

}  // namespace cli
