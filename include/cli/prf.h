#pragma once

#include <CLI/CLI.hpp>

#include "../prf.h"
#include "cli.h"

class PRFHandler final : public CommandHandler {
 public:
  std::vector<uint64_t> params;
  prf::Trace::Mode mode = prf::Trace::Mode::CountsOnly;

  int operator()(const CommandContext& ctx) override;
};

[[maybe_unused]] static std::unique_ptr<CommandHandler> make_prf(CLI::App& sub) {
  auto handler = std::make_unique<PRFHandler>();
  sub.add_option("params", handler->params, "the prf parameters for execution")
    ->expected(2, 2)
    ->required();
  const std::map<std::string, prf::Trace::Mode> mode_map{
    {"off", prf::Trace::Mode::Off},
    {"full", prf::Trace::Mode::Full},
    {"counts-only", prf::Trace::Mode::CountsOnly},
  };

  sub.add_option("--mode", handler->mode, "Trace mode")
    ->transform(
      CLI::CheckedTransformer(mode_map, CLI::ignore_case).description("off|full|counts-only")
    );
  return handler;
}