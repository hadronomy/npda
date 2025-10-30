#pragma once

#include <CLI/CLI.hpp>

#include "cli.h"

class PRFHandler final : public CommandHandler {
 public:
  std::vector<uint64_t> params;

  int operator()(const CommandContext& ctx) override;
};

[[maybe_unused]] static std::unique_ptr<CommandHandler> make_prf(CLI::App& sub) {
  auto handler = std::make_unique<PRFHandler>();
  sub.add_option("params", handler->params, "the prf parameters for execution")
    ->expected(2, 2)
    ->required();
  return handler;
}