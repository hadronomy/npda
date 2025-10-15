#pragma once

#include <filesystem>

#include <CLI/CLI.hpp>

#include "cli.h"

class RunHandler final : public CommandHandler {
 public:
  std::filesystem::path file_path;
  std::vector<std::string> input_strings;
  bool trace_enabled;
  bool explain;

  int operator()(const CommandContext& ctx) override;
};

[[maybe_unused]] static std::unique_ptr<CommandHandler> make_npda(CLI::App& sub) {
  auto handler = std::make_unique<RunHandler>();
  sub.add_option("file_path", handler->file_path, "the NPDA description file path")
    ->required()
    ->check(CLI::ExistingFile);
  sub.add_option("input_string", handler->input_strings, "the string to accept")
    ->multi_option_policy(CLI::MultiOptionPolicy::TakeAll)
    ->required();
  sub.add_flag("--trace,!--no-trace", handler->trace_enabled, "Disable trace mode");
  sub.add_flag("--explain", handler->explain, "Enable explanations of the transitions");
  return handler;
}