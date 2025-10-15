#pragma once

#include <filesystem>

#include <CLI/CLI.hpp>

#include "cli.h"
#include "turing/rule.h"

class TuringHandler final : public CommandHandler {
 public:
  std::filesystem::path file_path;
  std::vector<std::string> input_strings;
  bool trace_enabled = false;
  bool explain = false;

  // Configuration options
  std::size_t num_tapes = 1;
  turing::TapeDirection tape_direction = turing::TapeDirection::Bidirectional;
  turing::OperationMode operation_mode = turing::OperationMode::Simultaneous;
  bool allow_stay = true;

  int operator()(const CommandContext& ctx) override;
};

[[maybe_unused]] static std::unique_ptr<CommandHandler> make_turing(CLI::App& sub) {
  auto handler = std::make_unique<TuringHandler>();
  sub.add_option("file_path", handler->file_path, "the Turing Machine description file path")
    ->required()
    ->check(CLI::ExistingFile);
  sub.add_option("input_string", handler->input_strings, "the string to process")
    ->multi_option_policy(CLI::MultiOptionPolicy::TakeAll)
    ->required();
  sub.add_flag("--trace,!--no-trace", handler->trace_enabled, "Enable trace mode");
  sub.add_flag("--explain", handler->explain, "Enable explanations of transitions");

  // Configuration options
  sub.add_option("--num-tapes", handler->num_tapes, "Number of tapes (default: 1)")
    ->check(CLI::PositiveNumber);
  sub.add_option(
    "--tape-direction", handler->tape_direction, "Tape direction: bidirectional or right-only"
  );
  sub.add_option(
    "--operation-mode", handler->operation_mode, "Operation mode: Simultaneous or Independent"
  );
  sub.add_flag(
    "--allow-stay,!--no-stay", handler->allow_stay, "Allow stay movement (S) or only L/R"
  );

  return handler;
}