#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "diagnostics/diagnostic.h"
#include "terminal/ui.h"

namespace cli {

namespace {

struct ExplainOptions {
  std::string code;
};

int RunExplain(const ExplainOptions& options, const GlobalOptions&) {
  const auto entries = diag::Explain(options.code);
  if (entries.empty()) {
    ui::Error("unknown error code '" + options.code + "'");
    return 1;
  }
  for (const auto& e : entries) {
    std::cout << e.code << " [" << e.machine << "]: " << e.title << '\n'
              << e.hint << '\n';
  }
  return 0;
}

}  // namespace

void RegisterExplain(CLI::App& app, const GlobalOptions& options,
                     int& exit_code) {
  auto& sub = *app.add_subcommand(
      "explain", "show what an error code means and how to fix it");
  sub.fallthrough(false);
  sub.allow_extras(false);

  auto command_options = std::make_shared<ExplainOptions>();
  sub.add_option("code", command_options->code,
                 "the error code to explain (e.g. E0007)")
      ->required();
  sub.callback([command_options, &options, &exit_code] {
    exit_code = RunExplain(*command_options, options);
  });
}

}  // namespace cli
