#include <format>
#include <iostream>
#include <print>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "cli/help.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

std::string Join(const std::vector<std::string>& lines) {
  std::string text;
  for (const auto& line : lines) {
    if (!text.empty()) text += '\n';
    text += line;
  }
  return text;
}

/// The error message, and the help page to read for the command that failed.
std::string FailureMessage(const CLI::App* app, const CLI::Error& error) {
  const auto style = terminal::Style::For(terminal::OutputStream::kStderr);
  std::string path = app->get_name();
  for (auto used = app->get_subcommands(); !used.empty();
       used = used.front()->get_subcommands()) {
    path += " " + used.front()->get_name();
  }
  return std::format(
      "{}: {}\n{}\n",
      style.Paint(terminal::Tone::kBad, "error"),
      error.what(),
      style.Paint(
          terminal::Tone::kMuted,
          std::format("Run '{} -help' for usage.", path)
      )
  );
}

}  // namespace

void ReportError(std::string_view message) {
  const auto style = terminal::Style::For(terminal::OutputStream::kStderr);
  std::println(
      std::cerr,
      "{}: {}",
      style.Paint(terminal::Tone::kBad, "error"),
      message
  );
}

CLI::App& AddCommand(
    CLI::App& app,
    std::string name,
    std::string description,
    std::vector<std::string> examples
) {
  auto& command = *app.add_subcommand(std::move(name), std::move(description));
  command.allow_non_standard_option_names();
  command.set_help_flag("-help,--help", "Show this help");
  command.footer(Join(examples));
  return command;
}

int Main(int argc, char** argv) {
  CLI::App app("Compute primitive recursive functions.", "cc");
  app.allow_non_standard_option_names();
  app.set_help_flag();
  app.formatter(MakeHelpFormatter());
  app.failure_message(FailureMessage);
  app.require_subcommand(1);
  int exit_code = 0;
  RegisterPrf(app, exit_code);
  app.set_help_flag("-help,--help", "Show this help");
  app.footer(Join({
      "cc prf -x 2 -y 3",
      "cc prf -x 2 -y 3 -trace tree",
  }));
  if (argc == 1) {
    std::cout << app.help();
    return 0;
  }
  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& error) {
    // CLI11 assigns different statuses to usage errors, but the program uses
    // 1 for every error so scripts need to check only one error status.
    return app.exit(error) == 0 ? 0 : 1;
  }
  return exit_code;
}

}  // namespace cli
