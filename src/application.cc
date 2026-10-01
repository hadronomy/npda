#include "application.h"

#include <format>
#include <iostream>
#include <string>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "cli/help_formatter.h"
#include "terminal/ansi.h"
#include "terminal/ui.h"

namespace cli {

int RunApplication(int argc, char** argv) {
  CLI::App app(
      "a simple cli for running a NPDA or turing machine and see all the "
      "traces",
      "cc");
  cli::GlobalOptions options;
  int exit_code = 0;
  app.require_subcommand(1);
  app.set_help_all_flag("--help-all", "Show help for all subcommands");
  app.allow_extras(false);
  app.formatter(cli::MakeHelpFormatter());
  app.add_flag("-v,--verbose", options.verbose, "Enable verbose output");
  cli::RegisterNpda(app, options, exit_code);
  cli::RegisterTuring(app, options, exit_code);
  cli::RegisterPrf(app, options, exit_code);
  cli::RegisterExplain(app, options, exit_code);
  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& error) {
    if (error.get_name() == "RuntimeError") return 1;
    if (error.get_name() == "CallForHelp") {
      std::cout << app.help();
      return 0;
    }
    if (error.get_name() == "CallForAllHelp") {
      std::cout << app.help("", CLI::AppFormatMode::All);
      return 0;
    }
    if (error.get_name() == "CallForVersion") {
      std::cout << error.what() << '\n';
      return error.get_exit_code();
    }
    ui::Error(std::format(
        "{}\n\x1b[0mRun with {} to see more information\n", error.what(),
        ansi::Format(ansi::Fg(ansi::TerminalColor::kCyan), "--help")));
    return 1;
  }
  return exit_code;
}

}  // namespace cli
