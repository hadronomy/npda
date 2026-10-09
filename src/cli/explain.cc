#include <cctype>
#include <format>
#include <memory>
#include <print>
#include <string>

#include <CLI/CLI.hpp>

#include "cli/commands.h"
#include "cli/machine_command.h"
#include "diagnostics/diagnostic.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

int RunExplain(std::string code) {
  for (auto& character : code) {
    character =
        static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
  }
  const auto entries = diag::Explain(code);
  if (entries.empty()) {
    ReportError(std::format("unknown error code '{}'", code));
    return 1;
  }
  const auto style = terminal::Style::For(terminal::OutputStream::kStdout);
  for (const auto& entry : entries) {
    std::println(
        "{} {} {}",
        style.Paint(terminal::Tone::kStrong, entry.code),
        style.Paint(terminal::Tone::kMuted, std::format("[{}]", entry.machine)),
        entry.title
    );
    std::println("  {}", entry.hint);
  }
  return 0;
}

}  // namespace

void RegisterExplain(CLI::App& app, int& exit_code) {
  auto& command = AddCommand(
      app,
      "explain",
      "Say what an error code means and how to fix it",
      {"cc explain E0007"}
  );
  auto code = std::make_shared<std::string>();
  command.add_option("code", *code, "Error code, such as E0007")->required();
  command.callback([code, &exit_code] { exit_code = RunExplain(*code); });
}

}  // namespace cli
