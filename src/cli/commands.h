#ifndef NPDA_SRC_CLI_COMMANDS_H_
#define NPDA_SRC_CLI_COMMANDS_H_

#include <format>
#include <string>
#include <utility>
#include <vector>

#include <CLI/App.hpp>
#include <CLI/Validators.hpp>

namespace cli {

/// Adds a subcommand whose long options start with one dash, as in the course
/// statement: `-config`, `-trace`.
///
/// The help page shows `examples` below EXAMPLES, one on each line. Add the
/// options of the command after this call.
CLI::App& AddCommand(
    CLI::App& app,
    std::string name,
    std::string description,
    std::vector<std::string> examples
);

/// Returns the words of `choices` in the form of the help page: `a|b|c`.
template <typename T>
[[nodiscard]] std::string Words(
    const std::vector<std::pair<std::string, T>>& choices
) {
  std::string words;
  for (const auto& [word, value] : choices) {
    words += (words.empty() ? "" : "|") + word;
  }
  return words;
}

/// Converts a word in `choices` to its value, or returns an error that lists
/// the accepted words.
template <typename T>
[[nodiscard]] CLI::Validator OneOf(
    std::vector<std::pair<std::string, T>> choices
) {
  return CLI::Validator(
      [choices, words = Words(choices)](std::string& text) -> std::string {
        for (const auto& [word, value] : choices) {
          if (text != word) continue;
          text = std::to_string(static_cast<long long>(value));
          return {};
        }
        return std::format("expected {}, not '{}'", words, text);
      },
      ""
  );
}

/// Each command stores its exit status in `exit_code` when it runs.
void RegisterNpda(CLI::App& app, int& exit_code);
void RegisterTuring(CLI::App& app, int& exit_code);
void RegisterPrf(CLI::App& app, int& exit_code);
void RegisterExplain(CLI::App& app, int& exit_code);

[[nodiscard]] int Main(int argc, char** argv);

}  // namespace cli

#endif  // NPDA_SRC_CLI_COMMANDS_H_
