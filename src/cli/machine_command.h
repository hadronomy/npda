#ifndef NPDA_SRC_CLI_MACHINE_COMMAND_H_
#define NPDA_SRC_CLI_MACHINE_COMMAND_H_

#include <cstddef>
#include <expected>
#include <filesystem>
#include <fstream>
#include <ostream>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <CLI/App.hpp>
#include <CLI/Validators.hpp>

#include "cli/commands.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "terminal/style.h"

/// The parts that the npda and turing commands share: the course statement's
/// options, input sources, trace destination, and format of verdicts and
/// summaries.
namespace cli {

/// The options of a machine command, with `View` supplying its trace views.
template <typename View>
struct MachineOptions {
  std::filesystem::path config;
  bool trace = true;
  View view = View::kDetailed;
  std::size_t trace_limit = 200;
  bool explain = false;
  std::filesystem::path in;
  std::filesystem::path out;
  bool verbose = false;
  std::vector<std::string> inputs;
};

/// Adds `-config`, `-trace`, `-trace-view`, `-trace-limit`, `-explain`, `-in`,
/// `-out`, `-verbose`, and the input strings in the order shown on the help
/// page.
template <typename View>
void AddMachineOptions(
    CLI::App& command,
    MachineOptions<View>& options,
    std::vector<std::pair<std::string, View>> views
) {
  const auto existing_file = CLI::ExistingFile.description("");
  command.add_option("-config", options.config, "Machine definition file")
      ->required()
      ->check(existing_file)
      ->type_name("FILE");
  command
      .add_option(
          "-trace",
          options.trace,
          "Show how the machine runs each input"
      )
      ->transform(OneOf<bool>({{"y", true}, {"n", false}}))
      ->type_name("y|n")
      ->default_str("y");
  const auto default_view = views.front().first;
  const auto view_names = Words(views);
  command.add_option("-trace-view", options.view, "How the trace looks")
      ->transform(OneOf<View>(std::move(views)))
      ->type_name(view_names)
      ->default_str(default_view);
  command
      .add_option(
          "-trace-limit",
          options.trace_limit,
          "Configurations a trace shows per input"
      )
      ->check(CLI::PositiveNumber.description(""))
      ->type_name("N")
      ->capture_default_str();
  command.add_flag(
      "-explain",
      options.explain,
      "Explain every transition in words"
  );
  command
      .add_option(
          "-in",
          options.in,
          "File with one input on each line. A line with only '.' is the empty "
          "string"
      )
      ->check(existing_file)
      ->type_name("FILE");
  command
      .add_option(
          "-out",
          options.out,
          "Write the trace to this file, not the screen"
      )
      ->type_name("FILE");
  command.add_flag(
      "-verbose",
      options.verbose,
      "Show every repeated diagnostic instead of a summary"
  );
  command.add_option(
      "inputs",
      options.inputs,
      "Strings to run. Without them and -in, the program reads the keyboard"
  );
}

/// Writes `error: <message>` to standard error.
void ReportError(std::string_view message);

/// Reads all of a machine file.
///
/// # Errors
///
/// A message with the file name if the file cannot be read.
[[nodiscard]] std::expected<diag::SourceFile, std::string> ReadSource(
    const std::filesystem::path& path
);

/// Writes the diagnostics to standard error, or nothing if there are none.
void ReportDiagnostics(
    const diag::SourceFile& source,
    const diag::DiagnosticSet& diagnostics,
    bool verbose
);

/// Returns the inputs from the arguments, then from the `in` file.
///
/// Files and the keyboard both skip blank lines and treat `.` alone as the
/// empty string, as in machine files. If neither the arguments nor the file
/// gives an input and `read_keyboard` is true, the function reads the keyboard.
///
/// # Errors
///
/// A message naming the `in` file when it cannot be read.
[[nodiscard]] std::expected<std::vector<std::string>, std::string>
CollectInputs(
    std::span<const std::string> arguments,
    const std::filesystem::path& in,
    bool read_keyboard
);

/// The destination of the trace: the `-out` file if there is one, or else
/// standard output.
class TraceOutput {
 public:
  /// Opens `path`, or uses standard output if `path` is empty.
  ///
  /// # Errors
  ///
  /// A message with the file name if the file cannot be written.
  [[nodiscard]] static std::expected<TraceOutput, std::string> Open(
      const std::filesystem::path& path
  );

  [[nodiscard]] std::ostream& Stream();

  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const terminal::Style& style() const { return style_; }

  [[nodiscard]] bool IsFile() const { return file_.is_open(); }

 private:
  TraceOutput(std::ofstream file, terminal::Style style);

  std::ofstream file_;
  terminal::Style style_;
};

enum class Verdict { kAccept, kReject, kUnknown };

/// Returns one aligned result line: `accept  aabb  δ1 δ2 δ3`.
///
/// The input gets spaces up to `input_width` columns, so that the details
/// align.
[[nodiscard]] std::string FormatVerdict(
    Verdict verdict,
    std::string_view input,
    std::size_t input_width,
    std::string_view detail,
    const terminal::Style& style
);

/// Returns the width of the input column that `FormatVerdict` needs for
/// `inputs`.
[[nodiscard]] std::size_t InputColumnWidth(std::span<const std::string> inputs);

class Tally {
 public:
  void Add(Verdict verdict);

  /// Returns `3 inputs · 2 accepted · 1 rejected`. If some inputs have no
  /// verdict, the text also gives their number.
  [[nodiscard]] std::string Summary(const terminal::Style& style) const;

  [[nodiscard]] bool AllDecided() const { return unknown_ == 0; }

 private:
  std::size_t accepted_ = 0;
  std::size_t rejected_ = 0;
  std::size_t unknown_ = 0;
};

}  // namespace cli

#endif  // NPDA_SRC_CLI_MACHINE_COMMAND_H_
