#include "cli/machine_command.h"

#include <unistd.h>

#include <algorithm>
#include <cstddef>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <istream>
#include <iterator>
#include <ostream>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "diagnostics/diagnostic.h"
#include "diagnostics/render.h"
#include "diagnostics/source.h"
#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

using terminal::Style;
using terminal::Tone;

/// If an input is wider than this, the output keeps its two ends and puts an
/// ellipsis between them.
constexpr std::size_t kMaxInputColumns = 32;

void ReadLines(std::istream& stream, std::vector<std::string>& inputs) {
  for (std::string line; std::getline(stream, line);) {
    if (line.ends_with('\r')) line.pop_back();
    if (line.empty()) continue;
    inputs.push_back(line == "." ? std::string() : std::move(line));
  }
}

std::string DisplayInput(std::string_view input, const Style& style) {
  if (input.empty()) return std::string(style.glyphs().epsilon);
  return terminal::TruncateMiddle(
      input,
      kMaxInputColumns,
      style.glyphs().ellipsis
  );
}

}  // namespace

void ReportError(std::string_view message) {
  const auto style = Style::For(terminal::OutputStream::kStderr);
  std::println(std::cerr, "{}: {}", style.Paint(Tone::kBad, "error"), message);
}

std::expected<diag::SourceFile, std::string> ReadSource(
    const std::filesystem::path& path
) {
  std::ifstream file(path, std::ios::binary);
  if (!file)
    return std::unexpected(std::format("cannot read '{}'", path.string()));
  std::string text{
      std::istreambuf_iterator<char>(file),
      std::istreambuf_iterator<char>()
  };
  return diag::SourceFile::From(path.string(), std::move(text));
}

void ReportDiagnostics(
    const diag::SourceFile& source,
    const diag::DiagnosticSet& diagnostics,
    bool verbose
) {
  if (diagnostics.items.empty()) return;
  diag::Render(std::cerr, source, diagnostics, {.verbose = verbose});
}

std::expected<std::vector<std::string>, std::string> CollectInputs(
    std::span<const std::string> arguments,
    const std::filesystem::path& in,
    bool read_keyboard
) {
  std::vector<std::string> inputs;
  for (const auto& input : arguments) {
    inputs.push_back(input == "." ? std::string() : input);
  }
  if (!in.empty()) {
    std::ifstream file(in);
    if (!file) {
      return std::unexpected(std::format("cannot read '{}'", in.string()));
    }
    ReadLines(file, inputs);
  }
  if (inputs.empty() && read_keyboard) {
    if (isatty(STDIN_FILENO) != 0) {
      const auto style = Style::For(terminal::OutputStream::kStderr);
      std::println(
          std::cerr,
          "{}",
          style.Paint(
              Tone::kMuted,
              "Type one input on each line, or '.' for the empty string. "
              "Then push Ctrl-D."
          )
      );
    }
    ReadLines(std::cin, inputs);
  }
  return inputs;
}

TraceOutput::TraceOutput(std::ofstream file, terminal::Style style)
    : file_(std::move(file)), style_(style) {}

std::expected<TraceOutput, std::string> TraceOutput::Open(
    const std::filesystem::path& path
) {
  if (path.empty()) {
    return TraceOutput({}, Style::For(terminal::OutputStream::kStdout));
  }
  std::ofstream file(path);
  if (!file) {
    return std::unexpected(std::format("cannot write '{}'", path.string()));
  }
  return TraceOutput(std::move(file), Style::Plain());
}

std::ostream& TraceOutput::Stream() {
  if (file_.is_open()) return file_;
  return std::cout;
}

std::string FormatVerdict(
    Verdict verdict,
    std::string_view input,
    std::size_t input_width,
    std::string_view detail,
    const Style& style
) {
  std::string word;
  switch (verdict) {
    case Verdict::kAccept:
      word = style.Paint(Tone::kGood, "accept") + " ";
      break;
    case Verdict::kReject:
      word = style.Paint(Tone::kBad, "reject") + " ";
      break;
    case Verdict::kUnknown:
      word = style.Paint(Tone::kWarn, "unknown");
      break;
  }
  const auto shown = DisplayInput(input, style);
  if (detail.empty()) return std::format("{} {}", word, shown);
  return std::format(
      "{} {}  {}",
      word,
      terminal::PadRight(shown, input_width),
      detail
  );
}

std::size_t InputColumnWidth(std::span<const std::string> inputs) {
  std::size_t width = 1;
  for (const auto& input : inputs) {
    width = std::max(
        width,
        std::min(terminal::DisplayWidth(input), kMaxInputColumns)
    );
  }
  return width;
}

void Tally::Add(Verdict verdict) {
  switch (verdict) {
    case Verdict::kAccept:
      ++accepted_;
      return;
    case Verdict::kReject:
      ++rejected_;
      return;
    case Verdict::kUnknown:
      ++unknown_;
      return;
  }
}

std::string Tally::Summary(const Style& style) const {
  const auto separator = style.Separator();
  const auto total = accepted_ + rejected_ + unknown_;
  std::string text = std::format(
      "{} {}{}{} accepted{}{} rejected",
      style.Number(total),
      total == 1 ? "input" : "inputs",
      separator,
      style.Number(accepted_),
      separator,
      style.Number(rejected_)
  );
  if (unknown_ > 0) {
    text += std::format(
        "{}{}",
        separator,
        style.Paint(
            Tone::kWarn,
            std::format("{} undecided", style.Number(unknown_))
        )
    );
  }
  return text;
}

}  // namespace cli
