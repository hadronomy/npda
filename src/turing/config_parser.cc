#include "turing/config_parser.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "turing/config.h"

namespace turing {

namespace {

constexpr std::string_view kOpening = "# /// config";
constexpr std::string_view kClosing = "# ///";

/// A config key and the information needed to read its value.
struct Key {
  std::string_view name;
  /// Stores `value` in `config` if it is valid, or returns false otherwise.
  bool (*apply)(std::string_view value, Config& config);
  std::string_view code;
  std::string_view label;
  std::string_view note;
};

std::string_view Trim(std::string_view text) {
  const auto begin = text.find_first_not_of(" \t");
  if (begin == std::string_view::npos) return text.substr(text.size());
  return text.substr(begin, text.find_last_not_of(" \t") - begin + 1);
}

std::string_view Unquote(std::string_view text) {
  const bool quoted = text.size() >= 2 && text.front() == text.back() &&
                      (text.front() == '"' || text.front() == '\'');
  return quoted ? text.substr(1, text.size() - 2) : text;
}

bool ApplyTapeDirection(std::string_view value, Config& config) {
  value = Unquote(value);
  if (value == "bidirectional") {
    config.tape_direction = TapeDirection::kBidirectional;
  } else if (value == "right-only") {
    config.tape_direction = TapeDirection::kRightOnly;
  } else {
    return false;
  }
  return true;
}

bool ApplyOperationMode(std::string_view value, Config& config) {
  value = Unquote(value);
  if (value == "simultaneous") {
    config.operation_mode = OperationMode::kSimultaneous;
  } else if (value == "independent") {
    config.operation_mode = OperationMode::kIndependent;
  } else {
    return false;
  }
  return true;
}

bool ApplyAllowStay(std::string_view value, Config& config) {
  value = Unquote(value);
  if (value != "true" && value != "false") return false;
  config.allow_stay = value == "true";
  return true;
}

constexpr std::array kKeys{
    Key{
        .name = "tape_direction",
        .apply = ApplyTapeDirection,
        .code = "C0009",
        .label = "invalid option",
        .note = "Valid options are: 'bidirectional' or 'right-only'",
    },
    Key{
        .name = "operation_mode",
        .apply = ApplyOperationMode,
        .code = "C0010",
        .label = "invalid option",
        .note = "Valid options are: 'simultaneous' or 'independent'",
    },
    Key{
        .name = "allow_stay",
        .apply = ApplyAllowStay,
        .code = "C0011",
        .label = "invalid boolean value",
        .note = "Valid options are: 'true' or 'false'",
    },
};

std::size_t EditDistance(std::string_view from, std::string_view to) {
  std::vector<std::size_t> previous(to.size() + 1);
  std::vector<std::size_t> current(to.size() + 1);
  for (std::size_t column = 0; column <= to.size(); ++column) {
    previous[column] = column;
  }
  for (std::size_t row = 1; row <= from.size(); ++row) {
    current[0] = row;
    for (std::size_t column = 1; column <= to.size(); ++column) {
      const std::size_t substitution = from[row - 1] == to[column - 1] ? 0 : 1;
      current[column] = std::min({
          previous[column] + 1,
          current[column - 1] + 1,
          previous[column - 1] + substitution,
      });
    }
    std::swap(previous, current);
  }
  return previous[to.size()];
}

void ReportUnknownKey(
    std::string_view key,
    diag::SourceSpan span,
    diag::DiagnosticSet& diagnostics
) {
  auto warning = diag::Warning("C0012", "unknown configuration key");
  warning.Label(span, "unknown key");
  std::string close;
  std::string known;
  for (const auto& candidate : kKeys) {
    const auto quoted = std::format("'{}'", candidate.name);
    if (EditDistance(key, candidate.name) <= 3) {
      close += (close.empty() ? "" : ", ") + quoted;
    }
    known += (known.empty() ? "" : ", ") + std::string(candidate.name);
  }
  if (!close.empty()) warning.Note(std::format("Did you mean {}?", close));
  warning.Note("Known keys are: " + known);
  warning.Emit(diagnostics);
}

bool IsKeyCharacter(char character) {
  const auto byte = static_cast<unsigned char>(character);
  return std::isalnum(byte) != 0 || character == '_' || character == '-';
}

/// Reads one `key = value` line of the block into `result`.
void ReadEntry(
    const diag::SourceFile& source,
    std::string_view content,
    std::unordered_set<std::string_view>& seen,
    ConfigParseResult& result
) {
  auto& diagnostics = result.diagnostics;
  const auto equals = content.find('=');
  if (equals == std::string_view::npos) {
    diag::Error("C0002", "invalid configuration line: missing '='")
        .Label(source.SpanOf(content), "expected 'key = value' format")
        .Note("Configuration lines must follow the format: key = value")
        .Emit(diagnostics);
    return;
  }
  const auto key = Trim(content.substr(0, equals));
  const auto value = Trim(content.substr(equals + 1));
  if (key.empty()) {
    diag::Error("C0003", "empty configuration key")
        .Label(
            source.SpanOf(content.substr(0, equals)),
            "expected key before '='"
        )
        .Emit(diagnostics);
    return;
  }
  if (!std::ranges::all_of(key, IsKeyCharacter)) {
    diag::Error("C0004", "invalid configuration key")
        .Label(
            source.SpanOf(key),
            "keys may only contain letters, numbers, underscores, and hyphens"
        )
        .Emit(diagnostics);
    return;
  }
  if (!seen.insert(key).second) {
    diag::Error("C0005", "duplicate configuration key")
        .Label(source.SpanOf(key), "duplicate key")
        .Note(std::format("Configuration key '{}' was already defined", key))
        .Emit(diagnostics);
    return;
  }
  if (value.empty()) {
    diag::Error("C0006", "empty configuration value")
        .Label(
            source.SpanOf(content.substr(equals + 1)),
            "expected value after '='"
        )
        .Emit(diagnostics);
    return;
  }
  const auto* spec = std::ranges::find(kKeys, key, &Key::name);
  if (spec == kKeys.end()) {
    ReportUnknownKey(key, source.SpanOf(key), diagnostics);
    return;
  }
  if (!spec->apply(value, result.config)) {
    diag::Error(
        std::string(spec->code),
        std::format("invalid value for '{}'", spec->name)
    )
        .Label(source.SpanOf(value), std::string(spec->label))
        .Note(std::string(spec->note))
        .Emit(diagnostics);
  }
}

}  // namespace

ConfigParseResult ParseConfig(const diag::SourceFile& source) {
  ConfigParseResult result;
  std::optional<std::string_view> opening;
  std::unordered_set<std::string_view> seen;
  for (std::size_t number = 1; number <= source.LineCount(); ++number) {
    const auto line = Trim(source.Line(number));
    if (line.starts_with(kOpening)) {
      if (opening) {
        diag::Error("C0001", "nested configuration block")
            .Label(
                source.SpanOf(line.substr(0, kOpening.size())),
                "config block already started"
            )
            .Emit(result.diagnostics);
      } else {
        opening = line.substr(0, kOpening.size());
      }
      continue;
    }
    if (!opening) continue;
    if (line.starts_with(kClosing)) return result;
    if (!line.starts_with('#')) continue;
    const auto content = Trim(line.substr(1));
    if (!content.empty()) ReadEntry(source, content, seen, result);
  }
  if (opening) {
    diag::Error("C0013", "unclosed configuration block")
        .Label(source.SpanOf(*opening), "config block started here")
        .Note("Configuration block must be closed with '# ///'")
        .Emit(result.diagnostics);
  }
  return result;
}

}  // namespace turing
