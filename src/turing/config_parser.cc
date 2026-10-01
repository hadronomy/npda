#include "turing/config_parser.h"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <functional>
#include <initializer_list>
#include <limits>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_set>
#include <utility>
#include <vector>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "turing/machine.h"

namespace turing::parse {

// ---------- Helpers for config parsing ----------

// trim ASCII spaces and tabs only (config grammar uses them)
namespace {

[[nodiscard]] std::string_view TrimWhitespace(std::string_view s) {
  const auto b = s.find_first_not_of(" \t");
  if (b == std::string_view::npos) return {};
  const auto e = s.find_last_not_of(" \t");
  return s.substr(b, e - b + 1);
}

[[nodiscard]] std::string_view Unquote(std::string_view v) {
  if (v.size() >= 2) {
    const char a = v.front();
    const char b = v.back();
    if ((a == '"' && b == '"') || (a == '\'' && b == '\'')) {
      return v.substr(1, v.size() - 2);
    }
  }
  return v;
}

[[nodiscard]] bool ParsePositiveInteger(std::string_view v, std::size_t& out) {
  v = TrimWhitespace(v);
  if (v.empty()) return false;
  if (!std::all_of(v.begin(), v.end(),
                   [](unsigned char c) { return std::isdigit(c) != 0; })) {
    return false;
  }
  const auto* first = v.data();
  const auto* last = v.data() + v.size();
  unsigned long long ull = 0;
  auto ec = std::from_chars(first, last, ull).ec;
  if (ec != std::errc()) return false;
  if (ull == 0) return false;
  if (ull > std::numeric_limits<std::size_t>::max()) return false;
  out = static_cast<std::size_t>(ull);
  return true;
}

[[nodiscard]] bool ParseBoolLiteral(std::string_view v, bool& out) {
  v = TrimWhitespace(Unquote(v));
  if (v == "true") {
    out = true;
    return true;
  }
  if (v == "false") {
    out = false;
    return true;
  }
  return false;
}

// Generic enum parser: ensures v is exactly one of allowed values
[[nodiscard]] bool ParseEnumValue(
    std::string_view v, std::initializer_list<std::string_view> allowed,
    std::size_t& idx_out) {
  v = TrimWhitespace(Unquote(v));
  std::size_t i = 0;
  for (std::string_view a : allowed) {
    if (v == a) {
      idx_out = i;
      return true;
    }
    ++i;
  }
  return false;
}

// Bounded Levenshtein distance (UTF-8 as bytes, good enough for keys)
inline std::size_t LevenshteinDistanceBounded(std::string_view a,
                                              std::string_view b,
                                              std::size_t max_dist) {
  const std::size_t n = a.size();
  const std::size_t m = b.size();
  if (n == 0) return m;
  if (m == 0) return n;
  if (max_dist != std::numeric_limits<std::size_t>::max() &&
      (n > m ? n - m : m - n) > max_dist) {
    return max_dist + 1;
  }

  std::vector<std::size_t> prev(m + 1), curr(m + 1);
  for (std::size_t j = 0; j <= m; ++j) prev[j] = j;
  for (std::size_t i = 1; i <= n; ++i) {
    curr[0] = i;
    std::size_t row_min = curr[0];
    for (std::size_t j = 1; j <= m; ++j) {
      const std::size_t cost = (a[i - 1] == b[j - 1]) ? 0 : 1;
      curr[j] = std::min({prev[j] + 1, curr[j - 1] + 1, prev[j - 1] + cost});
      row_min = std::min(row_min, curr[j]);
    }
    if (max_dist != std::numeric_limits<std::size_t>::max() &&
        row_min > max_dist) {
      return max_dist + 1;
    }
    std::swap(prev, curr);
  }
  return prev[m];
}

// ---------- Declarative schema for config ----------

struct ConfigKeySpec {
  std::string name;
  std::string expected;
  std::vector<std::string> enum_values;
  std::function<bool(std::string_view, diag::SourceSpan, MachineConfig&,
                     diag::DiagnosticSet&)>
      parse_and_apply;
};

inline const std::vector<ConfigKeySpec>& ConfigurationSchema() {
  static const std::vector<ConfigKeySpec> kSchema = {
      ConfigKeySpec{
          "num_tapes",
          "positive integer",
          {},
          [](std::string_view v, diag::SourceSpan span, MachineConfig& cfg,
             diag::DiagnosticSet& dx) {
            std::size_t nt = 0;
            if (!ParsePositiveInteger(v, nt)) {
              diag::Error("C0007", "invalid value for 'num_tapes'")
                  .Label(span, "expected positive integer")
                  .Note("num_tapes must be a positive integer (e.g., 1, 2, 3)")
                  .Emit(dx);
              return false;
            }
            cfg.num_tapes = nt;
            return true;
          }},
      ConfigKeySpec{
          "tape_direction",
          "enum('bidirectional'|'right-only')",
          {"bidirectional", "right-only"},
          [](std::string_view v, diag::SourceSpan span, MachineConfig& cfg,
             diag::DiagnosticSet& dx) {
            std::size_t idx = std::numeric_limits<std::size_t>::max();
            if (!ParseEnumValue(v, {"bidirectional", "right-only"}, idx)) {
              diag::Error("C0009", "invalid value for 'tape_direction'")
                  .Label(span, "invalid option")
                  .Note("Valid options are: 'bidirectional' or 'right-only'")
                  .Emit(dx);
              return false;
            }
            cfg.tape_direction = (idx == 0) ? TapeDirection::kBidirectional
                                            : TapeDirection::kRightOnly;
            return true;
          }},
      ConfigKeySpec{
          "operation_mode",
          "enum('simultaneous'|'independent')",
          {"simultaneous", "independent"},
          [](std::string_view v, diag::SourceSpan span, MachineConfig& cfg,
             diag::DiagnosticSet& dx) {
            std::size_t idx = std::numeric_limits<std::size_t>::max();
            if (!ParseEnumValue(v, {"simultaneous", "independent"}, idx)) {
              diag::Error("C0010", "invalid value for 'operation_mode'")
                  .Label(span, "invalid option")
                  .Note("Valid options are: 'simultaneous' or 'independent'")
                  .Emit(dx);
              return false;
            }
            cfg.operation_mode = (idx == 0) ? OperationMode::kSimultaneous
                                            : OperationMode::kIndependent;
            return true;
          }},
      ConfigKeySpec{"allow_stay",
                    "boolean('true'|'false')",
                    {"true", "false"},
                    [](std::string_view v, diag::SourceSpan span,
                       MachineConfig& cfg, diag::DiagnosticSet& dx) {
                      bool b = false;
                      if (!ParseBoolLiteral(v, b)) {
                        diag::Error("C0011", "invalid value for 'allow_stay'")
                            .Label(span, "invalid boolean value")
                            .Note("Valid options are: 'true' or 'false'")
                            .Emit(dx);
                        return false;
                      }
                      cfg.allow_stay = b;
                      return true;
                    }},
  };
  return kSchema;
}

// For unknown keys: produce suggestions using bounded Levenshtein
[[nodiscard]] std::vector<std::string> SuggestKeys(
    std::string_view unknown, const std::vector<ConfigKeySpec>& schema,
    std::size_t max_distance = 3) {
  std::vector<std::string> out;
  for (const auto& s : schema) {
    const auto d = LevenshteinDistanceBounded(unknown, s.name, max_distance);
    if (d <= max_distance) out.push_back(s.name);
  }
  std::sort(out.begin(), out.end());
  out.erase(std::unique(out.begin(), out.end()), out.end());
  return out;
}

// Parse TOML-like configuration from structured comments with
// comprehensive diagnostics
// Format: # /// config\n# key = value\n# ///
[[nodiscard]] ConfigParseResult ParseStructuredConfigWithSchema(
    const diag::SourceFile& source, const std::vector<ConfigKeySpec>& schema) {
  ConfigParseResult result;
  bool in_config_block = false;
  std::size_t config_start_line = 0;

  // Track seen keys to detect duplicates
  std::unordered_set<std::string> seen_keys;

  for (std::size_t li = 1; li <= source.line_count(); ++li) {
    std::string_view line = source.line_view(li);
    const std::size_t line_start = source.line_starts[li - 1];

    // Trim leading whitespace for marker detection
    std::size_t start = line.find_first_not_of(" \t");
    if (start == std::string_view::npos) continue;
    std::string_view trimmed = line.substr(start);

    // Check for config block markers
    if (trimmed.starts_with("# /// config")) {
      if (in_config_block) {
        // Nested config block - error
        diag::Error("C0001", "nested configuration block")
            .Label(
                diag::SourceSpan{line_start + start, line_start + start + 14},
                "config block already started")
            .Emit(result.diagnostics);
      } else {
        in_config_block = true;
        config_start_line = li;
      }
      continue;
    }

    if (trimmed.starts_with("# ///") && in_config_block) {
      // End of config block
      in_config_block = false;
      break;
    }

    if (!in_config_block || !trimmed.starts_with("#")) continue;

    // Remove the # and any following whitespace
    std::string_view content = trimmed.substr(1);
    std::size_t content_start = content.find_first_not_of(" \t");
    if (content_start == std::string_view::npos) continue;
    content = content.substr(content_start);
    const std::size_t content_offset = line_start + start + 1 + content_start;

    // Skip empty lines or comment-only lines
    if (content.empty()) continue;

    // Parse key=value pairs with detailed error reporting
    std::size_t eq_pos = content.find('=');

    if (eq_pos == std::string_view::npos) {
      // No equals sign found
      diag::Error("C0002", "invalid configuration line: missing '='")
          .Label(diag::SourceSpan{content_offset,
                                  content_offset + content.length()},
                 "expected 'key = value' format")
          .Note("Configuration lines must follow the format: key = value")
          .Emit(result.diagnostics);
      continue;
    }

    // Extract and validate key
    std::string_view key = TrimWhitespace(content.substr(0, eq_pos));
    std::string_view value = TrimWhitespace(content.substr(eq_pos + 1));

    // Validate key
    if (key.empty()) {
      diag::Error("C0003", "empty configuration key")
          .Label(diag::SourceSpan{content_offset, content_offset + eq_pos},
                 "expected key before '='")
          .Emit(result.diagnostics);
      continue;
    }

    // Check for invalid characters in key
    bool valid_key = true;
    for (char c : key) {
      if (!std::isalnum(static_cast<unsigned char>(c)) && c != '_' &&
          c != '-') {
        valid_key = false;
        break;
      }
    }

    if (!valid_key) {
      diag::Error("C0004", "invalid configuration key")
          .Label(diag::SourceSpan{content_offset + key.find_first_not_of(" \t"),
                                  content_offset + key.find_last_not_of(" \t") +
                                      1},
                 "keys may only contain letters, numbers, underscores, and "
                 "hyphens")
          .Emit(result.diagnostics);
      continue;
    }

    // Check for duplicate keys
    std::string key_str(key);
    if (seen_keys.count(key_str)) {
      diag::Error("C0005", "duplicate configuration key")
          .Label(diag::SourceSpan{content_offset + key.find_first_not_of(" \t"),
                                  content_offset + key.find_last_not_of(" \t") +
                                      1},
                 "duplicate key")
          .Note("Configuration key '" + key_str + "' was already defined")
          .Emit(result.diagnostics);
      continue;
    }
    seen_keys.insert(key_str);

    // Validate value presence
    if (value.empty()) {
      diag::Error("C0006", "empty configuration value")
          .Label(diag::SourceSpan{content_offset + eq_pos + 1,
                                  content_offset + content.length()},
                 "expected value after '='")
          .Emit(result.diagnostics);
      continue;
    }

    // Parse and validate specific configuration options
    const std::size_t key_span_start =
        content_offset + key.find_first_not_of(" \t");
    const std::size_t key_span_end =
        content_offset + key.find_last_not_of(" \t") + 1;
    const std::size_t value_span_start =
        content_offset + eq_pos + 1 + value.find_first_not_of(" \t");
    const std::size_t value_span_end =
        content_offset + eq_pos + 1 + value.find_last_not_of(" \t") + 1;
    const diag::SourceSpan value_span{value_span_start, value_span_end};

    // Look up key in schema
    const ConfigKeySpec* spec = nullptr;
    for (const auto& s : schema) {
      if (key == s.name) {
        spec = &s;
        break;
      }
    }

    if (spec) {
      spec->parse_and_apply(value, value_span, result.config,
                            result.diagnostics);
      continue;
    }

    // Unknown key: warn and provide suggestions
    std::vector<std::string> suggestions = SuggestKeys(key, schema, 3);

    auto warn = diag::Warning("C0012", "unknown configuration key");
    warn.Label(diag::SourceSpan{key_span_start, key_span_end}, "unknown key");
    if (!suggestions.empty()) {
      if (suggestions.size() == 1) {
        warn.Note("Did you mean '" + suggestions.front() + "'?");
      } else {
        std::string note = "Did you mean one of: ";
        for (std::size_t si = 0; si < suggestions.size(); ++si) {
          if (si) note += ", ";
          note += "'" + suggestions[si] + "'";
        }
        note += "?";
        warn.Note(std::move(note));
      }
    }
    {
      std::string known = "Known keys are: ";
      for (std::size_t i = 0; i < schema.size(); ++i) {
        if (i) known += ", ";
        known += schema[i].name;
      }
      warn.Note(std::move(known));
    }
    warn.Emit(result.diagnostics);
  }

  // Check for unclosed config block
  if (in_config_block) {
    diag::Error("C0013", "unclosed configuration block")
        .Label(diag::SourceSpan{source.line_starts[config_start_line - 1],
                                source.line_starts[config_start_line - 1] + 14},
               "config block started here")
        .Note("Configuration block must be closed with '# ///'")
        .Emit(result.diagnostics);
  }

  return result;
}

}  // namespace

ConfigParseResult ParseConfiguration(const diag::SourceFile& source) {
  return ParseStructuredConfigWithSchema(source, ConfigurationSchema());
}

}  // namespace turing::parse
