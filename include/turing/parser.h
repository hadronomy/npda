// C++23. Turing Machine parser with rustc-style diagnostics and error recovery.
// Supports single and multi-tape configurations with all variants

#pragma once

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstdio>
#include <expected>
#include <functional>
#include <istream>
#include <limits>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "diag.h"
#include "turing/rule.h"
#include "turing/turing.h"

namespace turing::parse {

using TM = turing::TuringMachine<std::string, std::string>;
using Rule = turing::Rule<std::string, std::string>;

struct ParseResult {
  std::expected<TM, diag::Diagnostics> value;
  diag::SourceFile source;
  TMConfig config;                // Parsed configuration options
  diag::Diagnostics diagnostics;  // Always include diagnostics (warnings, etc.)
};

struct Token {
  std::string text;
  diag::Span span;
};

struct Line {
  std::size_t num_1 = 1;      // 1-based
  std::vector<Token> tokens;  // tokens before '#'
  diag::Span line_span;       // entire line (without '\n')
};

struct SpecTokens {
  std::vector<Line> lines;  // all lines with tokens
};

// Read entire stream
inline std::string read_all(std::istream& is) {
  std::ostringstream oss;
  oss << is.rdbuf();
  return oss.str();
}

inline SpecTokens lex(const diag::SourceFile& src) {
  SpecTokens st;
  st.lines.reserve(src.line_count());

  for (std::size_t li = 1; li <= src.line_count(); ++li) {
    const std::string_view sv = src.line_view(li);
    const std::size_t line_start = src.line_starts[li - 1];
    const std::size_t line_end = line_start + sv.size();

    Line L;
    L.num_1 = li;
    L.line_span = diag::Span{line_start, line_end};

    // Cut comments
    const std::size_t cut = sv.find('#');
    const std::size_t upto = (cut == std::string_view::npos) ? sv.size() : cut;

    // Tokenize (whitespace-separated)
    std::size_t i = 0;
    const auto is_space = [](char c) noexcept {
      return std::isspace(static_cast<unsigned char>(c)) != 0;
    };

    while (i < upto) {
      while (i < upto && is_space(sv[i]))
        ++i;
      if (i >= upto)
        break;

      std::size_t j = i;
      while (j < upto && !is_space(sv[j]))
        ++j;

      const std::size_t tok_lo = line_start + i;
      const std::size_t tok_hi = line_start + j;
      L.tokens.push_back(Token{
        std::string(sv.substr(i, j - i)),
        diag::Span{tok_lo, tok_hi},
      });

      i = j;
    }

    st.lines.push_back(std::move(L));
  }

  return st;
}

inline void add_simple_error(
  diag::Diagnostics& dx,
  std::string code,
  std::string msg,
  diag::Span where,
  std::string label_msg
) {
  diag::Diagnostic d;
  d.severity = diag::Severity::Error;
  d.code = std::move(code);
  d.message = std::move(msg);
  d.labels.push_back(diag::Label{
    .span = where,
    .primary = true,
    .message = std::move(label_msg),
  });
  dx.items.push_back(std::move(d));
}

inline void add_symbol_error(
  diag::Diagnostics& dx,
  std::string code,
  std::string msg,
  const Token& t,
  std::string label_msg
) {
  add_simple_error(dx, std::move(code), std::move(msg), t.span, std::move(label_msg));
}

inline bool line_has_tokens(const Line& L) {
  return !L.tokens.empty();
}

// Next non-empty line >= i
inline std::optional<std::size_t> next_nonempty(const SpecTokens& st, std::size_t i) {
  const std::size_t n = st.lines.size();
  for (std::size_t k = i; k < n; ++k) {
    if (line_has_tokens(st.lines[k]))
      return k;
  }
  return std::nullopt;
}

// Turn tokens to set of strings
inline std::unordered_set<std::string> to_set(const std::vector<Token>& toks) {
  std::unordered_set<std::string> s;
  s.reserve(toks.size());
  for (const auto& t : toks)
    s.insert(t.text);
  return s;
}

// Configuration parsing diagnostics and validation
struct ConfigParseResult {
  TMConfig config;
  diag::Diagnostics diagnostics;
  bool has_config_block = false;
  bool has_errors = false;
};

// TOML token types for better error reporting
enum class TomlTokenType { Key, Equals, Value, String, Number, Boolean, Invalid };

struct TomlToken {
  TomlTokenType type;
  std::string_view text;
  diag::Span span;
  std::string expected_type;
};

// ---------- Helpers for config parsing ----------

// trim ASCII spaces and tabs only (config grammar uses them)
inline std::string_view trim_ws(std::string_view s) {
  const auto b = s.find_first_not_of(" \t");
  if (b == std::string_view::npos)
    return {};
  const auto e = s.find_last_not_of(" \t");
  return s.substr(b, e - b + 1);
}

inline std::string_view unquote_if(std::string_view v) {
  if (v.size() >= 2) {
    const char a = v.front();
    const char b = v.back();
    if ((a == '"' && b == '"') || (a == '\'' && b == '\'')) {
      return v.substr(1, v.size() - 2);
    }
  }
  return v;
}

inline bool parse_uint_nonzero(std::string_view v, std::size_t& out) {
  v = trim_ws(v);
  if (v.empty())
    return false;
  if (!std::all_of(v.begin(), v.end(), [](unsigned char c) { return std::isdigit(c) != 0; })) {
    return false;
  }
  const auto* first = v.data();
  const auto* last = v.data() + v.size();
  unsigned long long ull = 0;
  auto ec = std::from_chars(first, last, ull).ec;
  if (ec != std::errc())
    return false;
  if (ull == 0)
    return false;
  if (ull > std::numeric_limits<std::size_t>::max())
    return false;
  out = static_cast<std::size_t>(ull);
  return true;
}

inline bool parse_bool_literal(std::string_view v, bool& out) {
  v = trim_ws(unquote_if(v));
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
inline bool parse_enum_value(
  std::string_view v,
  const std::vector<std::string>& allowed,
  std::size_t& idx_out
) {
  v = trim_ws(unquote_if(v));
  for (std::size_t i = 0; i < allowed.size(); ++i) {
    if (v == allowed[i]) {
      idx_out = i;
      return true;
    }
  }
  return false;
}

// Bounded Levenshtein distance (UTF-8 as bytes, good enough for keys)
inline std::size_t
  levenshtein_distance_bounded(std::string_view a, std::string_view b, std::size_t max_dist) {
  const std::size_t n = a.size();
  const std::size_t m = b.size();
  if (n == 0)
    return m;
  if (m == 0)
    return n;
  if (max_dist != std::numeric_limits<std::size_t>::max() && (n > m ? n - m : m - n) > max_dist) {
    return max_dist + 1;
  }

  std::vector<std::size_t> prev(m + 1), curr(m + 1);
  for (std::size_t j = 0; j <= m; ++j)
    prev[j] = j;
  for (std::size_t i = 1; i <= n; ++i) {
    curr[0] = i;
    std::size_t row_min = curr[0];
    for (std::size_t j = 1; j <= m; ++j) {
      const std::size_t cost = (a[i - 1] == b[j - 1]) ? 0 : 1;
      curr[j] = std::min({prev[j] + 1, curr[j - 1] + 1, prev[j - 1] + cost});
      row_min = std::min(row_min, curr[j]);
    }
    if (max_dist != std::numeric_limits<std::size_t>::max() && row_min > max_dist) {
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
  std::function<bool(std::string_view, diag::Span, TMConfig&, diag::Diagnostics&)> parse_and_apply;
};

inline const std::vector<ConfigKeySpec>& default_config_schema() {
  static const std::vector<ConfigKeySpec> kSchema = {
    ConfigKeySpec{
      "num_tapes",
      "positive integer",
      {},
      [](std::string_view v, diag::Span span, TMConfig& cfg, diag::Diagnostics& dx) {
        std::size_t nt = 0;
        if (!parse_uint_nonzero(v, nt)) {
          diag::Diagnostic d;
          d.severity = diag::Severity::Error;
          d.code = "C0007";
          d.message = "invalid value for 'num_tapes'";
          d.labels.push_back({.span = span, .primary = true, .message = "expected positive integer"}
          );
          d.notes.push_back("num_tapes must be a positive integer (e.g., 1, 2, 3)");
          dx.items.push_back(std::move(d));
          return false;
        }
        cfg.num_tapes = nt;
        return true;
      }
    },
    ConfigKeySpec{
      "tape_direction",
      "enum('bidirectional'|'right-only')",
      {"bidirectional", "right-only"},
      [](std::string_view v, diag::Span span, TMConfig& cfg, diag::Diagnostics& dx) {
        std::size_t idx = std::numeric_limits<std::size_t>::max();
        if (!parse_enum_value(v, {"bidirectional", "right-only"}, idx)) {
          diag::Diagnostic d;
          d.severity = diag::Severity::Error;
          d.code = "C0009";
          d.message = "invalid value for 'tape_direction'";
          d.labels.push_back({.span = span, .primary = true, .message = "invalid option"});
          d.notes.push_back("Valid options are: 'bidirectional' or 'right-only'");
          dx.items.push_back(std::move(d));
          return false;
        }
        cfg.tape_direction = (idx == 0) ? TapeDirection::Bidirectional : TapeDirection::RightOnly;
        return true;
      }
    },
    ConfigKeySpec{
      "operation_mode",
      "enum('simultaneous'|'independent')",
      {"simultaneous", "independent"},
      [](std::string_view v, diag::Span span, TMConfig& cfg, diag::Diagnostics& dx) {
        std::size_t idx = std::numeric_limits<std::size_t>::max();
        if (!parse_enum_value(v, {"simultaneous", "independent"}, idx)) {
          diag::Diagnostic d;
          d.severity = diag::Severity::Error;
          d.code = "C0010";
          d.message = "invalid value for 'operation_mode'";
          d.labels.push_back({.span = span, .primary = true, .message = "invalid option"});
          d.notes.push_back("Valid options are: 'simultaneous' or 'independent'");
          dx.items.push_back(std::move(d));
          return false;
        }
        cfg.operation_mode = (idx == 0) ? OperationMode::Simultaneous : OperationMode::Independent;
        return true;
      }
    },
    ConfigKeySpec{
      "allow_stay",
      "boolean('true'|'false')",
      {"true", "false"},
      [](std::string_view v, diag::Span span, TMConfig& cfg, diag::Diagnostics& dx) {
        bool b = false;
        if (!parse_bool_literal(v, b)) {
          diag::Diagnostic d;
          d.severity = diag::Severity::Error;
          d.code = "C0011";
          d.message = "invalid value for 'allow_stay'";
          d.labels.push_back({.span = span, .primary = true, .message = "invalid boolean value"});
          d.notes.push_back("Valid options are: 'true' or 'false'");
          dx.items.push_back(std::move(d));
          return false;
        }
        cfg.allow_stay = b;
        return true;
      }
    },
  };
  return kSchema;
}

// For unknown keys: produce suggestions using bounded Levenshtein
inline std::vector<std::string> suggest_keys(
  std::string_view unknown,
  const std::vector<ConfigKeySpec>& schema,
  std::size_t max_distance = 3
) {
  std::vector<std::string> out;
  for (const auto& s : schema) {
    const auto d = levenshtein_distance_bounded(unknown, s.name, max_distance);
    if (d <= max_distance)
      out.push_back(s.name);
  }
  std::sort(out.begin(), out.end());
  out.erase(std::unique(out.begin(), out.end()), out.end());
  return out;
}

// Parse TOML-like configuration from structured comments with
// comprehensive diagnostics
// Format: # /// config\n# key = value\n# ///
inline ConfigParseResult parse_structured_config_with_schema(
  const diag::SourceFile& source,
  const std::vector<ConfigKeySpec>& schema
) {
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
    if (start == std::string_view::npos)
      continue;
    std::string_view trimmed = line.substr(start);

    // Check for config block markers
    if (trimmed.starts_with("# /// config")) {
      if (in_config_block) {
        // Nested config block - error
        diag::Diagnostic d;
        d.severity = diag::Severity::Error;
        d.code = "C0001";
        d.message = "nested configuration block";
        d.labels.push_back(diag::Label{
          .span = diag::Span{line_start + start, line_start + start + 14},
          .primary = true,
          .message = "config block already started"
        });
        result.diagnostics.items.push_back(std::move(d));
        result.has_errors = true;
      } else {
        in_config_block = true;
        config_start_line = li;
        result.has_config_block = true;
      }
      continue;
    }

    if (trimmed.starts_with("# ///") && in_config_block) {
      // End of config block
      in_config_block = false;
      break;
    }

    if (!in_config_block || !trimmed.starts_with("#"))
      continue;

    // Remove the # and any following whitespace
    std::string_view content = trimmed.substr(1);
    std::size_t content_start = content.find_first_not_of(" \t");
    if (content_start == std::string_view::npos)
      continue;
    content = content.substr(content_start);
    const std::size_t content_offset = line_start + start + 1 + content_start;

    // Skip empty lines or comment-only lines
    if (content.empty())
      continue;

    // Parse key=value pairs with detailed error reporting
    std::size_t eq_pos = content.find('=');

    if (eq_pos == std::string_view::npos) {
      // No equals sign found
      diag::Diagnostic d;
      d.severity = diag::Severity::Error;
      d.code = "C0002";
      d.message = "invalid configuration line: missing '='";
      d.labels.push_back(diag::Label{
        .span = diag::Span{content_offset, content_offset + content.length()},
        .primary = true,
        .message = "expected 'key = value' format"
      });
      d.notes.push_back("Configuration lines must follow the format: key = value");
      result.diagnostics.items.push_back(std::move(d));
      result.has_errors = true;
      continue;
    }

    // Extract and validate key
    std::string_view key = trim_ws(content.substr(0, eq_pos));
    std::string_view value = trim_ws(content.substr(eq_pos + 1));

    // Validate key
    if (key.empty()) {
      diag::Diagnostic d;
      d.severity = diag::Severity::Error;
      d.code = "C0003";
      d.message = "empty configuration key";
      d.labels.push_back(diag::Label{
        .span = diag::Span{content_offset, content_offset + eq_pos},
        .primary = true,
        .message = "expected key before '='"
      });
      result.diagnostics.items.push_back(std::move(d));
      result.has_errors = true;
      continue;
    }

    // Check for invalid characters in key
    bool valid_key = true;
    for (char c : key) {
      if (!std::isalnum(static_cast<unsigned char>(c)) && c != '_' && c != '-') {
        valid_key = false;
        break;
      }
    }

    if (!valid_key) {
      diag::Diagnostic d;
      d.severity = diag::Severity::Error;
      d.code = "C0004";
      d.message = "invalid configuration key";
      d.labels.push_back(diag::Label{
        .span =
          diag::Span{
            content_offset + key.find_first_not_of(" \t"),
            content_offset + key.find_last_not_of(" \t") + 1
          },
        .primary = true,
        .message = "keys may only contain letters, numbers, underscores, and hyphens"
      });
      result.diagnostics.items.push_back(std::move(d));
      result.has_errors = true;
      continue;
    }

    // Check for duplicate keys
    std::string key_str(key);
    if (seen_keys.count(key_str)) {
      diag::Diagnostic d;
      d.severity = diag::Severity::Error;
      d.code = "C0005";
      d.message = "duplicate configuration key";
      d.labels.push_back(diag::Label{
        .span =
          diag::Span{
            content_offset + key.find_first_not_of(" \t"),
            content_offset + key.find_last_not_of(" \t") + 1
          },
        .primary = true,
        .message = "duplicate key"
      });
      d.notes.push_back("Configuration key '" + key_str + "' was already defined");
      result.diagnostics.items.push_back(std::move(d));
      result.has_errors = true;
      continue;
    }
    seen_keys.insert(key_str);

    // Validate value presence
    if (value.empty()) {
      diag::Diagnostic d;
      d.severity = diag::Severity::Error;
      d.code = "C0006";
      d.message = "empty configuration value";
      d.labels.push_back(diag::Label{
        .span = diag::Span{content_offset + eq_pos + 1, content_offset + content.length()},
        .primary = true,
        .message = "expected value after '='"
      });
      result.diagnostics.items.push_back(std::move(d));
      result.has_errors = true;
      continue;
    }

    // Parse and validate specific configuration options
    const std::size_t key_span_start = content_offset + key.find_first_not_of(" \t");
    const std::size_t key_span_end = content_offset + key.find_last_not_of(" \t") + 1;
    const std::size_t value_span_start =
      content_offset + eq_pos + 1 + value.find_first_not_of(" \t");
    const std::size_t value_span_end =
      content_offset + eq_pos + 1 + value.find_last_not_of(" \t") + 1;
    const diag::Span value_span{value_span_start, value_span_end};

    // Look up key in schema
    const ConfigKeySpec* spec = nullptr;
    for (const auto& s : schema) {
      if (key == s.name) {
        spec = &s;
        break;
      }
    }

    if (spec) {
      const bool ok = spec->parse_and_apply(value, value_span, result.config, result.diagnostics);
      if (!ok)
        result.has_errors = true;
      continue;
    }

    // Unknown key: warn and provide suggestions
    std::vector<std::string> suggestions = suggest_keys(key, schema, 3);

    diag::Diagnostic d;
    d.severity = diag::Severity::Warning;
    d.code = "C0012";
    d.message = "unknown configuration key";
    d.labels.push_back(
      {.span = diag::Span{key_span_start, key_span_end}, .primary = true, .message = "unknown key"}
    );
    if (!suggestions.empty()) {
      if (suggestions.size() == 1) {
        d.notes.push_back("Did you mean '" + suggestions.front() + "'?");
      } else {
        std::string note = "Did you mean one of: ";
        for (std::size_t si = 0; si < suggestions.size(); ++si) {
          if (si)
            note += ", ";
          note += "'" + suggestions[si] + "'";
        }
        note += "?";
        d.notes.push_back(std::move(note));
      }
    }
    {
      std::string known = "Known keys are: ";
      for (std::size_t i = 0; i < schema.size(); ++i) {
        if (i)
          known += ", ";
        known += schema[i].name;
      }
      d.notes.push_back(std::move(known));
    }
    result.diagnostics.items.push_back(std::move(d));
  }

  // Check for unclosed config block
  if (in_config_block) {
    diag::Diagnostic d;
    d.severity = diag::Severity::Error;
    d.code = "C0013";
    d.message = "unclosed configuration block";
    d.labels.push_back(diag::Label{
      .span =
        diag::Span{
          source.line_starts[config_start_line - 1], source.line_starts[config_start_line - 1] + 14
        },
      .primary = true,
      .message = "config block started here"
    });
    d.notes.push_back("Configuration block must be closed with '# ///'");
    result.diagnostics.items.push_back(std::move(d));
    result.has_errors = true;
  }

  return result;
}

// Backwards-compatible entry point using built-in schema
inline ConfigParseResult parse_structured_config(const diag::SourceFile& source) {
  return parse_structured_config_with_schema(source, default_config_schema());
}

inline bool looks_like_single_tape_transition(
  const std::vector<Token>& toks,
  const std::unordered_set<std::string>& Q,
  const std::unordered_set<std::string>& G
) {
  if (toks.size() != 5)
    return false;

  const auto& from = toks[0];
  const auto& read = toks[1];
  const auto& to = toks[2];
  const auto& write = toks[3];
  const auto& move = toks[4];

  if (Q.count(from.text) == 0)
    return false;
  if (G.count(read.text) == 0)
    return false;  // Γ
  if (Q.count(to.text) == 0)
    return false;
  if (G.count(write.text) == 0)
    return false;
  if (move.text != "L" && move.text != "R" && move.text != "S")
    return false;

  return true;
}

inline bool looks_like_multi_tape_transition(
  const std::vector<Token>& toks,
  const std::unordered_set<std::string>& Q,
  const std::unordered_set<std::string>& G,
  std::size_t num_tapes
) {
  // Expect: from read1 ... readN to write1 move1 write2 move2 ... writeN moveN
  const std::size_t expected_size = 2 + num_tapes * 3;
  if (toks.size() != expected_size)
    return false;

  const auto& from = toks[0];
  if (Q.count(from.text) == 0)
    return false;

  const std::size_t to_idx = 1 + num_tapes;
  if (to_idx >= toks.size())
    return false;
  const auto& to = toks[to_idx];
  if (Q.count(to.text) == 0)
    return false;

  // read symbols
  for (std::size_t i = 0; i < num_tapes; ++i) {
    if (1 + i >= toks.size())
      return false;
    if (G.count(toks[1 + i].text) == 0)
      return false;
  }

  // write/move pairs
  for (std::size_t i = 0; i < num_tapes; ++i) {
    const std::size_t pair_base = 1 + num_tapes + 1 + 2 * i;
    if (pair_base + 1 >= toks.size())
      return false;
    const auto& write = toks[pair_base];
    const auto& move = toks[pair_base + 1];
    if (G.count(write.text) == 0)
      return false;
    if (move.text != "L" && move.text != "R" && move.text != "S")
      return false;
  }

  return true;
}

inline ParseResult parse_with_diagnostics(std::istream& is, std::string filename) {
  ParseResult out;
  out.source = diag::SourceFile::from(filename, read_all(is));
  const auto st = lex(out.source);

  diag::Diagnostics dx;

  const auto eof_span = [&]() -> diag::Span {
    if (out.source.text.empty())
      return {0, 0};
    const std::size_t n = out.source.text.size();
    return {n ? n - 1 : 0, n};
  };

  const auto need_line = [&](std::optional<std::size_t> idx, std::string name) {
    if (!idx.has_value()) {
      add_simple_error(
        dx,
        "E0001",
        "missing required section: " + name,
        eof_span(),
        "file ends before section '" + name + "'"
      );
      return false;
    }
    return true;
  };

  std::size_t i = 0;
  TMConfig config;

  // Parse structured configuration
  auto config_result = parse_structured_config(out.source);
  config = config_result.config;

  // Accumulate configuration diagnostics
  for (auto& diag : config_result.diagnostics.items) {
    dx.items.push_back(std::move(diag));
  }

  // 1) Q (states)
  std::vector<Token> QToks;
  const std::optional<std::size_t> iQ = next_nonempty(st, i);
  if (!need_line(iQ, "Q")) {
    QToks = {};
  } else {
    QToks = st.lines[*iQ].tokens;
    i = *iQ + 1;
    if (QToks.empty()) {
      add_simple_error(
        dx, "E0002", "empty set Q", st.lines[*iQ].line_span, "no states found on this line"
      );
    }
  }

  // 2) Σ (input alphabet)
  std::vector<Token> SToks;
  const std::optional<std::size_t> iS = next_nonempty(st, i);
  if (!need_line(iS, "Σ")) {
    SToks = {};
  } else {
    SToks = st.lines[*iS].tokens;
    i = *iS + 1;
    if (SToks.empty()) {
      add_simple_error(
        dx, "E0003", "empty alphabet Σ", st.lines[*iS].line_span, "no input symbols found"
      );
    }
  }

  // 3) Γ (tape alphabet)
  std::vector<Token> GToks;
  const std::optional<std::size_t> iG = next_nonempty(st, i);
  if (!need_line(iG, "Γ")) {
    GToks = {};
  } else {
    GToks = st.lines[*iG].tokens;
    i = *iG + 1;
    if (GToks.empty()) {
      add_simple_error(
        dx, "E0004", "empty tape alphabet Γ", st.lines[*iG].line_span, "no tape symbols found"
      );
    }
  }

  // Build sets
  const auto Qset = to_set(QToks);
  const auto Sset = to_set(SToks);
  const auto Gset = to_set(GToks);
  (void)Sset;  // currently unused in transition parsing

  // 4) q0 (start state)
  Token q0Tok{"<q0?>", eof_span()};
  const std::optional<std::size_t> iq0 = next_nonempty(st, i);
  if (!need_line(iq0, "q0")) {
    // keep default placeholder
  } else {
    const Line& Lq0 = st.lines[*iq0];
    i = *iq0 + 1;
    if (Lq0.tokens.size() != 1) {
      add_simple_error(
        dx,
        "E0005",
        "invalid q0 line",
        Lq0.line_span,
        "expected exactly one token (the start state)"
      );
      if (!Lq0.tokens.empty())
        q0Tok = Lq0.tokens.front();
    } else {
      q0Tok = Lq0.tokens[0];
    }
    if (Qset.count(q0Tok.text) == 0) {
      add_symbol_error(dx, "E0007", "start state not in Q", q0Tok, "this state is not listed in Q");
    }
  }

  // 5) b (blank symbol)
  Token bTok{"<b?>", eof_span()};
  const std::optional<std::size_t> ib = next_nonempty(st, i);
  if (!need_line(ib, "b")) {
    // keep default
  } else {
    const Line& Lb = st.lines[*ib];
    i = *ib + 1;
    if (Lb.tokens.size() != 1) {
      add_simple_error(
        dx,
        "E0006",
        "invalid blank symbol line",
        Lb.line_span,
        "expected exactly one token (the blank symbol)"
      );
      if (!Lb.tokens.empty())
        bTok = Lb.tokens.front();
    } else {
      bTok = Lb.tokens[0];
    }
    if (Gset.count(bTok.text) == 0) {
      add_symbol_error(
        dx, "E0008", "blank symbol not in Γ", bTok, "this symbol is not listed in Γ"
      );
    }
  }

  // 6) F (accepting states) - optional
  std::vector<Token> Ftok;
  bool has_F_syntax = false;
  const std::optional<std::size_t> iF = next_nonempty(st, i);
  if (iF.has_value()) {
    const Line& LF = st.lines[*iF];

    // Check if this looks like transitions or accepting states
    bool is_transition = false;
    if (config.num_tapes == 1) {
      is_transition = looks_like_single_tape_transition(LF.tokens, Qset, Gset);
    } else {
      is_transition = looks_like_multi_tape_transition(LF.tokens, Qset, Gset, config.num_tapes);
    }

    if (!is_transition) {
      has_F_syntax = true;
      Ftok.reserve(LF.tokens.size());
      for (const auto& t : LF.tokens) {
        if (Qset.count(t.text) == 0) {
          diag::Diagnostic d;
          d.severity = diag::Severity::Error;
          d.code = "E0009";
          d.message = "invalid accepting states line";
          d.labels.push_back(diag::Label{
            .span = t.span,
            .primary = true,
            .message = "unknown state '" + t.text + "'",
          });
          d.notes.push_back("F must contain only states from Q");
          dx.items.push_back(std::move(d));
        } else {
          Ftok.push_back(t);
        }
      }
      i = *iF + 1;
    }
  }

  // 7) Transitions
  std::vector<Rule> rules;
  if (i < st.lines.size()) {
    rules.reserve(st.lines.size() - i);
  }

  for (std::size_t k = i; k < st.lines.size(); ++k) {
    const Line& L = st.lines[k];
    if (!line_has_tokens(L))
      continue;

    const auto& T = L.tokens;

    if (config.num_tapes == 1) {
      // Single tape transition: from read to write move
      if (T.size() != 5) {
        add_simple_error(
          dx,
          "E0010",
          "transition must have exactly 5 parts",
          L.line_span,
          "expected: from read to write move"
        );
        continue;  // recover by skipping this line
      }

      const Token& from = T[0];
      const Token& read = T[1];
      const Token& to = T[2];
      const Token& write = T[3];
      const Token& move = T[4];

      // Recovery flags
      bool bad_from = false;
      bool bad_read = false;
      bool bad_to = false;
      bool bad_write = false;
      bool bad_move = false;

      if (Qset.count(from.text) == 0) {
        add_symbol_error(dx, "E0011", "unknown 'from' state", from, "state not in Q");
        bad_from = true;
      }
      if (Gset.count(read.text) == 0) {
        add_symbol_error(dx, "E0012", "unknown read symbol", read, "symbol not in Γ");
        bad_read = true;
      }
      if (Qset.count(to.text) == 0) {
        add_symbol_error(dx, "E0014", "unknown 'to' state", to, "state not in Q");
        bad_to = true;
      }
      if (Gset.count(write.text) == 0) {
        add_symbol_error(dx, "E0015", "unknown write symbol", write, "symbol not in Γ");
        bad_write = true;
      }
      if (move.text != "L" && move.text != "R" && move.text != "S") {
        add_symbol_error(dx, "E0016", "invalid move direction", move, "must be L, R, or S");
        bad_move = true;
      }

      // Build arity-1 multi rule (even for invalids to aid recovery)
      Direction dir = Direction::Stay;
      if (move.text == "L") {
        dir = Direction::Left;
      } else if (move.text == "R") {
        dir = Direction::Right;
      } else {
        dir = Direction::Stay;
      }

      Rule rule;
      rule.from = from.text;
      rule.to = to.text;
      rule.read = {read.text};
      rule.write = {write.text};
      rule.move = {dir};

      (void)bad_from;
      (void)bad_to;
      (void)bad_read;
      (void)bad_write;
      (void)bad_move;

      rules.push_back(std::move(rule));
    } else {
      // Multi-tape transition
      const std::size_t expected_size = 2 + config.num_tapes * 3;
      if (T.size() != expected_size) {
        add_simple_error(
          dx,
          "E0010",
          "multi-tape transition must have correct format",
          L.line_span,
          "expected: from read1 ... readN to write1 move1 ... writeN moveN"
        );
        continue;
      }

      const Token& from = T[0];
      if (Qset.count(from.text) == 0) {
        add_symbol_error(dx, "E0011", "unknown 'from' state", from, "state not in Q");
        continue;
      }

      const std::size_t to_idx = 1 + config.num_tapes;
      if (to_idx >= T.size()) {
        add_simple_error(
          dx,
          "E0014",
          "missing 'to' state",
          L.line_span,
          "multi-tape transition missing target state"
        );
        continue;
      }

      const Token& to = T[to_idx];
      if (Qset.count(to.text) == 0) {
        add_symbol_error(dx, "E0014", "unknown 'to' state", to, "state not in Q");
        continue;
      }

      Rule rule;
      rule.from = from.text;
      rule.to = to.text;
      rule.read.resize(config.num_tapes);
      rule.write.resize(config.num_tapes);
      rule.move.resize(config.num_tapes);

      bool valid = true;

      // Parse read symbols
      for (std::size_t tape_idx = 0; tape_idx < config.num_tapes; ++tape_idx) {
        const std::size_t read_idx = 1 + tape_idx;
        if (read_idx >= T.size()) {
          valid = false;
          break;
        }
        const Token& read = T[read_idx];
        if (Gset.count(read.text) == 0) {
          add_symbol_error(dx, "E0012", "unknown read symbol", read, "symbol not in Γ");
          valid = false;
        } else {
          rule.read[tape_idx] = read.text;
        }
      }

      // Parse (write_i, move_i) pairs after 'to'
      for (std::size_t tape_idx = 0; tape_idx < config.num_tapes; ++tape_idx) {
        const std::size_t pair_base = 1 + config.num_tapes + 1 + 2 * tape_idx;
        if (pair_base + 1 >= T.size()) {
          valid = false;
          break;
        }
        const Token& write = T[pair_base];
        const Token& move = T[pair_base + 1];

        if (Gset.count(write.text) == 0) {
          add_symbol_error(dx, "E0015", "unknown write symbol", write, "symbol not in Γ");
          valid = false;
        } else {
          rule.write[tape_idx] = write.text;
        }

        if (move.text != "L" && move.text != "R" && move.text != "S") {
          add_symbol_error(dx, "E0016", "invalid move direction", move, "must be L, R, or S");
          valid = false;
        } else {
          if (move.text == "L") {
            rule.move[tape_idx] = Direction::Left;
          } else if (move.text == "R") {
            rule.move[tape_idx] = Direction::Right;
          } else {
            rule.move[tape_idx] = Direction::Stay;
          }
        }
      }

      if (valid) {
        rules.push_back(std::move(rule));
      }
    }
  }

  // Build Turing Machine regardless; suppress returning it if errors exist
  auto b = TM::Builder();
  b.config(config);
  b.start(q0Tok.text).blank(bTok.text);

  if (has_F_syntax && !Ftok.empty()) {
    std::vector<std::string> F;
    F.reserve(Ftok.size());
    for (const auto& t : Ftok)
      F.push_back(t.text);
    b.accepting(F.begin(), F.end());
  }

  for (auto& r : rules)
    b.rule(r);

  auto built = std::move(b).build();
  if (!built) {
    // Report as an error but still include prior diagnostics
    add_simple_error(
      dx,
      "E0017",
      "failed to build Turing Machine",
      st.lines.empty() ? eof_span() : st.lines.back().line_span,
      built.error().message
    );
  }

  out.config = config;
  out.diagnostics = std::move(dx);

  if (out.diagnostics.has_errors()) {
    return {
      std::unexpected(std::move(out.diagnostics)),
      out.source,
      out.config,
      std::move(out.diagnostics)
    };
  }

  return {
    std::expected<TM, diag::Diagnostics>(*std::move(built)),
    out.source,
    out.config,
    std::move(out.diagnostics)
  };
}

}  // namespace turing::parse