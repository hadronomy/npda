// Own the rustc-style diagnostics. STL-only.
// Import the standard library below. Change this file, not a header.
module;
#include <unistd.h>

export module diag;
import std;
// C++23. Minimal rustc-style diagnostics: spans, labels, rendering.



export namespace diag {

struct Span {
  std::size_t lo = 0;  // inclusive
  std::size_t hi = 0;  // exclusive
  [[nodiscard]] bool empty() const { return lo >= hi; }
};

struct SourceFile {
  std::string filename;
  std::string text;
  std::vector<std::size_t> line_starts;  // offsets of each line start

  [[nodiscard]] static SourceFile from(std::string filename, std::string text) {
    SourceFile sf;
    sf.filename = std::move(filename);
    sf.text = std::move(text);

    std::size_t newline_count = 0;
    for (char c : sf.text) {
      if (c == '\n') {
        newline_count++;
      }
    }
    sf.line_starts.reserve(newline_count + 1);
    sf.line_starts.push_back(0);
    for (std::size_t i = 0; i < sf.text.size(); ++i) {
      if (sf.text[i] == '\n')
        sf.line_starts.push_back(i + 1);
    }
    return sf;
  }

  [[nodiscard]] std::size_t line_count() const { return line_starts.size(); }

  [[nodiscard]] std::pair<std::size_t, std::size_t> line_col(std::size_t pos) const {
    auto it = std::upper_bound(line_starts.begin(), line_starts.end(), pos);
    std::size_t line = (it == line_starts.begin())
                       ? 1
                       : static_cast<std::size_t>(std::distance(line_starts.begin(), it));

    if (line == 0)
      line = 1;
    if (line > line_starts.size())
      line = line_starts.size();

    std::size_t start = line_starts[line - 1];
    std::size_t col = (pos >= start) ? (pos - start + 1) : 1;
    return {line, col};
  }

  [[nodiscard]] std::string_view line_view(std::size_t line_1) const {
    if (line_1 == 0 || line_1 > line_starts.size())
      return {};
    std::size_t start = line_starts[line_1 - 1];
    std::size_t end = text.size();
    if (line_1 < line_starts.size())
      end = line_starts[line_1] - 1;
    return std::string_view(text).substr(start, end - start);
  }
};

enum class Severity { Error, Warning, Note, Help };

struct Label {
  Span span{};
  bool primary = true;
  std::string message;
};

struct Diagnostic {
  Severity severity = Severity::Error;
  std::string code;
  std::string message;
  std::vector<Label> labels;
  std::vector<std::string> notes;
};

struct Diagnostics {
  std::vector<Diagnostic> items;

  [[nodiscard]] bool has_errors() const {
    return std::any_of(items.begin(), items.end(), [](const Diagnostic& d) {
      return d.severity == Severity::Error;
    });
  }
};

// Color output control. Auto follows NO_COLOR and terminal detection.
enum class ColorMode { Auto, Always, Never };

struct RenderOptions {
  ColorMode color = ColorMode::Auto;
  std::size_t context_lines = 0;
};

// Detect color support: off with NO_COLOR set or dumb terminal,
// otherwise on for terminals only. Matches Clang behavior.
[[nodiscard]] inline bool color_enabled() {
  if (const char* no_color = std::getenv("NO_COLOR");
      no_color != nullptr && no_color[0] != '\0')
    return false;
  if (const char* term = std::getenv("TERM");
      term != nullptr && std::string_view(term) == "dumb")
    return false;
  return isatty(STDERR_FILENO) != 0;
}

// Resolve Auto against the terminal. Explicit modes pass through.
[[nodiscard]] inline bool resolve_color(ColorMode mode) {
  if (mode == ColorMode::Always)
    return true;
  if (mode == ColorMode::Never)
    return false;
  return color_enabled();
}

// Builds one diagnostic step by step. Emits nothing until emit().
class DiagnosticBuilder {
 public:
  DiagnosticBuilder(std::string code, std::string message, Severity severity = Severity::Error) {
    d_.severity = severity;
    d_.code = std::move(code);
    d_.message = std::move(message);
  }
  DiagnosticBuilder& label(Span span, std::string message, bool primary = true) {
    d_.labels.push_back(Label{.span = span, .primary = primary, .message = std::move(message)});
    return *this;
  }
  DiagnosticBuilder& note(std::string note) {
    d_.notes.push_back(std::move(note));
    return *this;
  }
  void emit(Diagnostics& dx) { dx.items.push_back(std::move(d_)); }

 private:
  Diagnostic d_;
};

[[nodiscard]] inline DiagnosticBuilder error(std::string code, std::string message) {
  return DiagnosticBuilder(std::move(code), std::move(message), Severity::Error);
}

[[nodiscard]] inline DiagnosticBuilder warning(std::string code, std::string message) {
  return DiagnosticBuilder(std::move(code), std::move(message), Severity::Warning);
}

// One entry of the error code index. Powers `cc explain CODE`.
// Titles match the emitted messages. C0008 stays retired.
struct CodeInfo {
  std::string_view code;
  std::string_view machine;
  std::string_view title;
  std::string_view hint;
};

[[nodiscard]] inline const std::vector<CodeInfo>& code_table() {
  static const std::vector<CodeInfo> table = {
    {"E0001", "common", "missing required section", "Add the named section. Sections come in fixed order."},
    {"E0002", "common", "empty set Q", "List at least one state on the Q line."},
    {"E0003", "common", "empty alphabet \u03a3", "List at least one input symbol on the \u03a3 line."},
    {"E0004", "npda", "empty stack alphabet \u0393", "List at least one stack symbol on the \u0393 line."},
    {"E0004", "turing", "empty tape alphabet \u0393", "List at least one tape symbol on the \u0393 line."},
    {"E0005", "common", "invalid q0 line", "Write exactly one token: the start state."},
    {"E0006", "npda", "invalid Z0 line", "Write exactly one token: the bottom symbol."},
    {"E0006", "turing", "invalid blank symbol line", "Write exactly one token: the blank symbol."},
    {"E0007", "common", "start state not in Q", "Name a state from the Q line."},
    {"E0008", "npda", "stack bottom symbol not in \u0393", "Name a symbol from the \u0393 line."},
    {"E0008", "turing", "blank symbol not in \u0393", "Name a symbol from the \u0393 line."},
    {"E0009", "common", "invalid accepting states line", "List only states from Q, or drop the line."},
    {"E0010", "npda", "transition too short", "Write from, input, stack top, target, then pushes."},
    {"E0010", "turing", "transition must have exactly 5 parts", "Write from, read, target, write, move."},
    {"E0010", "turing", "multi-tape transition must have correct format", "Write from, reads, target, then write and move pairs."},
    {"E0011", "common", "unknown 'from' state", "Name a state from Q."},
    {"E0012", "npda", "unknown input symbol (or epsilon)", "Use a \u03a3 symbol or an epsilon spelling."},
    {"E0012", "turing", "unknown read symbol", "Use a \u0393 symbol."},
    {"E0013", "npda", "unknown stack top symbol (or epsilon)", "Use a \u0393 symbol or an epsilon spelling."},
    {"E0014", "common", "unknown 'to' state", "Name a state from Q."},
    {"E0015", "npda", "unknown push symbol", "Push only \u0393 symbols or epsilon."},
    {"E0015", "turing", "unknown write symbol", "Write only \u0393 symbols."},
    {"E0019", "turing", "invalid move direction", "Write L, R, or S."},
    {"E0017", "turing", "failed to build Turing Machine", "Fix the errors above first."},
    {"E0018", "turing", "blank symbol cannot be in \u03a3", "Remove the blank symbol from the \u03a3 line."},
    {"E0016", "npda", "failed to build NPDA", "Fix the errors above first."},
    {"C0001", "turing", "nested configuration block", "Close each block with '# ///' first."},
    {"C0002", "turing", "invalid configuration line: missing '='", "Write lines as 'key = value'."},
    {"C0003", "turing", "empty configuration key", "Write a key before '='."},
    {"C0004", "turing", "invalid configuration key", "Use letters, numbers, underscores, and hyphens."},
    {"C0005", "turing", "duplicate configuration key", "Delete the repeated line."},
    {"C0006", "turing", "empty configuration value", "Write a value after '='."},
    {"C0007", "turing", "invalid value for 'num_tapes'", "Write a positive integer such as 1, 2, or 3."},
    {"C0009", "turing", "invalid value for 'tape_direction'", "Write 'bidirectional' or 'right-only'."},
    {"C0010", "turing", "invalid value for 'operation_mode'", "Write 'simultaneous' or 'independent'."},
    {"C0011", "turing", "invalid value for 'allow_stay'", "Write 'true' or 'false'."},
    {"C0012", "turing", "unknown configuration key", "Check the spelling against the known keys."},
    {"C0013", "turing", "unclosed configuration block", "Close the block with '# ///'."},
  };
  return table;
}

[[nodiscard]] inline std::vector<CodeInfo> explain(std::string_view code) {
  std::vector<CodeInfo> out;
  for (const auto& e : code_table())
    if (e.code == code)
      out.push_back(e);
  return out;
}

namespace ansi {
[[nodiscard]] inline constexpr std::string_view red(bool c) {
  return c ? "\x1b[31m" : "";
}
[[nodiscard]] inline constexpr std::string_view yellow(bool c) {
  return c ? "\x1b[33m" : "";
}
[[nodiscard]] inline constexpr std::string_view blue(bool c) {
  return c ? "\x1b[34m" : "";
}
[[nodiscard]] inline constexpr std::string_view green(bool c) {
  return c ? "\x1b[32m" : "";
}
[[nodiscard]] inline constexpr std::string_view bold(bool c) {
  return c ? "\x1b[1m" : "";
}
[[nodiscard]] inline constexpr std::string_view dim(bool c) {
  return c ? "\x1b[2m" : "";
}
[[nodiscard]] inline constexpr std::string_view reset(bool c) {
  return c ? "\x1b[0m" : "";
}
}  // namespace ansi

[[nodiscard]] inline constexpr std::string_view sev_str(Severity s) {
  switch (s) {
    case Severity::Error:
      return "error";
    case Severity::Warning:
      return "warning";
    case Severity::Note:
      return "note";
    case Severity::Help:
      return "help";
  }
  return "message";
}

[[nodiscard]] inline std::string_view sev_color(Severity s, bool color) {
  using namespace ansi;
  if (!color)
    return reset(false);

  switch (s) {
    case Severity::Error:
      return red(true);
    case Severity::Warning:
      return yellow(true);
    case Severity::Note:
      return blue(true);
    case Severity::Help:
      return green(true);
  }
  return reset(true);
}

inline void render_snippet(
  std::ostream& os,
  const SourceFile& src,
  const Diagnostic& d,
  const RenderOptions& opt
) {
  using namespace ansi;
  const bool color = resolve_color(opt.color);

  struct LineMarks {
    std::vector<const Label*> primary;
    std::vector<const Label*> secondary;
  };
  std::map<std::size_t, LineMarks> per_line;

  for (const auto& lab : d.labels) {
    // Spans never cross lines: the lexer splits tokens at newlines.
    // Each label belongs to exactly the line where it starts.
    const auto [start_line, unused_col] = src.line_col(lab.span.lo);
    (void)unused_col;
    auto& bucket = lab.primary ? per_line[start_line].primary : per_line[start_line].secondary;
    bucket.push_back(&lab);
  }

  std::size_t max_line_num_width = 2;
  if (!per_line.empty()) {
    max_line_num_width = std::max<std::size_t>(
      max_line_num_width, std::to_string(per_line.rbegin()->first + opt.context_lines).length()
    );
  }
  std::string pad(max_line_num_width, ' ');

  std::size_t printed_upto = 0;
  const auto print_plain = [&](std::size_t l) {
    std::string num = std::to_string(l);
    if (num.size() < max_line_num_width)
      num.insert(0, max_line_num_width - num.size(), ' ');
    os << ' ' << dim(color) << num << " │ " << reset(color) << src.line_view(l) << '\n';
  };

  const auto to_utf8 = [](std::u32string_view s) {
    std::string out;
    out.reserve(s.size() * 4);
    for (char32_t cp : s) {
      if (cp <= 0x7F) {
        out.push_back(static_cast<char>(cp));
      } else if (cp <= 0x7FF) {
        out.push_back(static_cast<char>(0xC0 | (cp >> 6)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
      } else if (cp <= 0xFFFF) {
        out.push_back(static_cast<char>(0xE0 | (cp >> 12)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
      } else {
        out.push_back(static_cast<char>(0xF0 | (cp >> 18)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
      }
    }
    return out;
  };

  constexpr char32_t primary_underline_cp = U'\u223F';
  constexpr char32_t primary_message_cp = U'\u2191';
  constexpr char32_t secondary_marker_cp = U'-';
  const std::string primary_message_utf8 = to_utf8(std::u32string_view(&primary_message_cp, 1));
  const std::string secondary_marker_utf8(1, '-');
  const std::string primary_underline_utf8 = to_utf8(std::u32string_view(&primary_underline_cp, 1));

  bool header_printed = false;
  for (const auto& [line, marks] : per_line) {
    const std::size_t lead = line > opt.context_lines ? line - opt.context_lines : 1;
    for (std::size_t l = std::max(printed_upto + 1, lead); l < line; ++l)
      print_plain(l);
    if (!header_printed) {
      const Label* first = nullptr;
      if (!marks.primary.empty())
        first = marks.primary.front();
      else if (!marks.secondary.empty())
        first = marks.secondary.front();

      auto [l, c] = src.line_col(first ? first->span.lo : 0);
      os << "  " << dim(color) << "-->" << reset(color) << " " << src.filename << ":" << l
         << ":" << c << '\n';
      header_printed = true;
    }

    auto line_sv = src.line_view(line);

    os << ' ' << pad << dim(color) << " │" << reset(color) << '\n';

    std::string line_number = std::to_string(line);
    if (line_number.size() < max_line_num_width)
      line_number.insert(0, max_line_num_width - line_number.size(), ' ');
    os << ' ' << dim(color) << line_number << " │ " << reset(color) << line_sv << '\n';

    std::u32string underline(line_sv.size(), U' ');
    auto ensure_length = [&](std::size_t size) {
      if (underline.size() < size)
        underline.resize(size, U' ');
    };

    auto place_marks = [&](const std::vector<const Label*>& labs, char32_t ch) {
      for (const auto* lb : labs) {
        const auto [sl, sc] = src.line_col(lb->span.lo);
        if (sl != line)
          continue;

        std::size_t start = sc ? sc - 1 : 0;
        std::size_t end = start + 1;
        if (!lb->span.empty()) {
          const auto [el, ec] = src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
          if (el == line)
            end = ec ? ec : start + 1;
        }

        if (end <= start)
          end = start + 1;

        ensure_length(end);
        for (std::size_t i = start; i < end; ++i)
          underline[i] = ch;
      }
    };

    place_marks(marks.secondary, secondary_marker_cp);
    place_marks(marks.primary, primary_underline_cp);

    while (!underline.empty() && underline.back() == U' ')
      underline.pop_back();

    if (!underline.empty()) {
      os << ' ' << pad << dim(color) << " │ " << reset(color);
      for (char32_t ch : underline) {
        if (ch == primary_underline_cp) {
          os << sev_color(d.severity, color) << primary_underline_utf8 << reset(color);
        } else if (ch == secondary_marker_cp) {
          os << blue(color) << secondary_marker_utf8 << reset(color);
        } else {
          os << to_utf8(std::u32string_view(&ch, 1));
        }
      }
      os << '\n';
    }

    auto print_msgs = [&](const std::vector<const Label*>& labs, bool primary) {
      for (const auto* lb : labs) {
        auto [msg_line, msg_col] = src.line_col(lb->span.lo);
        // Only print the message on the first line of the label (where it starts)
        if (msg_line != line)
          continue;

        os << ' ' << pad << dim(color) << " │ " << reset(color);
        for (std::size_t i = 0; i < (msg_col ? msg_col - 1 : 0); ++i)
          os << ' ';

        const std::string_view col_code =
          primary ? sev_color(d.severity, color) : blue(color);
        const std::string& marker = primary ? primary_message_utf8 : secondary_marker_utf8;
        os << col_code << marker << reset(color);
        if (!lb->message.empty())
          os << ' ' << lb->message;
        os << '\n';
      }
    };

    print_msgs(marks.secondary, false);
    print_msgs(marks.primary, true);
    printed_upto = line;
    for (std::size_t l = line + 1; l <= line + opt.context_lines && l <= src.line_count(); ++l) {
      print_plain(l);
      printed_upto = l;
    }
  }

  for (const auto& n : d.notes) {
    os << ' ' << pad << dim(color) << " = " << reset(color) << n << '\n';
  }
}

inline void render(
  std::ostream& os,
  const SourceFile& src,
  const Diagnostics& ds,
  const RenderOptions& opt = {}
) {
  using namespace ansi;
  const bool color = resolve_color(opt.color);

  // Drop exact duplicates: same code and span means one fault.
  std::vector<const Diagnostic*> shown;
  std::set<std::tuple<std::string_view, std::size_t, std::size_t, std::string_view>> seen;
  for (const auto& d : ds.items) {
    if (d.labels.empty()) {
      shown.push_back(&d);
      continue;
    }
    bool fresh = false;
    for (const auto& lab : d.labels)
      fresh |= seen.insert({d.code, lab.span.lo, lab.span.hi, lab.message}).second;
    if (fresh)
      shown.push_back(&d);
  }

  for (const auto* dp : shown) {
    const auto& d = *dp;
    os << sev_color(d.severity, color) << sev_str(d.severity) << reset(color);
    if (!d.code.empty()) {
      os << "[" << d.code << "]";
    }
    os << ": " << d.message << '\n';
    render_snippet(os, src, d, opt);
    os << '\n';
  }

  std::size_t errors = 0;
  for (const auto* dp : shown)
    if (dp->severity == Severity::Error)
      ++errors;
  if (errors > 1)
    os << sev_color(Severity::Error, color) << "error" << reset(color) << ": aborting due to "
       << errors << " previous errors\n";
}

}  // namespace diag

