// C++23. Minimal rustc-style diagnostics: spans, labels, rendering.

#pragma once

#include <algorithm>
#include <cstddef>
#include <iomanip>
#include <iostream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace diag {

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

struct RenderOptions {
  bool color = true;
  std::size_t context_lines = 0;
};

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

  struct LineMarks {
    std::vector<const Label*> primary;
    std::vector<const Label*> secondary;
  };
  std::unordered_map<std::size_t, LineMarks> per_line;

  for (const auto& lab : d.labels) {
    auto [l, c] = src.line_col(lab.span.lo);
    (lab.primary ? per_line[l].primary : per_line[l].secondary).push_back(&lab);
  }

  // Determine the maximum line number length to establish consistent padding
  std::size_t max_line_num_width = 0;
  if (!per_line.empty()) {
    std::size_t max_line = 0;
    for (const auto& [line_num, _] : per_line) {
      if (line_num > max_line) {
        max_line = line_num;
      }
    }
    max_line_num_width = std::to_string(max_line).length();
  }
  if (max_line_num_width < 2) {
    max_line_num_width = 2;  // Minimum 2 for consistency
  }

  bool header_printed = false;
  for (const auto& [line, marks] : per_line) {
    if (!header_printed) {
      const Label* first = nullptr;
      if (!marks.primary.empty())
        first = marks.primary.front();
      else if (!marks.secondary.empty())
        first = marks.secondary.front();

      auto [l, c] = src.line_col(first ? first->span.lo : 0);
      os << "  " << dim(opt.color) << "-->" << reset(opt.color) << " " << src.filename << ":" << l
         << ":" << c << '\n';
      header_printed = true;
    }

    auto line_sv = src.line_view(line);
    os << " " << std::setw(max_line_num_width) << "" << dim(opt.color) << " │" << reset(opt.color)
       << '\n';
    os << " " << dim(opt.color) << std::right << std::setw(max_line_num_width) << line << " │ "
       << reset(opt.color) << line_sv << '\n';

    // Now, for the underline, we need to consider the column where the highlight starts.
    // We get the first primary label (or any label if no primary) to determine the starting column
    std::size_t first_label_col_on_line = 0;
    const Label* representative_label = nullptr;
    if (!marks.primary.empty()) {
      representative_label = marks.primary.front();
    } else if (!marks.secondary.empty()) {
      representative_label = marks.secondary.front();
    }

    if (representative_label) {
      first_label_col_on_line = src.line_col(representative_label->span.lo).second;
    }

    // Pad to align the `│` and then pad further to the label's start column
    os << " " << std::setw(max_line_num_width) << "" << dim(opt.color) << " │" << reset(opt.color);

    // We print spaces to align the underline.
    // The underline itself will be printed on a new line, so we need to carry over the alignment.
    os << '\n';  // Move to the next line for the underline

    os << " " << std::setw(max_line_num_width) << ""  // Space for line number
       << "   ";                                      // space | space - this is 3 characters.

    // Pad to the column of the highlight within the line
    for (std::size_t i = 0; i < (first_label_col_on_line - 1); ++i) {
      os << ' ';
    }

    std::string underline(line_sv.size(), ' ');  // Initialize with spaces

    auto place_marks = [&](const std::vector<const Label*>& labs, char ch) {
      for (const auto* lb : labs) {
        auto [l0, c0] = src.line_col(lb->span.lo);
        std::size_t hi_pos = lb->span.hi;
        if (hi_pos == 0 && !lb->span.empty())
          hi_pos = lb->span.lo + 1;
        else if (hi_pos == 0 && lb->span.empty())
          hi_pos = lb->span.lo;

        auto [l1, c1] = src.line_col(hi_pos ? hi_pos - 1 : hi_pos);

        if (l0 != line)
          continue;
        std::size_t start = c0 ? c0 - 1 : 0;  // 0-indexed start column
        std::size_t end = (l1 == line) ? (c1 ? c1 : c0) : line_sv.size();

        if (end < start)
          std::swap(start, end);
        end = std::min(end, line_sv.size());
        for (std::size_t i = start; i < end; ++i)
          underline[i] = ch;
        if (start == end && start < underline.size()) {
          underline[start] = ch;
        }
      }
    };

    place_marks(marks.secondary, '-');
    place_marks(marks.primary, '^');

    os << underline << '\n';

    auto print_msgs = [&](const std::vector<const Label*>& labs, bool primary) {
      for (const auto* lb : labs) {
        auto [l0, c0] = src.line_col(lb->span.lo);
        std::size_t start_col_0_indexed = c0 ? c0 - 1 : 0;

        os << " " << std::setw(max_line_num_width) << ""
           << "   ";

        // Pad to the start column of the label
        for (std::size_t i = 0; i < start_col_0_indexed; ++i)
          os << ' ';

        const std::string_view col_code =
          primary ? sev_color(Severity::Error, opt.color) : blue(opt.color);
        os << col_code << (primary ? "^" : "-") << reset(opt.color) << " " << lb->message << '\n';
      }
    };

    print_msgs(marks.secondary, false);
    print_msgs(marks.primary, true);
  }

  for (const auto& n : d.notes) {
    os << " " << std::setw(max_line_num_width) << "" << dim(opt.color) << " = " << reset(opt.color)
       << n << '\n';
  }
}

inline void render(
  std::ostream& os,
  const SourceFile& src,
  const Diagnostics& ds,
  const RenderOptions& opt = {}
) {
  using namespace ansi;

  for (const auto& d : ds.items) {
    os << sev_color(d.severity, opt.color) << sev_str(d.severity) << reset(opt.color);
    if (!d.code.empty()) {
      os << "[" << d.code << "]";
    }
    os << ": " << d.message << '\n';
    render_snippet(os, src, d, opt);
    os << '\n';
  }
}

}  // namespace diag