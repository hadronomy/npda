// C++23. Minimal rustc-style diagnostics: spans, labels, rendering.

#pragma once

#include <algorithm>
#include <cstddef>
#include <map>
#include <ostream>
#include <string>
#include <string_view>
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
  std::map<std::size_t, LineMarks> per_line;

  for (const auto& lab : d.labels) {
    const auto line_info = src.line_col(lab.span.lo);
    auto& bucket =
      lab.primary ? per_line[line_info.first].primary : per_line[line_info.first].secondary;
    bucket.push_back(&lab);
  }

  std::size_t max_line_num_width = 2;
  if (!per_line.empty()) {
    max_line_num_width =
      std::max<std::size_t>(max_line_num_width, std::to_string(per_line.rbegin()->first).length());
  }
  std::string pad(max_line_num_width, ' ');

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

    os << ' ' << pad << dim(opt.color) << " │" << reset(opt.color) << '\n';

    std::string line_number = std::to_string(line);
    if (line_number.size() < max_line_num_width)
      line_number.insert(0, max_line_num_width - line_number.size(), ' ');
    os << ' ' << dim(opt.color) << line_number << " │ " << reset(opt.color) << line_sv << '\n';

    std::u32string underline(line_sv.size(), U' ');
    auto ensure_length = [&](std::size_t size) {
      if (underline.size() < size)
        underline.resize(size, U' ');
    };

    auto place_marks = [&](const std::vector<const Label*>& labs, char32_t ch) {
      for (const auto* lb : labs) {
        auto [start_line, start_col] = src.line_col(lb->span.lo);
        if (start_line != line)
          continue;
        std::size_t start = start_col ? start_col - 1 : 0;

        std::size_t end = start + 1;
        if (!lb->span.empty()) {
          std::size_t hi_index = lb->span.hi ? lb->span.hi - 1 : lb->span.hi;
          auto [end_line, end_col] = src.line_col(hi_index);
          if (end_line == line)
            end = end_col ? end_col : start + 1;
          else if (end_line > line)
            end = underline.size();
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
      os << ' ' << pad << dim(opt.color) << " │ " << reset(opt.color);
      for (char32_t ch : underline) {
        if (ch == primary_underline_cp) {
          os << sev_color(d.severity, opt.color) << primary_underline_utf8 << reset(opt.color);
        } else if (ch == secondary_marker_cp) {
          os << blue(opt.color) << secondary_marker_utf8 << reset(opt.color);
        } else {
          os << to_utf8(std::u32string_view(&ch, 1));
        }
      }
      os << '\n';
    }

    auto print_msgs = [&](const std::vector<const Label*>& labs, bool primary) {
      for (const auto* lb : labs) {
        auto [msg_line, msg_col] = src.line_col(lb->span.lo);
        if (msg_line != line)
          continue;

        os << ' ' << pad << dim(opt.color) << " │ " << reset(opt.color);
        for (std::size_t i = 0; i < (msg_col ? msg_col - 1 : 0); ++i)
          os << ' ';

        const std::string_view col_code =
          primary ? sev_color(d.severity, opt.color) : blue(opt.color);
        const std::string& marker = primary ? primary_message_utf8 : secondary_marker_utf8;
        os << col_code << marker << reset(opt.color);
        if (!lb->message.empty())
          os << ' ' << lb->message;
        os << '\n';
      }
    };

    print_msgs(marks.secondary, false);
    print_msgs(marks.primary, true);
  }

  for (const auto& n : d.notes) {
    os << ' ' << pad << dim(opt.color) << " = " << reset(opt.color) << n << '\n';
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