#include "diagnostics/render.h"

#include <unistd.h>

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <format>
#include <map>
#include <ostream>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "terminal/capabilities.h"

namespace diag {

namespace {

/// The number of source lines that the renderer shows before and after each
/// labeled line.
constexpr std::size_t kContextLines = 1;
constexpr std::size_t kTabWidth = 4;
/// The number of locations that a collapsed group lists. A count replaces the
/// other locations.
constexpr std::size_t kMaxLocationsListed = 3;

constexpr std::string_view kDim = "\x1b[2m";
constexpr std::string_view kReset = "\x1b[0m";
constexpr std::string_view kCurlyUnderline = "\x1b[4:3m";
constexpr std::string_view kUnderlineOff = "\x1b[59m\x1b[24m";

struct Glyphs {
  std::string_view rail;
  std::string_view arrow;
  std::string_view bar;
  std::string_view mark;
  std::string_view pointer;
  std::string_view branch;
  std::string_view last;
  std::string_view dash;
  std::string_view ellipsis;
};

constexpr Glyphs kUnicode{
    .rail = "█",
    .arrow = "-->",
    .bar = "│",
    .mark = "∿",
    .pointer = "↑",
    .branch = "├",
    .last = "╰",
    .dash = "──",
    .ellipsis = "…",
};

constexpr Glyphs kAscii{
    .rail = "!",
    .arrow = "-->",
    .bar = "|",
    .mark = "^",
    .pointer = "^",
    .branch = "|",
    .last = "`",
    .dash = "--",
    .ellipsis = "...",
};

/// What the output can use at its destination.
struct Features {
  bool color;
  const Glyphs* glyphs;
  /// OSC 8 hyperlinks from each location to its file.
  bool links;
  /// A curly underline below the labeled text in the source line.
  bool curly_underline;
};

Features Resolve(const RenderOptions& options) {
  const auto& terminal =
      terminal::CapabilitiesFor(terminal::OutputStream::kStderr);
  const bool color = options.color == ColorMode::kAlways ||
                     (options.color == ColorMode::kAuto && terminal.color);
  const bool unicode = options.charset == CharSet::kUnicode ||
                       (options.charset == CharSet::kAuto && terminal.utf8);
  return {
      .color = color,
      .glyphs = unicode ? &kUnicode : &kAscii,
      .links = terminal.tty,
      .curly_underline = color && terminal.tty && unicode,
  };
}

std::string_view SeverityName(Severity severity) {
  return severity == Severity::kError ? "error" : "warning";
}

std::string_view SeverityColor(Severity severity) {
  return severity == Severity::kError ? "\x1b[31m" : "\x1b[33m";
}

std::string_view UnderlineColor(Severity severity) {
  return severity == Severity::kError ? "\x1b[58;5;9m" : "\x1b[58;5;11m";
}

/// A source line with spaces in place of its tabs.
class ExpandedLine {
 public:
  explicit ExpandedLine(std::string_view line) {
    for (const char byte : line) {
      columns_.push_back(text_.size());
      if (byte == '\t') {
        text_.append(kTabWidth - text_.size() % kTabWidth, ' ');
      } else {
        text_.push_back(byte);
      }
    }
    columns_.push_back(text_.size());
  }

  [[nodiscard]] const std::string& Text() const { return text_; }

  /// Returns the display column where byte `byte` of the line starts.
  [[nodiscard]] std::size_t Column(std::size_t byte) const {
    return columns_[std::min(byte, columns_.size() - 1)];
  }

 private:
  std::string text_;
  std::vector<std::size_t> columns_;
};

/// The columns from `begin` to `end` that a label covers on its line. `end` is
/// not included.
struct Mark {
  std::size_t begin;
  std::size_t end;
  const Label* label;
};

/// Writes the diagnostics of one source file.
class Renderer {
 public:
  Renderer(std::ostream& out, const SourceFile& source, Features features)
      : out_(&out), source_(&source), features_(features) {}

  void WriteFull(const Diagnostic& diagnostic) const {
    WriteTitle(diagnostic, "");
    WriteExcerpt(diagnostic);
    *out_ << '\n';
  }

  /// Writes diagnostics with the same code as one title and a list of
  /// locations.
  void WriteCollapsed(std::span<const Diagnostic* const> group) const {
    const auto& glyphs = *features_.glyphs;
    WriteTitle(
        *group.front(),
        std::format(
            " ({} occurrences) {} {}",
            group.size(),
            glyphs.dash,
            source_->filename
        )
    );
    const auto listed = std::min(group.size(), kMaxLocationsListed);
    for (std::size_t index = 0; index < listed; ++index) {
      const auto& label = group[index]->labels.front();
      const bool last = index + 1 == listed && group.size() == listed;
      *out_ << std::format(
          "  {}{} {}",
          last ? glyphs.last : glyphs.branch,
          glyphs.dash,
          Location(label.span.lo)
      );
      if (!label.message.empty()) *out_ << ": " << label.message;
      *out_ << '\n';
    }
    if (group.size() > listed) {
      *out_ << std::format(
          "  {}{} {}(+{} more, -verbose shows all)\n",
          glyphs.last,
          glyphs.dash,
          glyphs.ellipsis,
          group.size() - listed
      );
    }
    *out_ << '\n';
  }

  void WriteAbort(std::size_t errors) const {
    *out_ << Paint(SeverityColor(Severity::kError), "error")
          << std::format(": aborting due to {} previous errors\n", errors);
  }

 private:
  [[nodiscard]] std::string Paint(
      std::string_view code,
      std::string_view text
  ) const {
    if (!features_.color) return std::string(text);
    return std::format("{}{}{}", code, text, kReset);
  }

  /// `file:line:column`. If the terminal can, the text is a link to the file.
  [[nodiscard]] std::string Location(std::size_t offset) const {
    const auto [line, column] = source_->LineColumn(offset);
    const auto& filename = source_->filename;
    const auto text = std::format("{}:{}:{}", filename, line, column);
    if (!features_.links) return text;
    char host[256] = {};
    std::error_code error;
    const auto path = std::filesystem::absolute(filename, error);
    if (gethostname(host, sizeof(host)) != 0 || error) return text;
    return std::format(
        "\x1b]8;;file://{}{}#{}\x1b\\{}\x1b]8;;\x1b\\",
        host,
        path.string(),
        line,
        text
    );
  }

  void WriteTitle(const Diagnostic& diagnostic, std::string_view suffix) const {
    const auto color = SeverityColor(diagnostic.severity);
    *out_ << Paint(color, features_.glyphs->rail) << ' '
          << Paint(color, SeverityName(diagnostic.severity));
    if (!diagnostic.code.empty()) *out_ << '[' << diagnostic.code << ']';
    *out_ << ": " << diagnostic.message << suffix << '\n';
  }

  /// Writes the labeled lines and the lines next to them, the marks and the
  /// message of each label, and the notes.
  void WriteExcerpt(const Diagnostic& diagnostic) const {
    std::map<std::size_t, std::vector<const Label*>> labels_by_line;
    for (const auto& label : diagnostic.labels) {
      labels_by_line[source_->LineColumn(label.span.lo).first].push_back(
          &label
      );
    }
    const auto last_line =
        labels_by_line.empty() ? 0 : labels_by_line.rbegin()->first;
    const auto width = std::max<std::size_t>(
        std::to_string(last_line + kContextLines).size(),
        2
    );
    if (!labels_by_line.empty()) {
      const auto& first = *labels_by_line.begin()->second.front();
      *out_ << "  " << Paint(kDim, features_.glyphs->arrow) << ' '
            << Location(first.span.lo) << '\n';
    }
    std::size_t written = 0;
    for (const auto& [line, labels] : labels_by_line) {
      const auto first_context =
          line > kContextLines ? line - kContextLines : 1;
      for (auto number = std::max(written + 1, first_context); number < line;
           ++number) {
        WriteSourceRow(
            number,
            width,
            ExpandedLine(source_->Line(number)).Text()
        );
      }
      WriteLabeledLine(diagnostic.severity, line, labels, width);
      written = line;
      for (auto number = line + 1;
           number <= line + kContextLines && number <= source_->LineCount();
           ++number) {
        WriteSourceRow(
            number,
            width,
            ExpandedLine(source_->Line(number)).Text()
        );
        written = number;
      }
    }
    for (const auto& note : diagnostic.notes) {
      *out_ << ' ' << std::string(width, ' ') << Paint(kDim, " = ") << note
            << '\n';
    }
  }

  void WriteLabeledLine(
      Severity severity,
      std::size_t line,
      std::span<const Label* const> labels,
      std::size_t width
  ) const {
    const ExpandedLine expanded(source_->Line(line));
    const auto marks = MarksOn(line, labels, expanded);
    *out_ << ' ' << std::string(width, ' ')
          << Paint(kDim, std::format(" {}", features_.glyphs->bar)) << '\n';
    WriteSourceRow(line, width, Underlined(expanded.Text(), marks, severity));
    WriteGutterRow(width, MarkRow(marks, severity));
    for (const auto& mark : marks) {
      std::string row =
          std::string(mark.begin, ' ') +
          Paint(SeverityColor(severity), features_.glyphs->pointer);
      if (!mark.label->message.empty()) row += " " + mark.label->message;
      WriteGutterRow(width, row);
    }
  }

  [[nodiscard]] std::vector<Mark> MarksOn(
      std::size_t line,
      std::span<const Label* const> labels,
      const ExpandedLine& expanded
  ) const {
    std::vector<Mark> marks;
    const auto line_start = source_->line_starts[line - 1];
    const auto line_end = line_start + source_->Line(line).size();
    for (const auto* label : labels) {
      const auto begin = label->span.lo - line_start;
      const auto end = std::min(label->span.hi, line_end) - line_start;
      const auto begin_column = expanded.Column(begin);
      marks.push_back({
          .begin = begin_column,
          .end = std::max(expanded.Column(end), begin_column + 1),
          .label = label,
      });
    }
    return marks;
  }

  [[nodiscard]] std::string MarkRow(
      std::span<const Mark> marks,
      Severity severity
  ) const {
    std::size_t width = 0;
    for (const auto& mark : marks) width = std::max(width, mark.end);
    std::vector<bool> marked(width, false);
    for (const auto& mark : marks) {
      std::fill(marked.begin() + mark.begin, marked.begin() + mark.end, true);
    }
    std::string row;
    for (const bool cell : marked) {
      row += cell ? Paint(SeverityColor(severity), features_.glyphs->mark)
                  : std::string(" ");
    }
    return row;
  }

  [[nodiscard]] std::string Underlined(
      const std::string& text,
      std::vector<Mark> marks,
      Severity severity
  ) const {
    if (!features_.curly_underline) return text;
    std::ranges::sort(marks, {}, &Mark::begin);
    std::string underlined;
    std::size_t position = 0;
    for (const auto& mark : marks) {
      if (mark.begin < position || mark.begin >= text.size()) continue;
      const auto end = std::min(mark.end, text.size());
      underlined += text.substr(position, mark.begin - position);
      underlined += std::format(
          "{}{}{}{}",
          kCurlyUnderline,
          UnderlineColor(severity),
          text.substr(mark.begin, end - mark.begin),
          kUnderlineOff
      );
      position = end;
    }
    return underlined + text.substr(position);
  }

  void WriteSourceRow(
      std::size_t number,
      std::size_t width,
      std::string_view text
  ) const {
    const auto gutter =
        std::format("{:>{}} {} ", number, width, features_.glyphs->bar);
    *out_ << ' ' << Paint(kDim, gutter) << text << '\n';
  }

  void WriteGutterRow(std::size_t width, std::string_view text) const {
    *out_ << ' ' << std::string(width, ' ')
          << Paint(kDim, std::format(" {} ", features_.glyphs->bar)) << text
          << '\n';
  }

  std::ostream* out_;
  const SourceFile* source_;
  Features features_;
};

/// Removes repeats. Diagnostics with the same code, spans, and messages are one
/// problem.
std::vector<const Diagnostic*> Distinct(const DiagnosticSet& diagnostics) {
  std::vector<const Diagnostic*> distinct;
  std::set<
      std::tuple<std::string_view, std::size_t, std::size_t, std::string_view>>
      seen;
  for (const auto& diagnostic : diagnostics.items) {
    bool fresh = diagnostic.labels.empty();
    for (const auto& label : diagnostic.labels) {
      fresh |= seen.insert({
                               diagnostic.code,
                               label.span.lo,
                               label.span.hi,
                               label.message,
                           })
                   .second;
    }
    if (fresh) distinct.push_back(&diagnostic);
  }
  return distinct;
}

/// Groups diagnostics by severity and code, in the order of their first
/// appearance.
std::vector<std::vector<const Diagnostic*>> GroupByCode(
    std::span<const Diagnostic* const> diagnostics
) {
  std::vector<std::vector<const Diagnostic*>> groups;
  std::unordered_map<std::string, std::size_t> group_of;
  for (const auto* diagnostic : diagnostics) {
    const auto key = std::format(
        "{}:{}",
        SeverityName(diagnostic->severity),
        diagnostic->code
    );
    const auto [entry, added] = group_of.try_emplace(key, groups.size());
    if (added) groups.emplace_back();
    groups[entry->second].push_back(diagnostic);
  }
  return groups;
}

}  // namespace

void Render(
    std::ostream& output,
    const SourceFile& source,
    const DiagnosticSet& diagnostics,
    const RenderOptions& options
) {
  const Renderer renderer(output, source, Resolve(options));
  const auto distinct = Distinct(diagnostics);
  for (const auto& group : GroupByCode(distinct)) {
    const bool collapse =
        group.size() > 1 && !options.verbose && !group.front()->labels.empty();
    if (collapse) {
      renderer.WriteCollapsed(group);
      continue;
    }
    for (const auto* diagnostic : group) renderer.WriteFull(*diagnostic);
  }
  const auto errors = std::ranges::count_if(distinct, [](const Diagnostic* d) {
    return d->severity == Severity::kError;
  });
  if (errors > 1) renderer.WriteAbort(static_cast<std::size_t>(errors));
}

}  // namespace diag
