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
  std::string file{};  // empty means the source under render
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
  int order = 0;  // paint sequence when labels share a line
};

struct Diagnostic {
  Severity severity = Severity::Error;
  std::string code;
  std::string message;
  std::vector<Label> labels;
  std::vector<std::string> notes;
  // File key for multi-source renders. Empty means the source under render.
  [[nodiscard]] std::string_view file() const {
    for (const auto& lab : labels)
      if (!lab.span.file.empty())
        return lab.span.file;
    return {};
  }
};

class DiagnosticBuilder;

struct Diagnostics {
  std::vector<Diagnostic> items;
  [[nodiscard]] bool has_errors() const {
    return std::any_of(items.begin(), items.end(), [](const Diagnostic& d) {
      return d.severity == Severity::Error;
    });
  }

  // Zero-friction emit: dx.emit(diag::error("E1", "bad").label(span, "here")).
  void emit(DiagnosticBuilder builder);
};

// Owns named sources for multi-file renders. Memoizes parsed files.
// Heterogeneous lookup takes string_view keys with no allocation.
struct TransparentHash {
  using is_transparent = void;
  [[nodiscard]] std::size_t operator()(std::string_view s) const noexcept {
    return std::hash<std::string_view>{}(s);
  }
};

struct SourceCache {
  std::unordered_map<std::string, SourceFile, TransparentHash, std::equal_to<>> files;

  const SourceFile* find(std::string_view name) const {
    const auto it = files.find(name);
    return it == files.end() ? nullptr : &it->second;
  }

  const SourceFile& insert(SourceFile sf) {
    const auto [it, _] = files.insert_or_assign(sf.filename, std::move(sf));
    return it->second;
  }
};

// Color output control. Auto follows NO_COLOR and terminal detection.
enum class ColorMode { Auto, Always, Never };

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

// Character set control. Auto follows the locale probe.
enum class CharSet { Auto, Unicode, Ascii };

// One slot per glyph the renderer paints. Same layout, two alphabets.
struct Glyphs {
  const char* vbar;      // │ |
  const char* arrow;     // -->
  char32_t under;        // ∿ ^
  char32_t msg;          // ↑ ^
  char32_t sec;          // - -
  const char* mtop;      // ╭ `
  const char* mmid;      // ├ |
  const char* mbot;      // ╰ `
  const char* mhead;     // ▶ >
  const char* rail;      // █ !
  const char* ellipsis;  // … ...
  const char* ddash;     // ── --
};

[[nodiscard]] inline const Glyphs& glyphs_for(bool unicode) {
  static constexpr Glyphs uni{
    "│", "-->", U'∿', U'↑', U'-', "╭", "├", "╰", "▶", "█", "…", "──"
  };
  static constexpr Glyphs asc{
    "|", "-->", U'^', U'^', U'-', "`", "|", "`", ">", "!", "...", "--"
  };
  return unicode ? uni : asc;
}

// Terminal capability probe. Runs once per process.
struct Terminal {
  bool tty = false;
  bool utf8 = true;
};

[[nodiscard]] inline const Terminal& probe_terminal() {
  static const Terminal t = [] {
    Terminal r;
    r.tty = isatty(STDERR_FILENO) != 0;
    std::string loc;
    for (const char* key : {"LC_ALL", "LC_CTYPE", "LANG"}) {
      if (const char* v = std::getenv(key); v != nullptr && v[0] != '\0') {
        loc = v;
        break;
      }
    }
    if (!loc.empty()) {
      for (auto& c : loc)
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
      r.utf8 = loc.find("utf-8") != std::string::npos || loc.find("utf8") != std::string::npos;
    }
    return r;
  }();
  return t;
}

struct RenderOptions {
  ColorMode color = ColorMode::Auto;
  CharSet charset = CharSet::Auto;
  std::size_t context_lines = 1;
  std::size_t tab_width = 4;
  bool links = true;         // file:// links; needs a TTY
  bool inline_marks = true;  // curly underline on the source row; needs color TTY
  bool verbose = false;      // expand same-code groups
};

// Resolved per render call. The paint code reads this, never the environment.
struct Resolved {
  bool color = false;
  bool unicode = true;
  const Glyphs* g = &glyphs_for(true);
  bool links = false;
  bool marks = false;
};

[[nodiscard]] inline Resolved resolve_opt(const RenderOptions& opt) {
  const Terminal& t = probe_terminal();
  const bool color = resolve_color(opt.color);
  const bool unicode =
    opt.charset == CharSet::Unicode || (opt.charset == CharSet::Auto && t.utf8);
  Resolved r;
  r.color = color;
  r.unicode = unicode;
  r.g = &glyphs_for(unicode);
  r.links = opt.links && t.tty;
  r.marks = opt.inline_marks && color && t.tty && unicode;
  return r;
}

// Underline color per severity. Mirrors the foreground palette, brightened.
[[nodiscard]] inline std::string_view sev_under_color(Severity s, bool color) {
  if (!color)
    return "";
  switch (s) {
    case Severity::Error:
      return "\x1b[58;5;9m";
    case Severity::Warning:
      return "\x1b[58;5;11m";
    case Severity::Note:
      return "\x1b[58;5;12m";
    case Severity::Help:
      return "\x1b[58;5;10m";
  }
  return "";
}

// Expand tabs to display columns. map[i] holds the display column of byte i.
[[nodiscard]] inline std::string expand_tabs(
  std::string_view line, std::size_t tab_width, std::vector<std::size_t>& map
) {
  if (tab_width == 0)
    tab_width = 1;
  std::string out;
  out.reserve(line.size());
  map.resize(line.size() + 1);
  std::size_t col = 0;
  for (std::size_t i = 0; i < line.size(); ++i) {
    map[i] = col;
    if (line[i] == '\t') {
      const std::size_t next = ((col / tab_width) + 1) * tab_width;
      out.append(next - col, ' ');
      col = next;
    } else {
      out.push_back(line[i]);
      ++col;
    }
  }
  map[line.size()] = col;
  return out;
}

// Wrap a location in an OSC 8 hyperlink. Falls back to plain text.
[[nodiscard]] inline std::string link_location(  std::string_view filename, std::size_t line, std::size_t col, bool active
) {
  const std::string text =
    std::string(filename) + ":" + std::to_string(line) + ":" + std::to_string(col);
  if (!active)
    return text;
  char host[256] = {};
  if (gethostname(host, sizeof(host)) != 0)
    return text;
  std::error_code ec;
  const std::string abs = std::filesystem::absolute(filename, ec).string();
  if (ec)
    return text;
  return "\x1b]8;;file://" + std::string(host) + abs + "#" + std::to_string(line) + "\x1b\\"
       + text + "\x1b]8;;\x1b\\";
}


// Builds one diagnostic step by step. Emits nothing until emit().
class DiagnosticBuilder {
 public:
  friend struct Diagnostics;
  DiagnosticBuilder(std::string code, std::string message, Severity severity = Severity::Error) {
    d_.severity = severity;
    d_.code = std::move(code);
    d_.message = std::move(message);
  }
  DiagnosticBuilder& label(
    Span span, std::string message, bool primary = true, int order = 0
  ) {
    d_.labels.push_back(
      Label{.span = std::move(span), .primary = primary, .message = std::move(message), .order = order}
    );
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

inline void Diagnostics::emit(DiagnosticBuilder builder) {
  items.push_back(std::move(builder.d_));
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

// Ephemeral status line for long runs. Prints nothing when piped,
// when the locale is not UTF-8, or when the work finishes fast.
// Stop it with dismiss() before printing diagnostics, or finish()
// with a final static line. Destructor stops silently.
class Activity {
 public:
  explicit Activity(std::string message, std::ostream& os = std::cerr)
      : os_(os), message_(std::move(message)) {
    const Terminal& t = probe_terminal();
    if (!t.tty || !t.utf8)
      return;
    worker_ = std::jthread([this] { run(); });
  }

  ~Activity() { dismiss(); }
  Activity(const Activity&) = delete;
  Activity& operator=(const Activity&) = delete;
  Activity(Activity&&) = delete;
  Activity& operator=(Activity&&) = delete;

  void set_message(std::string message) {
    std::lock_guard lock(mutex_);
    message_ = std::move(message);
  }

  void dismiss() {
    stop_requested_.store(true);
    cv_.notify_all();
    if (worker_.joinable()) {
      worker_.request_stop();
      worker_.join();
    }
    clear();
  }

  void finish(const std::string& done) {
    const bool had_worker = worker_.joinable();
    dismiss();
    // Silent unless a frame painted: fast and piped runs stay clean.
    if (finished_ || !had_worker || !painted_)
      return;
    finished_ = true;
    os_ << ansi::green(color_) << "✓" << ansi::reset(color_) << " " << done << "\n" << std::flush;
  }

 private:
  void clear() {
    std::lock_guard lock(mutex_);
    if (!active_)
      return;
    active_ = false;
    os_ << "\r\x1b[2K\x1b[?25h" << std::flush;
  }

  void run() {
    // Grace period: fast work prints nothing at all.
    {
      std::unique_lock lock(mutex_);
      if (cv_.wait_for(lock, std::chrono::milliseconds(150), [&] {
            return stop_requested_.load();
          }))
        return;
    }
    {
      std::lock_guard lock(mutex_);
      active_ = true;
      painted_ = true;
      os_ << "\x1b[?25l" << std::flush;
    }
    static constexpr const char* kFrames[] = {
      "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"
    };
    std::size_t frame = 0;
    while (true) {
      std::string msg;
      {
        std::lock_guard lock(mutex_);
        msg = message_;
      }
      os_ << "\x1b[?2026h\r\x1b[2K" << ansi::dim(color_) << kFrames[frame % 10]
          << ansi::reset(color_) << " " << msg << "\x1b[?2026l" << std::flush;
      frame++;
      std::unique_lock lock(mutex_);
      if (cv_.wait_for(lock, std::chrono::milliseconds(80), [&] {
            return stop_requested_.load();
          }))
        return;
    }
  }

  std::ostream& os_;
  std::string message_;
  const bool color_ = color_enabled();
  std::mutex mutex_;
  std::condition_variable_any cv_;
  std::atomic<bool> stop_requested_{false};
  bool active_ = false;
  bool painted_ = false;
  bool finished_ = false;
  std::jthread worker_;
};

inline void render_snippet(
  std::ostream& os,
  const SourceFile& src,
  const Diagnostic& d,
  const RenderOptions& opt,
  const Resolved& r
) {
  using namespace ansi;
  const bool color = r.color;
  const Glyphs& g = *r.g;

  struct LineMarks {
    std::vector<const Label*> primary;
    std::vector<const Label*> secondary;
  };
  std::map<std::size_t, LineMarks> per_line;

  // Paint order decides ties when labels share a line. Stable sort keeps
  // emission order for equal values.
  std::vector<const Label*> ordered;
  ordered.reserve(d.labels.size());
  for (const auto& lab : d.labels)
    ordered.push_back(&lab);
  std::stable_sort(ordered.begin(), ordered.end(), [](const Label* a, const Label* b) {
    return a->order < b->order;
  });

  // Line range per label, in paint order. Multi-line spans paint a gutter.
  struct Range {
    std::size_t sl;
    std::size_t el;
  };
  std::vector<Range> ranges;
  ranges.reserve(ordered.size());

  for (const auto* lab : ordered) {
    const auto [sl, unused_col] = src.line_col(lab->span.lo);
    (void)unused_col;
    std::size_t el = sl;
    if (!lab->span.empty()) {
      const auto [ell, unused_col2] =
        src.line_col(lab->span.hi ? lab->span.hi - 1 : lab->span.hi);
      (void)unused_col2;
      el = ell;
    }
    ranges.push_back({sl, el});
    for (std::size_t line = sl; line <= el; ++line) {
      auto& bucket = lab->primary ? per_line[line].primary : per_line[line].secondary;
      bucket.push_back(lab);
    }
  }

  const bool multiline = std::any_of(ranges.begin(), ranges.end(), [](const Range& rg) {
    return rg.el > rg.sl;
  });

  // Gutter mark for one line. Empty for single-line diagnostics.
  const auto gutter_for = [&](std::size_t l) -> std::string {
    if (!multiline)
      return {};
    bool start = false;
    bool end = false;
    bool inside = false;
    for (const auto& rg : ranges) {
      if (l == rg.sl)
        start = true;
      else if (l == rg.el)
        end = true;
      else if (l > rg.sl && l < rg.el)
        inside = true;
    }
    if (start)
      return std::string(g.mtop) + " ";
    if (end)
      return std::string(g.mbot) + " ";
    if (inside)
      return std::string(g.vbar) + " ";
    return "  ";
  };

  std::size_t max_line_num_width = 2;
  if (!per_line.empty()) {
    max_line_num_width = std::max<std::size_t>(
      max_line_num_width, std::to_string(per_line.rbegin()->first + opt.context_lines).length()
    );
  }
  std::string pad(max_line_num_width, ' ');

  std::size_t printed_upto = 0;
  std::vector<std::size_t> colmap;
  const auto print_plain = [&](std::size_t l) {
    std::string num = std::to_string(l);
    if (num.size() < max_line_num_width)
      num.insert(0, max_line_num_width - num.size(), ' ');
    const std::string shown = expand_tabs(src.line_view(l), opt.tab_width, colmap);
    os << ' ' << gutter_for(l) << dim(color) << num << " " << g.vbar << " " << reset(color)
       << shown << '\n';
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
  const std::string under_utf8 = to_utf8(std::u32string_view(&g.under, 1));
  const std::string msg_utf8 = to_utf8(std::u32string_view(&g.msg, 1));
  const std::string sec_utf8 = to_utf8(std::u32string_view(&g.sec, 1));

  // Byte column to display column. Clamps past the end.
  const auto dcol = [&](std::size_t b) {
    return b < colmap.size() ? colmap[b] : colmap.back();
  };

  bool header_printed = false;
  for (const auto& [line, marks] : per_line) {
    if (!header_printed) {
      const Label* first = nullptr;
      if (!marks.primary.empty())
        first = marks.primary.front();
      else if (!marks.secondary.empty())
        first = marks.secondary.front();

      auto [l, c] = src.line_col(first ? first->span.lo : 0);
      os << "  " << dim(color) << g.arrow << reset(color) << " "
         << link_location(src.filename, l, c, r.links) << '\n';
      header_printed = true;
    }
    const std::size_t lead = line > opt.context_lines ? line - opt.context_lines : 1;
    for (std::size_t l = std::max(printed_upto + 1, lead); l < line; ++l)
      print_plain(l);

    const std::string shown = expand_tabs(src.line_view(line), opt.tab_width, colmap);

    os << ' ' << gutter_for(line) << pad << dim(color) << " " << g.vbar << reset(color) << '\n';

    std::string line_number = std::to_string(line);
    if (line_number.size() < max_line_num_width)
      line_number.insert(0, max_line_num_width - line_number.size(), ' ');

    // Curly underline on the source row for primary single-line labels.
    std::string painted = shown;
    if (r.marks) {
      struct Seg {
        std::size_t s;
        std::size_t e;
        [[nodiscard]] bool operator<(const Seg& o) const { return s < o.s; }
      };
      std::vector<Seg> segs;
      for (const auto* lb : marks.primary) {
        const auto [sl, sc] = src.line_col(lb->span.lo);
        if (sl != line || lb->span.empty())
          continue;
        const auto [el, ec] = src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
        if (el != line)
          continue;
        segs.push_back({dcol(sc ? sc - 1 : 0), dcol(ec ? ec : 0)});
      }
      std::sort(segs.begin(), segs.end());
      std::string acc;
      acc.reserve(painted.size() + segs.size() * 16);
      std::size_t pos = 0;
      const std::string open =
        std::string("\x1b[4:3m") + std::string(sev_under_color(d.severity, true));
      for (const auto& sg : segs) {
        if (sg.s < pos || sg.e <= sg.s)
          continue;
        acc.append(painted, pos, sg.s - pos);
        acc += open;
        acc.append(painted, sg.s, std::min(sg.e, painted.size()) - sg.s);
        acc += "\x1b[59m\x1b[24m";
        pos = std::min(sg.e, painted.size());
      }
      acc.append(painted, pos, std::string::npos);
      painted = std::move(acc);
    }

    os << ' ' << gutter_for(line) << dim(color) << line_number << " " << g.vbar << " "
       << reset(color) << painted << '\n';

    // Mark kinds per display column: 1 primary, 2 secondary.
    std::vector<char> kind(colmap.back() + 1, 0);
    auto place_marks = [&](const std::vector<const Label*>& labs, char kindv) {
      for (const auto* lb : labs) {
        const auto [sl, sc] = src.line_col(lb->span.lo);
        std::size_t el = sl;
        std::size_t ec = 0;
        if (!lb->span.empty()) {
          const auto [ell, ecc] = src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
          el = ell;
          ec = ecc;
        }
        if (line < sl || line > el)
          continue;
        if (sl != el && line != sl && line != el)
          continue;  // gutter covers middle lines
        const std::size_t bstart = (line == sl) ? (sc ? sc - 1 : 0) : 0;
        std::size_t bend;
        if (line == el)
          bend = (!lb->span.empty() && ec) ? ec : bstart + 1;
        else
          bend = src.line_view(line).size();  // start row of multi-line: mark to EOL
        std::size_t ds = dcol(bstart);
        std::size_t de = dcol(bend);
        if (de <= ds)
          de = ds + 1;
        if (de > kind.size())
          kind.resize(de, 0);
        for (std::size_t i = ds; i < de; ++i)
          kind[i] = kindv;
      }
    };

    place_marks(marks.secondary, 2);
    place_marks(marks.primary, 1);

    while (!kind.empty() && kind.back() == 0)
      kind.pop_back();

    if (!kind.empty()) {
      os << ' ' << gutter_for(line) << pad << dim(color) << " " << g.vbar << " " << reset(color);
      for (char k : kind) {
        if (k == 1) {
          os << sev_color(d.severity, color) << under_utf8 << reset(color);
        } else if (k == 2) {
          os << blue(color) << sec_utf8 << reset(color);
        } else {
          os << ' ';
        }
      }
      os << '\n';
    }

    // Messages attach at the end row of each span.
    auto print_msgs = [&](const std::vector<const Label*>& labs, bool primary) {
      for (const auto* lb : labs) {
        const auto [sl, sc] = src.line_col(lb->span.lo);
        std::size_t el = sl;
        std::size_t ec = 0;
        if (!lb->span.empty()) {
          const auto [ell, ecc] = src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
          el = ell;
          ec = ecc;
        }
        if (el != line)
          continue;

        std::size_t indent = (line == sl) ? dcol(sc ? sc - 1 : 0) : dcol(ec ? ec : 0);
        os << ' ' << gutter_for(line) << pad << dim(color) << " " << g.vbar << " " << reset(color);
        for (std::size_t i = 0; i < indent; ++i)
          os << ' ';

        const std::string_view col_code =
          primary ? sev_color(d.severity, color) : blue(color);
        const std::string& marker = primary ? msg_utf8 : sec_utf8;
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
    os << ' ' << gutter_for(printed_upto) << pad << dim(color) << " = " << reset(color) << n
       << '\n';
  }
}

inline void render_group(
  std::ostream& os,
  const SourceFile& src,
  const std::vector<const Diagnostic*>& group,
  const RenderOptions& opt,
  const Resolved& r
) {
  using namespace ansi;
  for (const auto* dp : group) {
    const auto& d = *dp;
    os << sev_color(d.severity, r.color) << r.g->rail << reset(r.color) << " "
       << sev_color(d.severity, r.color) << sev_str(d.severity) << reset(r.color);
    if (!d.code.empty()) {
      os << "[" << d.code << "]";
    }
    os << ": " << d.message << '\n';
    render_snippet(os, src, d, opt, r);
    os << '\n';
  }
}

inline void render_core(
  std::ostream& os,
  const std::vector<std::pair<const SourceFile*, std::vector<const Diagnostic*>>>& groups,
  const RenderOptions& opt
) {
  using namespace ansi;
  const Resolved r = resolve_opt(opt);
  const Glyphs& g = *r.g;

  // Collapse same-code runs on one file. Verbose mode expands them.
  struct Collapse {
    std::string key;
    std::vector<const Diagnostic*> members;
  };
  for (const auto& [src, group] : groups) {
    std::vector<Collapse> collapses;
    std::unordered_map<std::string, std::size_t, TransparentHash, std::equal_to<>> index;
    for (const auto* dp : group) {
      const std::string key = std::string(sev_str(dp->severity)) + '\0' + dp->code + '\0'
                            + std::string(dp->file());
      const auto it = index.find(key);
      if (it == index.end()) {
        index.emplace(key, collapses.size());
        collapses.push_back({key, {dp}});
      } else {
        collapses[it->second].members.push_back(dp);
      }
    }
    // Emit in first-appearance order. Runs of one stay full renders.
    for (auto& c : collapses) {
      if (c.members.size() < 2 || opt.verbose || c.members.front()->labels.empty()) {
        render_group(os, *src, c.members, opt, r);
        continue;
      }
      const Diagnostic& first = *c.members.front();
      const std::string file =
        std::string(first.file().empty() ? std::string_view(src->filename) : first.file());
      os << sev_color(first.severity, r.color) << g.rail << reset(r.color) << " "
         << sev_color(first.severity, r.color) << sev_str(first.severity) << reset(r.color);
      if (!first.code.empty())
        os << "[" << first.code << "]";
      os << ": " << first.message << " (" << c.members.size() << " occurrences) " << g.ddash
         << " " << file << '\n';
      const std::size_t shown_n = std::min<std::size_t>(c.members.size(), 3);
      for (std::size_t i = 0; i < shown_n; ++i) {
        const Diagnostic& m = *c.members[i];
        const Label* pick = nullptr;
        for (const auto& lab : m.labels)
          if (lab.primary) {
            pick = &lab;
            break;
          }
        if (pick == nullptr)
          pick = &m.labels.front();
        const auto [l, co] = src->line_col(pick->span.lo);
        const bool last = (i + 1 == shown_n) && c.members.size() <= 3;
        os << "  " << (last ? g.mbot : g.mmid) << g.ddash << " "
           << link_location(file, l, co, r.links);
        if (!pick->message.empty())
          os << ": " << pick->message;
        os << '\n';
      }
      if (c.members.size() > 3)
        os << "  " << g.mbot << g.ddash << " " << g.ellipsis << "(+" << c.members.size() - 3
           << " more, --verbose shows all)\n";
      os << '\n';
    }
  }

  std::size_t errors = 0;
  for (const auto& [src, group] : groups)
    for (const auto* dp : group)
      if (dp->severity == Severity::Error)
        ++errors;
  if (errors > 1)
    os << sev_color(Severity::Error, r.color) << "error" << reset(r.color)
       << ": aborting due to " << errors << " previous errors\n";
}

inline void render(
  std::ostream& os,
  const SourceFile& src,
  const Diagnostics& ds,
  const RenderOptions& opt = {}
) {
  using namespace ansi;

  // Drop exact duplicates: same code and span means one fault.
  std::vector<const Diagnostic*> shown;
  std::set<std::tuple<std::string_view, std::string_view, std::size_t, std::size_t, std::string_view>>
    seen;
  for (const auto& d : ds.items) {
    if (d.labels.empty()) {
      shown.push_back(&d);
      continue;
    }
    bool fresh = false;
    for (const auto& lab : d.labels)
      fresh |=
        seen.insert({d.code, lab.span.file, lab.span.lo, lab.span.hi, lab.message}).second;
    if (fresh)
      shown.push_back(&d);
  }

  render_core(os, {{&src, std::move(shown)}}, opt);
}

inline void render(
  std::ostream& os,
  const SourceCache& cache,
  std::string_view name,
  const Diagnostics& ds,
  const RenderOptions& opt = {}
) {
  // Group diagnostics by file. Unknown files render title-only.
  std::vector<std::pair<std::string, std::vector<const Diagnostic*>>> keyed;
  std::unordered_map<std::string, std::size_t, TransparentHash, std::equal_to<>> index;
  for (const auto& d : ds.items) {
    const std::string key(d.file());
    const auto it = index.find(key);
    if (it == index.end()) {
      index.emplace(key, keyed.size());
      keyed.push_back({key, {&d}});
    } else {
      keyed[it->second].second.push_back(&d);
    }
  }

  static const SourceFile kEmpty = SourceFile::from("<unknown>", "");
  const SourceFile* fallback = cache.find(name);
  if (fallback == nullptr)
    fallback = &kEmpty;

  std::vector<std::pair<const SourceFile*, std::vector<const Diagnostic*>>> groups;
  groups.reserve(keyed.size());
  for (auto& [key, vec] : keyed) {
    const SourceFile* src = fallback;
    if (!key.empty() && key != name) {
      if (const SourceFile* hit = cache.find(key); hit != nullptr)
        src = hit;
    }
    groups.push_back({src, std::move(vec)});
  }
  render_core(os, groups, opt);
}

}  // namespace diag
