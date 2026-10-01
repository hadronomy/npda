#include "diagnostics/render.h"

#include <unistd.h>

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <filesystem>
#include <functional>
#include <iostream>
#include <map>
#include <ranges>
#include <set>
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

bool ColorEnabled() {
  return terminal::CapabilitiesFor(terminal::OutputStream::kStderr).color;
}

// Resolve Auto against the terminal. Explicit modes pass through.
[[nodiscard]] bool ResolveColor(ColorMode mode) {
  if (mode == ColorMode::kAlways) return true;
  if (mode == ColorMode::kNever) return false;
  return ColorEnabled();
}

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

[[nodiscard]] const Glyphs& GlyphsFor(bool unicode) {
  static constexpr Glyphs kUni{"│", "-->", U'∿', U'↑', U'-', "╭",
                               "├", "╰",   "▶",  "█",  "…",  "──"};
  static constexpr Glyphs kAsc{"|", "-->", U'^', U'^', U'-',  "`",
                               "|", "`",   ">",  "!",  "...", "--"};
  return unicode ? kUni : kAsc;
}

// Terminal capability probe. Runs once per process.
const terminal::Capabilities& ProbeTerminal() {
  return terminal::CapabilitiesFor(terminal::OutputStream::kStderr);
}

struct Resolved {
  bool color = false;
  bool unicode = true;
  const Glyphs* g = &GlyphsFor(true);
  bool links = false;
  bool marks = false;
};

[[nodiscard]] Resolved ResolveOpt(const RenderOptions& opt) {
  const terminal::Capabilities& t = ProbeTerminal();
  const bool color = ResolveColor(opt.color);
  const bool unicode = opt.charset == CharSet::kUnicode ||
                       (opt.charset == CharSet::kAuto && t.utf8);
  Resolved r;
  r.color = color;
  r.unicode = unicode;
  r.g = &GlyphsFor(unicode);
  r.links = opt.links && t.tty;
  r.marks = opt.inline_marks && color && t.tty && unicode;
  return r;
}

// Underline color per severity. Mirrors the foreground palette, brightened.
[[nodiscard]] std::string_view SevUnderColor(Severity s, bool color) {
  if (!color) return "";
  switch (s) {
    case Severity::kError:
      return "\x1b[58;5;9m";
    case Severity::kWarning:
      return "\x1b[58;5;11m";
    case Severity::kNote:
      return "\x1b[58;5;12m";
    case Severity::kHelp:
      return "\x1b[58;5;10m";
  }
  return "";
}

// Expand tabs to display columns. map[i] holds the display column of byte i.
[[nodiscard]] std::string ExpandTabs(std::string_view line,
                                     std::size_t tab_width,
                                     std::vector<std::size_t>& map) {
  if (tab_width == 0) tab_width = 1;
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
[[nodiscard]] std::string LinkLocation(std::string_view filename,
                                       std::size_t line, std::size_t col,
                                       bool active) {
  const std::string text = std::string(filename) + ":" + std::to_string(line) +
                           ":" + std::to_string(col);
  if (!active) return text;
  char host[256] = {};
  if (gethostname(host, sizeof(host)) != 0) return text;
  std::error_code ec;
  const std::string abs = std::filesystem::absolute(filename, ec).string();
  if (ec) return text;
  return "\x1b]8;;file://" + std::string(host) + abs + "#" +
         std::to_string(line) + "\x1b\\" + text + "\x1b]8;;\x1b\\";
}

namespace ansi {

[[nodiscard]] constexpr std::string_view Red(bool c) {
  return c ? "\x1b[31m" : "";
}

[[nodiscard]] constexpr std::string_view Yellow(bool c) {
  return c ? "\x1b[33m" : "";
}

[[nodiscard]] constexpr std::string_view Blue(bool c) {
  return c ? "\x1b[34m" : "";
}

[[nodiscard]] constexpr std::string_view Green(bool c) {
  return c ? "\x1b[32m" : "";
}

[[nodiscard]] constexpr std::string_view Dim(bool c) {
  return c ? "\x1b[2m" : "";
}

[[nodiscard]] constexpr std::string_view Reset(bool c) {
  return c ? "\x1b[0m" : "";
}

}  // namespace ansi

[[nodiscard]] constexpr std::string_view SevStr(Severity s) {
  switch (s) {
    case Severity::kError:
      return "error";
    case Severity::kWarning:
      return "warning";
    case Severity::kNote:
      return "note";
    case Severity::kHelp:
      return "help";
  }
  return "message";
}

[[nodiscard]] std::string_view SevColor(Severity s, bool color) {
  using ansi::Blue;
  using ansi::Dim;
  using ansi::Green;
  using ansi::Red;
  using ansi::Reset;
  using ansi::Yellow;
  if (!color) return Reset(false);

  switch (s) {
    case Severity::kError:
      return Red(true);
    case Severity::kWarning:
      return Yellow(true);
    case Severity::kNote:
      return Blue(true);
    case Severity::kHelp:
      return Green(true);
  }
  return Reset(true);
}

inline void RenderSnippet(std::ostream& os, const SourceFile& src,
                          const Diagnostic& d, const RenderOptions& opt,
                          const Resolved& r) {
  using ansi::Blue;
  using ansi::Dim;
  using ansi::Green;
  using ansi::Red;
  using ansi::Reset;
  using ansi::Yellow;
  const bool color = r.color;
  const Glyphs& g = *r.g;

  struct LineMarks {
    std::vector<const DiagnosticLabel*> primary;
    std::vector<const DiagnosticLabel*> secondary;
  };

  std::map<std::size_t, LineMarks> per_line;

  // Paint order decides ties when labels share a line. Stable sort keeps
  // emission order for equal values.
  std::vector<const DiagnosticLabel*> ordered;
  ordered.reserve(d.labels.size());
  for (const auto& lab : d.labels) ordered.push_back(&lab);
  std::stable_sort(ordered.begin(), ordered.end(),
                   [](const DiagnosticLabel* a, const DiagnosticLabel* b) {
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
      auto& bucket =
          lab->primary ? per_line[line].primary : per_line[line].secondary;
      bucket.push_back(lab);
    }
  }

  const bool multiline =
      std::any_of(ranges.begin(), ranges.end(),
                  [](const Range& rg) { return rg.el > rg.sl; });

  // Gutter mark for one line. Empty for single-line diagnostics.
  const auto gutter_for = [&](std::size_t l) -> std::string {
    if (!multiline) return {};
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
    if (start) return std::string(g.mtop) + " ";
    if (end) return std::string(g.mbot) + " ";
    if (inside) return std::string(g.vbar) + " ";
    return "  ";
  };

  std::size_t max_line_num_width = 2;
  if (!per_line.empty()) {
    max_line_num_width = std::max<std::size_t>(
        max_line_num_width,
        std::to_string(per_line.rbegin()->first + opt.context_lines).length());
  }
  std::string pad(max_line_num_width, ' ');

  std::size_t printed_upto = 0;
  std::vector<std::size_t> colmap;
  const auto print_plain = [&](std::size_t l) {
    std::string num = std::to_string(l);
    if (num.size() < max_line_num_width)
      num.insert(0, max_line_num_width - num.size(), ' ');
    const std::string shown =
        ExpandTabs(src.line_view(l), opt.tab_width, colmap);
    os << ' ' << gutter_for(l) << Dim(color) << num << " " << g.vbar << " "
       << Reset(color) << shown << '\n';
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
      const DiagnosticLabel* first = nullptr;
      if (!marks.primary.empty())
        first = marks.primary.front();
      else if (!marks.secondary.empty())
        first = marks.secondary.front();

      auto [l, c] = src.line_col(first ? first->span.lo : 0);
      os << "  " << Dim(color) << g.arrow << Reset(color) << " "
         << LinkLocation(src.filename, l, c, r.links) << '\n';
      header_printed = true;
    }
    const std::size_t lead =
        line > opt.context_lines ? line - opt.context_lines : 1;
    for (std::size_t l = std::max(printed_upto + 1, lead); l < line; ++l)
      print_plain(l);

    const std::string shown =
        ExpandTabs(src.line_view(line), opt.tab_width, colmap);

    os << ' ' << gutter_for(line) << pad << Dim(color) << " " << g.vbar
       << Reset(color) << '\n';

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
        if (sl != line || lb->span.empty()) continue;
        const auto [el, ec] =
            src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
        if (el != line) continue;
        segs.push_back({dcol(sc ? sc - 1 : 0), dcol(ec ? ec : 0)});
      }
      std::sort(segs.begin(), segs.end());
      std::string acc;
      acc.reserve(painted.size() + segs.size() * 16);
      std::size_t pos = 0;
      const std::string open = std::string("\x1b[4:3m") +
                               std::string(SevUnderColor(d.severity, true));
      for (const auto& sg : segs) {
        if (sg.s < pos || sg.e <= sg.s) continue;
        acc.append(painted, pos, sg.s - pos);
        acc += open;
        acc.append(painted, sg.s, std::min(sg.e, painted.size()) - sg.s);
        acc += "\x1b[59m\x1b[24m";
        pos = std::min(sg.e, painted.size());
      }
      acc.append(painted, pos, std::string::npos);
      painted = std::move(acc);
    }

    os << ' ' << gutter_for(line) << Dim(color) << line_number << " " << g.vbar
       << " " << Reset(color) << painted << '\n';

    // Mark kinds per display column: 1 primary, 2 secondary.
    std::vector<char> kind(colmap.back() + 1, 0);
    auto place_marks = [&](const std::vector<const DiagnosticLabel*>& labs,
                           char kindv) {
      for (const auto* lb : labs) {
        const auto [sl, sc] = src.line_col(lb->span.lo);
        std::size_t el = sl;
        std::size_t ec = 0;
        if (!lb->span.empty()) {
          const auto [ell, ecc] =
              src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
          el = ell;
          ec = ecc;
        }
        if (line < sl || line > el) continue;
        if (sl != el && line != sl && line != el)
          continue;  // gutter covers middle lines
        const std::size_t bstart = (line == sl) ? (sc ? sc - 1 : 0) : 0;
        std::size_t bend;
        if (line == el)
          bend = (!lb->span.empty() && ec) ? ec : bstart + 1;
        else
          bend = src.line_view(line)
                     .size();  // start row of multi-line: mark to EOL
        std::size_t ds = dcol(bstart);
        std::size_t de = dcol(bend);
        if (de <= ds) de = ds + 1;
        if (de > kind.size()) kind.resize(de, 0);
        for (std::size_t i = ds; i < de; ++i) kind[i] = kindv;
      }
    };

    place_marks(marks.secondary, 2);
    place_marks(marks.primary, 1);

    while (!kind.empty() && kind.back() == 0) kind.pop_back();

    if (!kind.empty()) {
      os << ' ' << gutter_for(line) << pad << Dim(color) << " " << g.vbar << " "
         << Reset(color);
      for (char k : kind) {
        if (k == 1) {
          os << SevColor(d.severity, color) << under_utf8 << Reset(color);
        } else if (k == 2) {
          os << Blue(color) << sec_utf8 << Reset(color);
        } else {
          os << ' ';
        }
      }
      os << '\n';
    }

    // Messages attach at the end row of each span.
    auto print_msgs = [&](const std::vector<const DiagnosticLabel*>& labs,
                          bool primary) {
      for (const auto* lb : labs) {
        const auto [sl, sc] = src.line_col(lb->span.lo);
        std::size_t el = sl;
        std::size_t ec = 0;
        if (!lb->span.empty()) {
          const auto [ell, ecc] =
              src.line_col(lb->span.hi ? lb->span.hi - 1 : lb->span.hi);
          el = ell;
          ec = ecc;
        }
        if (el != line) continue;

        std::size_t indent =
            (line == sl) ? dcol(sc ? sc - 1 : 0) : dcol(ec ? ec : 0);
        os << ' ' << gutter_for(line) << pad << Dim(color) << " " << g.vbar
           << " " << Reset(color);
        for (std::size_t i = 0; i < indent; ++i) os << ' ';

        const std::string_view col_code =
            primary ? SevColor(d.severity, color) : Blue(color);
        const std::string& marker = primary ? msg_utf8 : sec_utf8;
        os << col_code << marker << Reset(color);
        if (!lb->message.empty()) os << ' ' << lb->message;
        os << '\n';
      }
    };

    print_msgs(marks.secondary, false);
    print_msgs(marks.primary, true);
    printed_upto = line;
    for (std::size_t l = line + 1;
         l <= line + opt.context_lines && l <= src.line_count(); ++l) {
      print_plain(l);
      printed_upto = l;
    }
  }

  for (const auto& n : d.notes) {
    os << ' ' << gutter_for(printed_upto) << pad << Dim(color) << " = "
       << Reset(color) << n << '\n';
  }
}

inline void RenderGroup(std::ostream& os, const SourceFile& src,
                        const std::vector<const Diagnostic*>& group,
                        const RenderOptions& opt, const Resolved& r) {
  using ansi::Blue;
  using ansi::Dim;
  using ansi::Green;
  using ansi::Red;
  using ansi::Reset;
  using ansi::Yellow;
  for (const auto* dp : group) {
    const auto& d = *dp;
    os << SevColor(d.severity, r.color) << r.g->rail << Reset(r.color) << " "
       << SevColor(d.severity, r.color) << SevStr(d.severity) << Reset(r.color);
    if (!d.code.empty()) {
      os << "[" << d.code << "]";
    }
    os << ": " << d.message << '\n';
    RenderSnippet(os, src, d, opt, r);
    os << '\n';
  }
}

inline void RenderCore(
    std::ostream& os,
    const std::vector<
        std::pair<const SourceFile*, std::vector<const Diagnostic*>>>& groups,
    const RenderOptions& opt) {
  using ansi::Blue;
  using ansi::Dim;
  using ansi::Green;
  using ansi::Red;
  using ansi::Reset;
  using ansi::Yellow;
  const Resolved r = ResolveOpt(opt);
  const Glyphs& g = *r.g;

  // Collapse same-code runs on one file. Verbose mode expands them.
  struct Collapse {
    std::string key;
    std::vector<const Diagnostic*> members;
  };

  for (const auto& [src, group] : groups) {
    std::vector<Collapse> collapses;
    std::unordered_map<std::string, std::size_t, TransparentHash,
                       std::equal_to<>>
        index;
    for (const auto* dp : group) {
      const std::string key = std::string(SevStr(dp->severity)) + '\0' +
                              dp->code + '\0' + std::string(dp->file());
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
      if (c.members.size() < 2 || opt.verbose ||
          c.members.front()->labels.empty()) {
        RenderGroup(os, *src, c.members, opt, r);
        continue;
      }
      const Diagnostic& first = *c.members.front();
      const std::string file =
          std::string(first.file().empty() ? std::string_view(src->filename)
                                           : first.file());
      os << SevColor(first.severity, r.color) << g.rail << Reset(r.color) << " "
         << SevColor(first.severity, r.color) << SevStr(first.severity)
         << Reset(r.color);
      if (!first.code.empty()) os << "[" << first.code << "]";
      os << ": " << first.message << " (" << c.members.size()
         << " occurrences) " << g.ddash << " " << file << '\n';
      const std::size_t shown_n = std::min<std::size_t>(c.members.size(), 3);
      for (std::size_t i = 0; i < shown_n; ++i) {
        const Diagnostic& m = *c.members[i];
        const DiagnosticLabel* pick = nullptr;
        for (const auto& lab : m.labels)
          if (lab.primary) {
            pick = &lab;
            break;
          }
        if (pick == nullptr) pick = &m.labels.front();
        const auto [l, co] = src->line_col(pick->span.lo);
        const bool last = (i + 1 == shown_n) && c.members.size() <= 3;
        os << "  " << (last ? g.mbot : g.mmid) << g.ddash << " "
           << LinkLocation(file, l, co, r.links);
        if (!pick->message.empty()) os << ": " << pick->message;
        os << '\n';
      }
      if (c.members.size() > 3)
        os << "  " << g.mbot << g.ddash << " " << g.ellipsis << "(+"
           << c.members.size() - 3 << " more, --verbose shows all)\n";
      os << '\n';
    }
  }

  std::size_t errors = 0;
  for (const auto& [src, group] : groups)
    for (const auto* dp : group)
      if (dp->severity == Severity::kError) ++errors;
  if (errors > 1)
    os << SevColor(Severity::kError, r.color) << "error" << Reset(r.color)
       << ": aborting due to " << errors << " previous errors\n";
}

}  // namespace

void Render(std::ostream& os, const SourceFile& src, const DiagnosticSet& ds,
            const RenderOptions& opt) {
  using ansi::Reset;

  // Drop exact duplicates: same code and span means one fault.
  std::vector<const Diagnostic*> shown;
  std::set<std::tuple<std::string_view, std::string_view, std::size_t,
                      std::size_t, std::string_view>>
      seen;
  for (const auto& d : ds.items) {
    if (d.labels.empty()) {
      shown.push_back(&d);
      continue;
    }
    bool fresh = false;
    for (const auto& lab : d.labels)
      fresh |= seen.insert({d.code, lab.span.file, lab.span.lo, lab.span.hi,
                            lab.message})
                   .second;
    if (fresh) shown.push_back(&d);
  }

  RenderCore(os, {{&src, std::move(shown)}}, opt);
}

void Render(std::ostream& os, const SourceCache& cache, std::string_view name,
            const DiagnosticSet& ds, const RenderOptions& opt) {
  // Group diagnostics by file. Unknown files render title-only.
  std::vector<std::pair<std::string, std::vector<const Diagnostic*>>> keyed;
  std::unordered_map<std::string, std::size_t, TransparentHash, std::equal_to<>>
      index;
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

  static const SourceFile kEmpty = SourceFile::From("<unknown>", "");
  const SourceFile* fallback = cache.Find(name);
  if (fallback == nullptr) fallback = &kEmpty;

  std::vector<std::pair<const SourceFile*, std::vector<const Diagnostic*>>>
      groups;
  groups.reserve(keyed.size());
  for (auto& [key, vec] : keyed) {
    const SourceFile* src = fallback;
    if (!key.empty() && key != name) {
      if (const SourceFile* hit = cache.Find(key); hit != nullptr) src = hit;
    }
    groups.push_back({src, std::move(vec)});
  }
  RenderCore(os, groups, opt);
}

}  // namespace diag
