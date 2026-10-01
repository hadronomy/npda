#include "terminal/ui.h"

#include <cstddef>
#include <print>
#include <string>
#include <string_view>

#include "terminal/ansi.h"
#include "terminal/palette.h"

namespace ui {

void Error(std::string_view message) {
  std::print(
      "{}",
      ansi::Format(ansi::Fg(terminal::colors::kError) | ansi::Emphasis::kBold,
                   "{} Error: {}\n", terminal::symbols::kError, message));
}

void Info(std::string_view message) {
  std::print("{}", ansi::Format(ansi::Fg(terminal::colors::kInfo) |
                                    ansi::Emphasis::kBold,
                                "{} {}\n", terminal::symbols::kInfo, message));
}

// Shorten long text to a max width. Keeps the head and the tail.
[[nodiscard]] std::string TruncateMiddle(std::string_view s,
                                         std::size_t max_width) {
  if (s.size() <= max_width || max_width <= 4) return std::string(s);
  const std::size_t keep = max_width - 3;
  const std::size_t head = (keep + 1) / 2;
  const std::size_t tail = keep - head;
  return std::string(s.substr(0, head)) + "..." +
         std::string(s.substr(s.size() - tail));
}

// Section rule with a short title. Fixed 60 columns, dim chrome.
[[nodiscard]] std::string Rule(std::string_view title) {
  const bool uni = ansi::UnicodeEnabled();
  const std::string bar = uni ? "─" : "-";
  const std::string head = TruncateMiddle(title, 40);
  std::string out;
  std::size_t cols = 0;
  for (std::size_t i = 0; i < 12; ++i) {
    out += bar;
    ++cols;
  }
  out += " " + head + " ";
  cols += 2 + head.size();
  while (cols < 60) {
    out += bar;
    ++cols;
  }
  ansi::TextStyle dim;
  dim.em = ansi::Emphasis::kFaint;
  return ansi::Format(dim, "{}", out);
}

// Direction arrow. Matches the unicode probe: → on TTY, -> in pipes.
[[nodiscard]] std::string Arrow() {
  return ansi::UnicodeEnabled() ? "→" : "->";
}

// Block rail opener for input blocks. █ edges mark blocks at a glance
// in scrollback; light rules stay for steps and verdicts.
[[nodiscard]] std::string Rail(std::string_view title) {
  const bool uni = ansi::UnicodeEnabled();
  const std::string edge = uni ? "█" : "#";
  const std::string head = TruncateMiddle(title, 44);
  std::string out = edge + edge + edge + " " + head + " ";
  std::size_t cols = 3 + 1 + head.size() + 1;
  while (cols < 60) {
    out += edge;
    ++cols;
  }
  ansi::TextStyle bold;
  bold.em = ansi::Emphasis::kBold;
  return ansi::Format(bold, "{}", out);
}

}  // namespace ui
