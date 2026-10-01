#include "terminal/ansi.h"

#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <variant>

#include "terminal/capabilities.h"

namespace ansi {

bool ColorEnabled() {
  return terminal::CapabilitiesFor(terminal::OutputStream::kStdout).color;
}

bool UnicodeEnabled() {
  return terminal::CapabilitiesFor(terminal::OutputStream::kStdout).utf8;
}

namespace detail {

// Append a 3-digit decimal value. Matches fmt to_esc padding.
void AppendPadded(std::string& out, std::uint8_t v) {
  out.push_back(static_cast<char>('0' + v / 100));
  out.push_back(static_cast<char>('0' + v / 10 % 10));
  out.push_back(static_cast<char>('0' + v % 10));
}

// Append a 2-digit SGR code. All terminal codes have 2 digits.
void AppendCode(std::string& out, unsigned v) {
  out.push_back(static_cast<char>('0' + v / 10));
  out.push_back(static_cast<char>('0' + v % 10));
}

void AppendColor(std::string& out, Color c, bool background) {
  if (std::holds_alternative<Rgb>(c)) {
    const Rgb col = std::get<Rgb>(c);
    out += background ? "\x1b[48;2;" : "\x1b[38;2;";
    AppendPadded(out, col.r);
    out += ';';
    AppendPadded(out, col.g);
    out += ';';
    AppendPadded(out, col.b);
    out += 'm';
  } else if (std::holds_alternative<TerminalColor>(c)) {
    unsigned code = static_cast<unsigned>(std::get<TerminalColor>(c));
    if (background) code += 10;
    out += "\x1b[";
    AppendCode(out, code);
    out += 'm';
  }
}

}  // namespace detail

// Build the opening escape sequence. Empty style gives empty text.
// Order is emphasis, foreground, background, like fmt.
[[nodiscard]] std::string Open(TextStyle s) {
  std::string out;
  if (s.em != Emphasis{}) {
    out += "\x1b[";
    bool first = true;
    Emphasis flags[] = {
        Emphasis::kBold,      Emphasis::kFaint,         Emphasis::kItalic,
        Emphasis::kUnderline, Emphasis::kBlink,         Emphasis::kReverse,
        Emphasis::kConceal,   Emphasis::kStrikethrough,
    };
    int codes[] = {1, 2, 3, 4, 5, 7, 8, 9};
    for (std::size_t i = 0; i < 8; ++i) {
      if ((static_cast<std::uint8_t>(s.em) &
           static_cast<std::uint8_t>(flags[i])) == 0)
        continue;
      if (!first) out += ';';
      out.push_back(static_cast<char>('0' + codes[i]));
      first = false;
    }
    out += 'm';
  }
  if (!std::holds_alternative<std::monostate>(s.fg))
    detail::AppendColor(out, s.fg, false);
  if (!std::holds_alternative<std::monostate>(s.bg))
    detail::AppendColor(out, s.bg, true);
  return out;
}

}  // namespace ansi
