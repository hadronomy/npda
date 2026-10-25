#include "terminal/style.h"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>

#include "terminal/capabilities.h"

namespace terminal {

namespace {

constexpr Glyphs kUnicode{
    .digit_group = "\u202F",
    .ellipsis = "…",
    .tree_branch = "├─ ",
    .tree_last = "└─ ",
    .tree_pipe = "│  ",
};

constexpr Glyphs kAscii{
    .digit_group = " ",
    .ellipsis = "...",
    .tree_branch = "|- ",
    .tree_last = "`- ",
    .tree_pipe = "|  ",
};

std::string_view Code(Tone tone) {
  switch (tone) {
    case Tone::kPlain:
      return "";
    case Tone::kStrong:
      return "1";
    case Tone::kMuted:
      return "2";
    case Tone::kHeading:
      return "1;33";
    case Tone::kAccent:
      return "36";
    case Tone::kSecondary:
      return "35";
    case Tone::kDefined:
      return "1;33";
    case Tone::kBrand:
      return "1;35";
    case Tone::kBad:
      return "31";
  }
  return "";
}

bool IsContinuation(char byte) {
  return (static_cast<unsigned char>(byte) & 0xC0) == 0x80;
}

}  // namespace

Style::Style(bool color, bool unicode)
    : color_(color),
      unicode_(unicode),
      glyphs_(unicode ? &kUnicode : &kAscii) {}

Style Style::For(OutputStream stream) {
  const Capabilities& capabilities = CapabilitiesFor(stream);
  return Style(capabilities.color, capabilities.utf8);
}

Style Style::Plain() { return Style(false, true); }

std::string Style::Paint(Tone tone, std::string_view text) const {
  const auto code = Code(tone);
  if (!color_ || code.empty() || text.empty()) return std::string(text);
  return std::format("\x1b[{}m{}\x1b[0m", code, text);
}

Styled Style::Span(Tone tone, std::string_view text) const {
  return {.text = Paint(tone, text), .width = DisplayWidth(text)};
}

std::string Style::Number(std::uint64_t value) const {
  const auto digits = std::to_string(value);
  if (digits.size() <= 4) return digits;
  std::string text;
  for (std::size_t index = 0; index < digits.size(); ++index) {
    if (index != 0 && (digits.size() - index) % 3 == 0) {
      text += glyphs_->digit_group;
    }
    text += digits[index];
  }
  return text;
}

std::size_t DisplayWidth(std::string_view text) {
  std::size_t width = 0;
  for (const char byte : text) width += IsContinuation(byte) ? 0 : 1;
  return width;
}

std::string PadRight(std::string_view text, std::size_t width) {
  std::string padded(text);
  const auto used = DisplayWidth(text);
  if (used < width) padded.append(width - used, ' ');
  return padded;
}

std::string PadRight(const Styled& text, std::size_t width) {
  std::string padded = text.text;
  if (text.width < width) padded.append(width - text.width, ' ');
  return padded;
}

Styled& Styled::operator+=(const Styled& other) {
  text += other.text;
  width += other.width;
  return *this;
}

Styled operator+(Styled left, const Styled& right) {
  left += right;
  return left;
}

std::string PadLeft(std::string_view text, std::size_t width) {
  const auto used = DisplayWidth(text);
  std::string padded(used < width ? width - used : 0, ' ');
  padded += text;
  return padded;
}

}  // namespace terminal
