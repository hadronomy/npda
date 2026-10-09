#include "terminal/style.h"

#include <algorithm>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>

#include "terminal/capabilities.h"

namespace terminal {

namespace {

constexpr Glyphs kUnicode{
    .epsilon = "ε",
    .delta = "δ",
    .arrow = "→",
    .leads_to = "⇒",
    .cursor = "▸",
    .head = "▲",
    .separator = "·",
    .ellipsis = "…",
    .tree_branch = "├─ ",
    .tree_last = "└─ ",
    .tree_pipe = "│  ",
    .box_horizontal = "─",
    .box_vertical = "│",
    .box_top_left = "┌",
    .box_top_join = "┬",
    .box_top_right = "┐",
    .box_bottom_left = "└",
    .box_bottom_join = "┴",
    .box_bottom_right = "┘",
};

constexpr Glyphs kAscii{
    .epsilon = ".",
    .delta = "t",
    .arrow = "->",
    .leads_to = "=>",
    .cursor = "|",
    .head = "^",
    .separator = "|",
    .ellipsis = "...",
    .tree_branch = "|- ",
    .tree_last = "`- ",
    .tree_pipe = "|  ",
    .box_horizontal = "-",
    .box_vertical = "|",
    .box_top_left = "+",
    .box_top_join = "+",
    .box_top_right = "+",
    .box_bottom_left = "+",
    .box_bottom_join = "+",
    .box_bottom_right = "+",
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
    case Tone::kBrand:
      return "1;35";
    case Tone::kGood:
      return "32";
    case Tone::kBad:
      return "31";
    case Tone::kWarn:
      return "33";
  }
  return "";
}

bool IsContinuation(char byte) {
  return (static_cast<unsigned char>(byte) & 0xC0) == 0x80;
}

/// Returns the number of bytes of the first `columns` code points.
std::size_t PrefixBytes(std::string_view text, std::size_t columns) {
  std::size_t end = 0;
  for (std::size_t seen = 0; end < text.size(); ++end) {
    if (IsContinuation(text[end])) continue;
    if (seen++ == columns) break;
  }
  return end;
}

}  // namespace

Style::Style(bool color, bool unicode)
    : color_(color), glyphs_(unicode ? &kUnicode : &kAscii) {}

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

std::string Style::Separator() const {
  return Paint(Tone::kMuted, std::format(" {} ", glyphs_->separator));
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

std::string Truncate(
    std::string_view text,
    std::size_t width,
    std::string_view ellipsis
) {
  if (DisplayWidth(text) <= width) return std::string(text);
  const auto keep = width - std::min(width, DisplayWidth(ellipsis));
  return std::string(text.substr(0, PrefixBytes(text, keep))) +
         std::string(ellipsis);
}

std::string TruncateMiddle(
    std::string_view text,
    std::size_t width,
    std::string_view ellipsis
) {
  const auto total = DisplayWidth(text);
  if (total <= width) return std::string(text);
  const auto keep = width - std::min(width, DisplayWidth(ellipsis));
  const auto head = (keep + 1) / 2;
  const auto tail_start = PrefixBytes(text, total - (keep - head));
  return std::string(text.substr(0, PrefixBytes(text, head))) +
         std::string(ellipsis) + std::string(text.substr(tail_start));
}

}  // namespace terminal
