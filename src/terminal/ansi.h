#ifndef NPDA_SRC_TERMINAL_ANSI_H_
#define NPDA_SRC_TERMINAL_ANSI_H_

#include <algorithm>
#include <cstdint>
#include <format>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

namespace ansi {

// Output control. Auto probes once: off with NO_COLOR set,
// with a dumb terminal, or with piped stdout. Matches Clang behavior.
[[nodiscard]] bool ColorEnabled();

// Unicode probe for glyph selection. False under C/POSIX locales.
[[nodiscard]] bool UnicodeEnabled();

// A 24-bit color triple.
struct Rgb {
  std::uint8_t r{};
  std::uint8_t g{};
  std::uint8_t b{};
};

// Terminal palette. Values are raw SGR codes.
enum class TerminalColor : std::uint8_t {
  kBlack = 30,
  kRed,
  kGreen,
  kYellow,
  kBlue,
  kMagenta,
  kCyan,
  kWhite,
  kBrightBlack = 90,
  kBrightRed,
  kBrightGreen,
  kBrightYellow,
  kBrightBlue,
  kBrightMagenta,
  kBrightCyan,
  kBrightWhite
};

// Text emphasis. Values are bit flags.
enum class Emphasis : std::uint8_t {
  kBold = 1,
  kFaint = 1 << 1,
  kItalic = 1 << 2,
  kUnderline = 1 << 3,
  kBlink = 1 << 4,
  kReverse = 1 << 5,
  kConceal = 1 << 6,
  kStrikethrough = 1 << 7
};

// A foreground or background color: none, rgb, or terminal.
using Color = std::variant<std::monostate, Rgb, TerminalColor>;

// A composable text style: optional colors plus emphasis flags.
struct TextStyle {
  Color fg{};
  Color bg{};
  Emphasis em{};

  [[nodiscard]] constexpr bool empty() const noexcept {
    return std::holds_alternative<std::monostate>(fg) &&
           std::holds_alternative<std::monostate>(bg) && em == Emphasis{};
  }
};

[[nodiscard]] constexpr Emphasis operator|(Emphasis lhs,
                                           Emphasis rhs) noexcept {
  return static_cast<Emphasis>(static_cast<std::uint8_t>(lhs) |
                               static_cast<std::uint8_t>(rhs));
}

[[nodiscard]] constexpr TextStyle operator|(TextStyle lhs,
                                            TextStyle rhs) noexcept {
  if (!std::holds_alternative<std::monostate>(rhs.fg)) lhs.fg = rhs.fg;
  if (!std::holds_alternative<std::monostate>(rhs.bg)) lhs.bg = rhs.bg;
  lhs.em = lhs.em | rhs.em;
  return lhs;
}

[[nodiscard]] constexpr TextStyle operator|(TextStyle s, Emphasis e) noexcept {
  s.em = s.em | e;
  return s;
}

[[nodiscard]] constexpr TextStyle operator|(Emphasis e, TextStyle s) noexcept {
  s.em = s.em | e;
  return s;
}

// Make a style from a foreground color.
[[nodiscard]] constexpr TextStyle Fg(Rgb c) noexcept {
  TextStyle s;
  s.fg = c;
  return s;
}

[[nodiscard]] constexpr TextStyle Fg(TerminalColor c) noexcept {
  TextStyle s;
  s.fg = c;
  return s;
}

// Make a style from a background color.
[[nodiscard]] constexpr TextStyle Bg(Rgb c) noexcept {
  TextStyle s;
  s.bg = c;
  return s;
}

[[nodiscard]] constexpr TextStyle Bg(TerminalColor c) noexcept {
  TextStyle s;
  s.bg = c;
  return s;
}

[[nodiscard]] std::string Open(TextStyle style);

// Closing sequence. Empty style gives empty text.
[[nodiscard]] constexpr std::string_view Close(TextStyle s) noexcept {
  return s.empty() ? "" : "\x1b[0m";
}

// Format text with a style. Empty style gives plain text.
// Disabled color also gives plain text, so callers never branch.
template <typename... T>
[[nodiscard]] std::string Format(TextStyle s, std::format_string<T...> f,
                                 T&&... args) {
  std::string body = std::format(f, std::forward<T>(args)...);
  if (s.empty() || !ColorEnabled()) return body;
  return Open(s) + body + std::string(Close(s));
}

// A value paired with a style. Formats with the style applied.
template <typename T>
struct Styled {
  const T& value;
  TextStyle style;
};

template <typename T>
Styled(const T&, TextStyle) -> Styled<T>;

// A range paired with a separator. Formats elements joined by it.
template <std::ranges::input_range R>
struct Joined {
  const R& range;
  std::string_view sep;
};

template <std::ranges::input_range R>
[[nodiscard]] Joined<R> Join(const R& r, std::string_view sep) {
  return {r, sep};
}

}  // namespace ansi

namespace std {

template <typename T, typename Char>
struct formatter<ansi::Styled<T>, Char> {
  formatter<T, Char> base_;

  constexpr auto parse(auto& pc) { return base_.parse(pc); }

  template <typename Ctx>
  auto format(const ansi::Styled<T>& s, Ctx& ctx) const {
    if (!s.style.empty() && ansi::ColorEnabled()) {
      auto out = ctx.out();
      const std::string pre = ansi::Open(s.style);
      out = std::copy(pre.begin(), pre.end(), out);
      ctx.advance_to(out);
    }
    base_.format(s.value, ctx);
    if (!s.style.empty() && ansi::ColorEnabled()) {
      auto out = ctx.out();
      static constexpr char kReset[] = "\x1b[0m";
      out = std::copy_n(kReset, 4, out);
      ctx.advance_to(out);
    }
    return ctx.out();
  }
};

template <typename R, typename Char>
struct formatter<ansi::Joined<R>, Char> {
  using ElemT = std::ranges::range_value_t<R>;
  formatter<ElemT, Char> elem_;

  constexpr auto parse(auto& pc) { return elem_.parse(pc); }

  template <typename Ctx>
  auto format(const ansi::Joined<R>& j, Ctx& ctx) const {
    auto out = ctx.out();
    bool first = true;
    for (const auto& e : j.range) {
      if (!first) {
        for (char c : j.sep) *out++ = c;
        ctx.advance_to(out);
      }
      first = false;
      elem_.format(e, ctx);
      out = ctx.out();
    }
    return ctx.out();
  }
};

}  // namespace std

#endif  // NPDA_SRC_TERMINAL_ANSI_H_
