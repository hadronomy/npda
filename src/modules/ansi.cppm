// Own terminal styling: colors, emphasis, and formatting.
// Escape bytes match fmt 12.2.0 color output.
// Change this file to change styled output.
export module ansi;
import std;

export namespace ansi {

// A 24-bit color triple.
struct rgb {
  std::uint8_t r{};
  std::uint8_t g{};
  std::uint8_t b{};
};

// Terminal palette. Values are raw SGR codes.
enum class terminal_color : std::uint8_t {
  black = 30,
  red,
  green,
  yellow,
  blue,
  magenta,
  cyan,
  white,
  bright_black = 90,
  bright_red,
  bright_green,
  bright_yellow,
  bright_blue,
  bright_magenta,
  bright_cyan,
  bright_white
};

// Text emphasis. Values are bit flags.
enum class emphasis : std::uint8_t {
  bold = 1,
  faint = 1 << 1,
  italic = 1 << 2,
  underline = 1 << 3,
  blink = 1 << 4,
  reverse = 1 << 5,
  conceal = 1 << 6,
  strikethrough = 1 << 7
};

// A foreground or background color: none, rgb, or terminal.
using color = std::variant<std::monostate, rgb, terminal_color>;

// A composable text style: optional colors plus emphasis flags.
struct text_style {
  color fg{};
  color bg{};
  emphasis em{};

  [[nodiscard]] constexpr bool empty() const noexcept {
    return std::holds_alternative<std::monostate>(fg) &&
           std::holds_alternative<std::monostate>(bg) && em == emphasis{};
  }
};

[[nodiscard]] constexpr emphasis operator|(emphasis lhs, emphasis rhs) noexcept {
  return static_cast<emphasis>(
    static_cast<std::uint8_t>(lhs) | static_cast<std::uint8_t>(rhs));
}

[[nodiscard]] constexpr text_style operator|(text_style lhs, text_style rhs) noexcept {
  if (!std::holds_alternative<std::monostate>(rhs.fg)) lhs.fg = rhs.fg;
  if (!std::holds_alternative<std::monostate>(rhs.bg)) lhs.bg = rhs.bg;
  lhs.em = lhs.em | rhs.em;
  return lhs;
}

[[nodiscard]] constexpr text_style operator|(text_style s, emphasis e) noexcept {
  s.em = s.em | e;
  return s;
}

[[nodiscard]] constexpr text_style operator|(emphasis e, text_style s) noexcept {
  s.em = s.em | e;
  return s;
}

// Make a style from a foreground color.
[[nodiscard]] constexpr text_style fg(rgb c) noexcept {
  text_style s;
  s.fg = c;
  return s;
}

[[nodiscard]] constexpr text_style fg(terminal_color c) noexcept {
  text_style s;
  s.fg = c;
  return s;
}

// Make a style from a background color.
[[nodiscard]] constexpr text_style bg(rgb c) noexcept {
  text_style s;
  s.bg = c;
  return s;
}

[[nodiscard]] constexpr text_style bg(terminal_color c) noexcept {
  text_style s;
  s.bg = c;
  return s;
}

namespace detail {
// Append a 3-digit decimal value. Matches fmt to_esc padding.
constexpr void append_padded(std::string& out, std::uint8_t v) {
  out.push_back(static_cast<char>('0' + v / 100));
  out.push_back(static_cast<char>('0' + v / 10 % 10));
  out.push_back(static_cast<char>('0' + v % 10));
}

// Append a 2-digit SGR code. All terminal codes have 2 digits.
constexpr void append_code(std::string& out, unsigned v) {
  out.push_back(static_cast<char>('0' + v / 10));
  out.push_back(static_cast<char>('0' + v % 10));
}

constexpr void append_color(std::string& out, color c, bool background) {
  if (std::holds_alternative<rgb>(c)) {
    const rgb col = std::get<rgb>(c);
    out += background ? "\x1b[48;2;" : "\x1b[38;2;";
    append_padded(out, col.r);
    out += ';';
    append_padded(out, col.g);
    out += ';';
    append_padded(out, col.b);
    out += 'm';
  } else if (std::holds_alternative<terminal_color>(c)) {
    unsigned code = static_cast<unsigned>(std::get<terminal_color>(c));
    if (background)
      code += 10;
    out += "\x1b[";
    append_code(out, code);
    out += 'm';
  }
}
}  // namespace detail

// Build the opening escape sequence. Empty style gives empty text.
// Order is emphasis, foreground, background, like fmt.
[[nodiscard]] constexpr std::string open(text_style s) {
  std::string out;
  if (s.em != emphasis{}) {
    out += "\x1b[";
    bool first = true;
    constexpr emphasis flags[] = {
      emphasis::bold,
      emphasis::faint,
      emphasis::italic,
      emphasis::underline,
      emphasis::blink,
      emphasis::reverse,
      emphasis::conceal,
      emphasis::strikethrough,
    };
    constexpr int codes[] = {1, 2, 3, 4, 5, 7, 8, 9};
    for (std::size_t i = 0; i < 8; ++i) {
      if ((static_cast<std::uint8_t>(s.em) & static_cast<std::uint8_t>(flags[i])) == 0)
        continue;
      if (!first)
        out += ';';
      out.push_back(static_cast<char>('0' + codes[i]));
      first = false;
    }
    out += 'm';
  }
  if (!std::holds_alternative<std::monostate>(s.fg))
    detail::append_color(out, s.fg, false);
  if (!std::holds_alternative<std::monostate>(s.bg))
    detail::append_color(out, s.bg, true);
  return out;
}

// Closing sequence. Empty style gives empty text.
[[nodiscard]] constexpr std::string_view close(text_style s) noexcept {
  return s.empty() ? "" : "\x1b[0m";
}

// Format text with a style. Empty style gives plain text.
template <typename... T>
[[nodiscard]] std::string format(text_style s, std::format_string<T...> f, T&&... args) {
  std::string body = std::format(f, std::forward<T>(args)...);
  if (s.empty())
    return body;
  return open(s) + body + std::string(close(s));
}

// Print styled text to stdout.
template <typename... T>
void print(text_style s, std::format_string<T...> f, T&&... args) {
  std::print("{}", format(s, f, std::forward<T>(args)...));
}

template <typename... T>
void print(std::FILE* f, text_style s, std::format_string<T...> ff, T&&... args) {
  std::print(f, "{}", format(s, ff, std::forward<T>(args)...));
}

// Print styled text to stdout, then a newline.
template <typename... T>
void println(text_style s, std::format_string<T...> f, T&&... args) {
  std::print("{}\n", format(s, f, std::forward<T>(args)...));
}

template <typename... T>
void println(std::FILE* f, text_style s, std::format_string<T...> ff, T&&... args) {
  std::print(f, "{}\n", format(s, ff, std::forward<T>(args)...));
}

// A value paired with a style. Formats with the style applied.
template <typename T>
struct styled {
  const T& value;
  text_style style;
};

template <typename T>
styled(const T&, text_style) -> styled<T>;

// A range paired with a separator. Formats elements joined by it.
template <std::ranges::input_range R>
struct joined {
  const R& range;
  std::string_view sep;
};

template <std::ranges::input_range R>
[[nodiscard]] joined<R> join(const R& r, std::string_view sep) {
  return {r, sep};
}

static_assert(open(fg(rgb{203, 166, 247})) == "\x1b[38;2;203;166;247m");
static_assert(open(fg(terminal_color::green)) == "\x1b[32m");
static_assert(open(fg(terminal_color::bright_red)) == "\x1b[91m");
static_assert(open(fg(rgb{243, 139, 168}) | emphasis::bold) == "\x1b[1m\x1b[38;2;243;139;168m");
static_assert(
  open(fg(terminal_color::red) | emphasis::bold | emphasis::underline) == "\x1b[1;4m\x1b[31m");
static_assert(open(bg(rgb{1, 2, 3})) == "\x1b[48;2;001;002;003m");
static_assert(open(bg(terminal_color::blue)) == "\x1b[44m");
static_assert(open(text_style{}) == "");
static_assert(close(text_style{}) == "");
static_assert(close(fg(rgb{0, 0, 0})) == "\x1b[0m");

}  // namespace ansi

namespace std {
template <typename T, typename Char>
struct formatter<ansi::styled<T>, Char> {
  formatter<T, Char> base_;
  constexpr auto parse(auto& pc) { return base_.parse(pc); }
  template <typename Ctx>
  auto format(const ansi::styled<T>& s, Ctx& ctx) const {
    if (!s.style.empty()) {
      auto out = ctx.out();
      const std::string pre = ansi::open(s.style);
      out = std::copy(pre.begin(), pre.end(), out);
      ctx.advance_to(out);
    }
    base_.format(s.value, ctx);
    if (!s.style.empty()) {
      auto out = ctx.out();
      static constexpr char reset[] = "\x1b[0m";
      out = std::copy_n(reset, 4, out);
      ctx.advance_to(out);
    }
    return ctx.out();
  }
};

template <typename R, typename Char>
struct formatter<ansi::joined<R>, Char> {
  using elem_t = std::ranges::range_value_t<R>;
  formatter<elem_t, Char> elem_;
  constexpr auto parse(auto& pc) { return elem_.parse(pc); }
  template <typename Ctx>
  auto format(const ansi::joined<R>& j, Ctx& ctx) const {
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
