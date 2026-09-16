// Own terminal colors and text paint helpers.
// Replaces the fmt color API with identical escape bytes.
// Change this file to change color output.
export module ansi;
import std;

export namespace ansi {

// A 24-bit color triple.
struct rgb {
  std::uint8_t r{};
  std::uint8_t g{};
  std::uint8_t b{};
};

// Terminal palette entries used in this repo.
enum class term : int { red = 31, green = 32, yellow = 33, cyan = 36 };

// Paint text with a 24-bit color. No color returns text unchanged.
[[nodiscard]] inline std::string paint(std::string_view text, rgb c, bool color = true) {
  if (!color)
    return std::string(text);
  return std::format(
    "\x1b[38;2;{};{};{}m{}\x1b[0m",
    static_cast<unsigned>(c.r),
    static_cast<unsigned>(c.g),
    static_cast<unsigned>(c.b),
    text
  );
}

// Paint text with a terminal color. No color returns text unchanged.
[[nodiscard]] inline std::string paint(std::string_view text, term t, bool color = true) {
  if (!color)
    return std::string(text);
  return std::format("\x1b[{}m{}\x1b[0m", static_cast<int>(t), text);
}

// Paint bold text with a 24-bit color. No color returns text unchanged.
[[nodiscard]] inline std::string paint_bold(std::string_view text, rgb c, bool color = true) {
  if (!color)
    return std::string(text);
  return std::format(
    "\x1b[1m\x1b[38;2;{};{};{}m{}\x1b[0m",
    static_cast<unsigned>(c.r),
    static_cast<unsigned>(c.g),
    static_cast<unsigned>(c.b),
    text
  );
}

}  // namespace ansi
