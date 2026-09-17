// Own the small status helpers.
// Import everything below. Change this file to change the API.
export module ui;
import std;
import ansi;
import config;

export namespace ui {

[[maybe_unused]] inline void error(std::string_view message) {
  std::print(
    "{}",
    ansi::format(ansi::fg(npda::config::colors::error) | ansi::emphasis::bold, "{} Error: {}\n", npda::config::symbols::error, message)
  );
}

[[maybe_unused]] inline void info(std::string_view message) {
  std::print(
    "{}",
    ansi::format(ansi::fg(npda::config::colors::info) | ansi::emphasis::bold, "{} {}\n", npda::config::symbols::info, message)
  );
}

// Shorten long text to a max width. Keeps the head and the tail.
[[nodiscard]] inline std::string truncate_middle(std::string_view s, std::size_t max_width = 60) {
  if (s.size() <= max_width || max_width <= 4)
    return std::string(s);
  const std::size_t keep = max_width - 3;
  const std::size_t head = (keep + 1) / 2;
  const std::size_t tail = keep - head;
  return std::string(s.substr(0, head)) + "..." + std::string(s.substr(s.size() - tail));
}

}  // namespace ui