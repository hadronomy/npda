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

}  // namespace ui