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
    ansi::paint_bold(
      std::format("{} Error: {}\n", npda::config::symbols::error, message),
      npda::config::colors::error
    )
  );
}

[[maybe_unused]] inline void info(std::string_view message) {
  std::print(
    "{}",
    ansi::paint_bold(
      std::format("{} {}\n", npda::config::symbols::info, message),
      npda::config::colors::info
    )
  );
}

}  // namespace ui