// Own the small status helpers.
// fmt stays in the global fragment. Import the rest below.
module;
#include <fmt/color.h>
#include <fmt/format.h>

export module ui;
import std;
import config;

export namespace ui {

[[maybe_unused]] inline void error(std::string_view message) {
  fmt::print(
    fg(npda::config::colors::error) | fmt::emphasis::bold,
    "{} Error: {}\n",
    npda::config::symbols::error,
    message
  );
}

[[maybe_unused]] inline void info(std::string_view message) {
  fmt::print(
    fg(npda::config::colors::info) | fmt::emphasis::bold,
    "{} {}\n",
    npda::config::symbols::info,
    message
  );
}

}  // namespace ui