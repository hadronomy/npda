#pragma once

#include <fmt/color.h>
#include <fmt/format.h>

#include "config.h"

namespace ui {

[[maybe_unused]] static void error(std::string_view message) {
  fmt::print(
    fg(npda::config::colors::error) | fmt::emphasis::bold,
    "{} Error: {}\n",
    npda::config::symbols::error,
    message
  );
}

[[maybe_unused]] static void info(std::string_view message) {
  fmt::print(
    fg(npda::config::colors::info) | fmt::emphasis::bold,
    "{} {}\n",
    npda::config::symbols::info,
    message
  );
}

}  // namespace ui