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

}  // namespace ui