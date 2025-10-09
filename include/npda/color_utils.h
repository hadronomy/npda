#pragma once

#include <fmt/color.h>
#include <cmath>
#include <cstdint>

namespace npda {

// Generate color for non-accepting states (avoids green hues)
// Takes a hash value as parameter to ensure consistent colors
inline fmt::rgb generate_non_accepting_state_color(std::size_t state_hash) {
  // Use golden ratio to create visually distinct colors
  // Map hash to a limited set of harmonious colors
  constexpr std::size_t color_palette_size = 8;
  std::size_t color_index = state_hash % color_palette_size;

  // Use golden ratio to generate harmonious hues, but avoid green range
  // Green is roughly 1/6 to 2/6 (0.166 to 0.333) of the hue wheel
  // We'll map to colors in the blue-purple-orange-red range instead
  constexpr double golden_ratio_conjugate = 0.618033988749895;
  double raw_hue = std::fmod(color_index * golden_ratio_conjugate, 1.0);

  // Shift hues to avoid green range (0.166 to 0.333)
  // Map [0, 0.166] -> [0, 0.166] (reds/oranges)
  // Map [0.166, 0.333] -> [0.333, 0.5] (blues)
  // Map [0.333, 1.0] -> [0.5, 1.0] (purples/reds)
  double base_hue;
  if (raw_hue < 0.166) {
    base_hue = raw_hue;  // Keep reds/oranges
  } else if (raw_hue < 0.333) {
    base_hue = raw_hue + 0.167;  // Shift greens to blues
  } else {
    base_hue = raw_hue;  // Keep purples/reds
  }

  // Create more vibrant colors for better visual distinction
  double saturation = 0.8;  // Increased saturation for more vibrant colors
  double lightness = 0.75;  // Slightly reduced lightness for better contrast

  // Convert HSL to RGB (simplified HSL to RGB conversion)
  double c = (1.0 - std::abs(2.0 * lightness - 1.0)) * saturation;
  double x = c * (1.0 - std::abs(std::fmod(base_hue * 6.0, 2.0) - 1.0));
  double m = lightness - c / 2.0;

  double r, g, b;
  if (base_hue < 1.0 / 6.0) {
    r = c;
    g = x;
    b = 0;
  } else if (base_hue < 2.0 / 6.0) {
    r = x;
    g = c;
    b = 0;
  } else if (base_hue < 3.0 / 6.0) {
    r = 0;
    g = c;
    b = x;
  } else if (base_hue < 4.0 / 6.0) {
    r = 0;
    g = x;
    b = c;
  } else if (base_hue < 5.0 / 6.0) {
    r = x;
    g = 0;
    b = c;
  } else {
    r = c;
    g = 0;
    b = x;
  }

  // Scale to 0-255 range with pastel effect
  std::uint8_t red = static_cast<std::uint8_t>((r + m) * 255.0 * 0.9 + 25);
  std::uint8_t green = static_cast<std::uint8_t>((g + m) * 255.0 * 0.9 + 25);
  std::uint8_t blue = static_cast<std::uint8_t>((b + m) * 255.0 * 0.9 + 25);

  return fmt::rgb{red, green, blue};
}

}  // namespace npda