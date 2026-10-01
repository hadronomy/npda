#ifndef NPDA_SRC_TERMINAL_PALETTE_H_
#define NPDA_SRC_TERMINAL_PALETTE_H_

#include "terminal/ansi.h"

// Application configuration and constants.
namespace terminal::colors {

// Catppuccin Mocha palette.
inline constexpr auto kBannerText = ansi::Rgb{203, 166, 247};      // Mauve
inline constexpr auto kBannerBorder = ansi::Rgb{180, 190, 254};    // Lavender
inline constexpr auto kSectionHeading = ansi::Rgb{249, 226, 175};  // Yellow
inline constexpr auto kCommandName = ansi::Rgb{250, 179, 135};     // Peach
inline constexpr auto kOptionName = ansi::Rgb{137, 220, 235};      // Sky
inline constexpr auto kExample = ansi::Rgb{116, 199, 236};         // Sapphire
inline constexpr auto kSuccess = ansi::Rgb{166, 227, 161};         // Green
inline constexpr auto kInfo = ansi::Rgb{205, 214, 244};            // Text
inline constexpr auto kWarning = ansi::Rgb{250, 179, 135};         // Peach
inline constexpr auto kError = ansi::Rgb{243, 139, 168};           // Red
inline constexpr auto kProgress = ansi::Rgb{186, 194, 222};        // Subtext1
inline constexpr auto kUsage = ansi::Rgb{148, 226, 213};           // Teal

}  // namespace terminal::colors

namespace terminal::symbols {

// Symbols for status indicators.
inline constexpr auto kSuccess = "✓";
inline constexpr auto kError = "✗";
inline constexpr auto kWarning = "!";
inline constexpr auto kInfo = "ℹ";
inline constexpr auto kArrow = "→";

}  // namespace terminal::symbols

namespace terminal {

// Application name and version.
inline constexpr const char* kAppVersion = "0.2.0";
inline constexpr const char* kRepoUrl = "https://github.com/hadronomy/npda";

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_PALETTE_H_
