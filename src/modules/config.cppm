// Own the application config: palette, symbols, and identity.
// fmt stays in the global fragment for the color palette below.
// Change this file to change the API.
module;
#include <fmt/color.h>

export module config;

namespace npda {

// Type-safe enum for command types.
export enum class CommandType { Process, Analyze, Export };

}  // namespace npda

// Application configuration and constants.
export namespace npda::config::colors {
// Catppuccin Mocha palette.
inline constexpr auto banner_text = fmt::rgb(203, 166, 247);      // Mauve
inline constexpr auto banner_border = fmt::rgb(180, 190, 254);    // Lavender
inline constexpr auto section_heading = fmt::rgb(249, 226, 175);  // Yellow
inline constexpr auto command_name = fmt::rgb(250, 179, 135);     // Peach
inline constexpr auto option_name = fmt::rgb(137, 220, 235);      // Sky
inline constexpr auto example = fmt::rgb(116, 199, 236);          // Sapphire
inline constexpr auto success = fmt::rgb(166, 227, 161);          // Green
inline constexpr auto info = fmt::rgb(205, 214, 244);             // Text
inline constexpr auto warning = fmt::rgb(250, 179, 135);          // Peach
inline constexpr auto error = fmt::rgb(243, 139, 168);            // Red
inline constexpr auto progress = fmt::rgb(186, 194, 222);         // Subtext1
inline constexpr auto usage = fmt::rgb(148, 226, 213);            // Teal
}  // namespace npda::config::colors

export namespace npda::config::symbols {
// Symbols for status indicators.
inline constexpr auto success = "✓";
inline constexpr auto error = "✗";
inline constexpr auto warning = "!";
inline constexpr auto info = "ℹ";
inline constexpr auto arrow = "→";
}  // namespace npda::config::symbols

export namespace npda::config::formats {
// Format strings.
inline constexpr auto timestamp = "{:%H:%M:%S}";
}  // namespace npda::config::formats

export namespace npda::config {
// Application name and version.
inline constexpr const char* app_name = "npda";
inline constexpr const char* app_version = "0.2.0";
inline constexpr const char* app_description = "TODO";
inline constexpr const char* repo_url = "https://github.com/hadronomy/npda";
}  // namespace npda::config
