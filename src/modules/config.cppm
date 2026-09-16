// Wrap the application config header in one module.
// The header stays: the NPDA and Turing wrappers parse it as text.
// Keep the include in the global fragment above the module line.
// Re-export public names with export using.
module;
#include <fmt/color.h>
#include "config.h"

export module config;

namespace npda {

// Command kinds.
export using npda::CommandType;

}  // namespace npda

namespace npda::config {

// Catppuccin Mocha palette.
namespace colors {
export using npda::config::colors::banner_border;
export using npda::config::colors::banner_text;
export using npda::config::colors::command_name;
export using npda::config::colors::error;
export using npda::config::colors::example;
export using npda::config::colors::info;
export using npda::config::colors::option_name;
export using npda::config::colors::progress;
export using npda::config::colors::section_heading;
export using npda::config::colors::success;
export using npda::config::colors::usage;
export using npda::config::colors::warning;
}  // namespace colors

// Status symbols.
namespace symbols {
export using npda::config::symbols::arrow;
export using npda::config::symbols::error;
export using npda::config::symbols::info;
export using npda::config::symbols::success;
export using npda::config::symbols::warning;
}  // namespace symbols

// Format strings.
namespace formats {
export using npda::config::formats::timestamp;
}  // namespace formats

// Application name and version.
export using npda::config::app_description;
export using npda::config::app_name;
export using npda::config::app_version;
export using npda::config::repo_url;

}  // namespace npda::config
