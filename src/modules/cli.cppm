// Wrap the CLI layer headers in one module.
// Keep all includes in the global fragment above the module line.
// Re-export public names with export using.
// Change the headers to change the API. Add no code here.
module;
#include <CLI/CLI.hpp>
#include <fmt/color.h>
#include <fmt/core.h>
#include "config.h"
#include "colorized_formatter.h"
#include "ui.h"
#include "cli.h"

export module cli;

// Command registry. Lives in the global namespace.
export using ::CommandContext;
export using ::CommandHandler;
export using ::CommandRegistry;

namespace npda {

// Colored help text for CLI11.
export using npda::ColorizedFormatter;

}  // namespace npda

namespace ui {

// Small status helpers.
export using ui::error;
export using ui::info;

}  // namespace ui
