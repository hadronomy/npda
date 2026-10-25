#ifndef NPDA_SRC_CLI_HELP_H_
#define NPDA_SRC_CLI_HELP_H_

#include <memory>

#include <CLI/FormatterFwd.hpp>

namespace cli {

/// Formats each help page with its title, usage, arguments, options,
/// subcommands, and examples, taking one example per line from the command's
/// footer.
[[nodiscard]] std::shared_ptr<CLI::FormatterBase> MakeHelpFormatter();

}  // namespace cli

#endif  // NPDA_SRC_CLI_HELP_H_
