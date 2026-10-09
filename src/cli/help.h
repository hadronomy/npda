#ifndef NPDA_SRC_CLI_HELP_H_
#define NPDA_SRC_CLI_HELP_H_

#include <memory>

#include <CLI/FormatterFwd.hpp>

namespace cli {

/// Returns the formatter for all of the help pages.
///
/// A page has a title, a usage line, the arguments, the options, the
/// subcommands, and the examples. The examples come from the footer of the
/// command, one on each line.
[[nodiscard]] std::shared_ptr<CLI::FormatterBase> MakeHelpFormatter();

}  // namespace cli

#endif  // NPDA_SRC_CLI_HELP_H_
