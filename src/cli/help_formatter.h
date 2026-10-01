#ifndef NPDA_SRC_CLI_HELP_FORMATTER_H_
#define NPDA_SRC_CLI_HELP_FORMATTER_H_

#include <memory>

#include <CLI/Formatter.hpp>

namespace cli {

[[nodiscard]] std::shared_ptr<CLI::Formatter> MakeHelpFormatter();

}  // namespace cli

#endif  // NPDA_SRC_CLI_HELP_FORMATTER_H_
