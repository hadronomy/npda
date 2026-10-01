#ifndef NPDA_SRC_CLI_COMMANDS_H_
#define NPDA_SRC_CLI_COMMANDS_H_

#include <CLI/App.hpp>

namespace cli {

struct GlobalOptions {
  bool verbose = false;
};

void RegisterNpda(CLI::App& app, const GlobalOptions& options, int& exit_code);
void RegisterTuring(CLI::App& app, const GlobalOptions& options,
                    int& exit_code);
void RegisterPrf(CLI::App& app, const GlobalOptions& options, int& exit_code);
void RegisterExplain(CLI::App& app, const GlobalOptions& options,
                     int& exit_code);

}  // namespace cli

#endif  // NPDA_SRC_CLI_COMMANDS_H_
