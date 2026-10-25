#ifndef NPDA_TESTS_SUPPORT_CLI_H_
#define NPDA_TESTS_SUPPORT_CLI_H_

#include <expected>
#include <string>
#include <system_error>
#include <vector>

namespace test_support {

struct ProcessOutput {
  int exit_code;
  std::string standard_output;
  std::string error_output;
};

/// Runs the CLI with plain output, no standard input, and a deadline of 30
/// seconds. Process failures are separate from the CLI's exit code.
std::expected<ProcessOutput, std::error_code> RunCli(
    std::vector<std::string> arguments
);

}  // namespace test_support

#endif  // NPDA_TESTS_SUPPORT_CLI_H_
