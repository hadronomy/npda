#ifndef NPDA_TESTS_SUPPORT_CLI_H_
#define NPDA_TESTS_SUPPORT_CLI_H_

#include <expected>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

namespace test_support {

struct ProcessOutput {
  int exit_code;
  std::string standard_output;
  std::string error_output;
};

// Runs the built CLI with plain output and a 30-second deadline. Empty input
// supplies EOF. Process failures are separate from the CLI's exit code.
std::expected<ProcessOutput, std::error_code> RunCli(
    std::vector<std::string> arguments, std::string_view input = {});

}  // namespace test_support

#endif  // NPDA_TESTS_SUPPORT_CLI_H_
