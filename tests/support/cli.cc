#include "support/cli.h"

#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include <reproc++/drain.hpp>
#include <reproc++/env.hpp>
#include <reproc++/input.hpp>
#include <reproc++/reproc.hpp>
#include <reproc++/run.hpp>

#include "support/files.h"

namespace test_support {

std::expected<ProcessOutput, std::error_code> RunCli(
    std::vector<std::string> arguments, std::string_view input) {
  const char* binary = std::getenv("NPDA_CLI_BINARY");
  if (binary == nullptr)
    throw std::runtime_error("Run the tests through xmake test");
  arguments.insert(arguments.begin(), binary);

  const std::string root = SourceRoot().string();
  reproc::options options;
  options.working_directory = root.c_str();
  options.env.extra = std::vector<std::pair<std::string, std::string>>{
      {"NO_COLOR", "1"}, {"TERM", "dumb"}, {"LC_ALL", "C.UTF-8"}};
  options.redirect.err.type = reproc::redirect::pipe;
  options.deadline = std::chrono::seconds(30);
  options.stop = {{reproc::stop::wait, reproc::deadline},
                  {reproc::stop::terminate, std::chrono::seconds(1)},
                  {reproc::stop::kill, reproc::infinite}};
  if (input.empty()) {
    options.redirect.in.type = reproc::redirect::discard;
  } else {
    options.input = reproc::input(
        reinterpret_cast<const std::uint8_t*>(input.data()), input.size());
  }

  ProcessOutput output{};
  const auto [exit_code, error] = reproc::run(
      arguments, options, reproc::sink::string(output.standard_output),
      reproc::sink::string(output.error_output));
  if (error) return std::unexpected(error);
  output.exit_code = exit_code;
  return output;
}

}  // namespace test_support
