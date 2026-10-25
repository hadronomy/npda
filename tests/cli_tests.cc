#include <format>
#include <string>
#include <string_view>
#include <vector>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "support/cli.h"
#include "support/files.h"

namespace {

struct OutputFixture {
  std::string_view name;
  std::vector<std::string> arguments;
};

std::string Transcript(const test_support::ProcessOutput& output) {
  return std::format(
      "exit={}\n{}{}",
      output.exit_code,
      output.standard_output,
      output.error_output
  );
}

TEST_CASE(
    "CLI output matches the complete behavior fixtures",
    "[cli][output]"
) {
  const auto fixture = GENERATE(
      OutputFixture{"help", {"-help"}},
      OutputFixture{"prf_help", {"prf", "-help"}},
      OutputFixture{"prf_counts", {"prf", "-x", "2", "-y", "3"}},
      OutputFixture{
          "prf_tree",
          {"prf", "-x", "1", "-y", "2", "-trace", "tree"}
      },
      OutputFixture{
          "prf_tree_limit",
          {"prf", "-x", "2", "-y", "3", "-trace", "tree", "-trace-limit", "4"}
      },
      OutputFixture{"prf_off", {"prf", "-x", "2", "-y", "3", "-trace", "off"}},
      OutputFixture{"prf_overflow", {"prf", "-x", "2", "-y", "64"}},
      OutputFixture{
          "prf_call_limit",
          {"prf", "-x", "2", "-y", "30", "-max-calls", "100000"}
      },
      OutputFixture{
          "prf_large",
          {"prf", "-x", "2", "-y", "20", "-trace", "off"}
      },
      OutputFixture{
          "prf_definition",
          {"prf", "-x", "2", "-y", "3", "-definition", "-trace", "off"}
      },
      OutputFixture{"prf_missing_exponent", {"prf", "-x", "2"}}
  );
  CAPTURE(fixture.name);
  const auto result = test_support::RunCli(fixture.arguments);
  REQUIRE(result.has_value());
  const auto expected = test_support::ReadFile(
      test_support::SourceRoot() / "tests/expected" /
      std::format("{}.txt", fixture.name)
  );
  CHECK(Transcript(*result) == expected);
}

}  // namespace
