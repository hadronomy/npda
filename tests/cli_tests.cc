#include <cstddef>
#include <filesystem>
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
      OutputFixture{"turing_help", {"turing", "-help"}},
      OutputFixture{
          "turing_result",
          {"turing",
           "-config",
           "examples/turing/anbm.turing",
           "-trace",
           "n",
           "ab",
           "aabb",
           "abb",
           "aab",
           "ba",
           ".",
           "abc"}
      },
      OutputFixture{
          "turing_count_result",
          {"turing",
           "-config",
           "examples/turing/count_replace.turing",
           "-trace",
           "n",
           "abbabaabb",
           "aa",
           "bb",
           "."}
      },
      OutputFixture{
          "turing_detailed_trace",
          {"turing",
           "-config",
           "examples/turing/anbm.turing",
           "-trace-limit",
           "3",
           "ab"}
      },
      OutputFixture{
          "turing_compact_trace",
          {"turing",
           "-config",
           "examples/turing/anbm.turing",
           "-trace-view",
           "compact",
           "abb"}
      },
      OutputFixture{
          "turing_independent_trace",
          {"turing",
           "-config",
           "examples/turing/successor.turing",
           "-trace-view",
           "compact",
           "-explain",
           "11"}
      },
      OutputFixture{
          "turing_diagnostics",
          {"turing", "-config", "examples/turing/anbm_bad.turing", "a"}
      },
      OutputFixture{"explain", {"explain", "E0006"}},
      OutputFixture{"explain_unknown", {"explain", "BOGUS"}}
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

TEST_CASE(
    "CLI follows the course statement's option spelling",
    "[cli][options]"
) {
  test_support::TemporaryDirectory directory;
  const auto inputs = directory.Path() / "inputs.txt";
  const auto trace = directory.Path() / "trace.txt";
  test_support::WriteFile(inputs, "aabb\n\naab\n.\n");

  SECTION("-in reads one input per line; '.' is the empty string") {
    const auto result = test_support::RunCli(
        {"turing",
         "-config",
         "examples/turing/anbm.turing",
         "-in",
         inputs.string()}
    );
    REQUIRE(result.has_value());
    CHECK(result->exit_code == 0);
    CHECK(result->standard_output.contains("reject  ε"));
    CHECK(
        result->standard_output.contains("3 inputs · 1 accepted · 2 rejected")
    );
  }
  SECTION("-out sends the trace to a file and keeps results on screen") {
    const auto result = test_support::RunCli(
        {"turing",
         "-config",
         "examples/turing/anbm.turing",
         "-trace",
         "y",
         "-in",
         inputs.string(),
         "-out",
         trace.string()}
    );
    REQUIRE(result.has_value());
    CHECK(result->exit_code == 0);
    CHECK_FALSE(result->standard_output.contains("trace aabb"));
    CHECK(result->standard_output.contains("accept  aabb"));
    const auto written = test_support::ReadFile(trace);
    CHECK(written.contains("trace aabb"));
    CHECK(written.contains("accept  aabb"));
    CHECK_FALSE(written.contains('\x1b'));
  }
  SECTION("the trace shows by default") {
    const auto result = test_support::RunCli(
        {"turing", "-config", "examples/turing/anbm.turing", "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK(result->standard_output.contains("trace aabb"));
  }
  SECTION("-trace n prints results only") {
    const auto result = test_support::RunCli(
        {"turing",
         "-config",
         "examples/turing/anbm.turing",
         "-trace",
         "n",
         "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK_FALSE(result->standard_output.contains("trace"));
  }
  SECTION("-trace takes only y or n") {
    const auto result = test_support::RunCli(
        {"turing",
         "-config",
         "examples/turing/anbm.turing",
         "-trace",
         "yes",
         "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK(result->exit_code != 0);
    CHECK(result->error_output.contains("expected y|n, not 'yes'"));
  }
}

TEST_CASE("CLI reads input from the keyboard", "[cli][io]") {
  const auto result = test_support::RunCli(
      {"turing", "-config", "examples/turing/anbm.turing"},
      "aabb\n.\n"
  );
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 0);
  CHECK(result->standard_output.contains("2 inputs · 1 accepted · 1 rejected"));
}

TEST_CASE("CLI exits with failure when a run does not halt", "[cli][limits]") {
  test_support::TemporaryDirectory directory;
  const auto path = directory.Path() / "loop.turing";
  const auto stays_forever = "q\na\na _\nq\n_\n1\nq a q a S\n";
  test_support::WriteFile(path, stays_forever);
  const auto result = test_support::RunCli(
      {"turing", "-config", path.string(), "-trace", "n", "a"}
  );
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 1);
  CHECK(result->standard_output.contains(
      "unknown a  still running after 100000 steps"
  ));
  CHECK(result->standard_output.contains("1 undecided"));
}

TEST_CASE(
    "Every example runs without crashing and produces output",
    "[cli][corpus]"
) {
  std::size_t example_count = 0;
  for (const auto& entry : std::filesystem::directory_iterator(
           test_support::SourceRoot() / "examples/turing"
       )) {
    if (entry.path().extension() != ".turing") continue;
    CAPTURE(entry.path());
    ++example_count;
    const auto result = test_support::RunCli(
        {"turing", "-config", entry.path().string(), "-trace", "y", "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK((result->exit_code == 0 || result->exit_code == 1));
    CHECK(!(result->standard_output + result->error_output).empty());
  }
  CHECK(example_count > 0);
}

}  // namespace
