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
      OutputFixture{"npda_help", {"npda", "-help"}},
      OutputFixture{
          "npda_result",
          {"npda",
           "-config",
           "examples/APf/APf-1.txt",
           "-trace",
           "n",
           "aabb",
           "aab",
           "."}
      },
      OutputFixture{
          "npda_detailed_trace",
          {"npda",
           "-config",
           "examples/APf/APf-1.txt",
           "-explain",
           "aabb",
           "aab"}
      },
      OutputFixture{
          "npda_compact_trace",
          {"npda",
           "-config",
           "examples/APv/APv-2.txt",
           "-trace-view",
           "compact",
           "-trace-limit",
           "8",
           "0110"}
      },
      OutputFixture{
          "npda_tree_trace",
          {"npda",
           "-config",
           "examples/APv/APv-2.txt",
           "-trace-view",
           "tree",
           "0110"}
      },
      OutputFixture{
          "npda_diagnostics",
          {"npda", "-config", "examples/APv/APv-jueves-1.txt", "a"}
      },
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
      OutputFixture{"prf_missing_exponent", {"prf", "-x", "2"}},
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

  SECTION("-in reads one input per line and treats '.' as the empty string") {
    const auto result = test_support::RunCli(
        {"npda", "-config", "examples/APf/APf-1.txt", "-in", inputs.string()}
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
        {"npda",
         "-config",
         "examples/APf/APf-1.txt",
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
  SECTION("the trace is shown by default") {
    const auto result = test_support::RunCli(
        {"npda", "-config", "examples/APf/APf-1.txt", "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK(result->standard_output.contains("trace aabb"));
  }
  SECTION("-trace n prints results only") {
    const auto result = test_support::RunCli(
        {"npda", "-config", "examples/APf/APf-1.txt", "-trace", "n", "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK_FALSE(result->standard_output.contains("trace"));
  }
  SECTION("-trace takes only y or n") {
    const auto result = test_support::RunCli(
        {"npda", "-config", "examples/APf/APf-1.txt", "-trace", "yes", "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK(result->exit_code != 0);
    CHECK(result->error_output.contains("expected y|n, not 'yes'"));
  }
}

TEST_CASE("CLI reads NPDA input from the keyboard", "[cli][io]") {
  const auto result = test_support::RunCli(
      {"npda", "-config", "examples/APf/APf-1.txt"},
      "aabb\n.\n"
  );
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 0);
  CHECK(result->standard_output.contains("2 inputs · 1 accepted · 1 rejected"));
}

TEST_CASE(
    "CLI exits with failure when a search cannot decide",
    "[cli][limits]"
) {
  test_support::TemporaryDirectory directory;
  const auto path = directory.Path() / "loop.npda";
  const auto pushes_forever = "q\na\nS\nq\nS\nq . S q SS\n";
  test_support::WriteFile(path, pushes_forever);
  const auto result = test_support::RunCli(
      {"npda", "-config", path.string(), "-trace", "n", "a"}
  );
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 1);
  CHECK(result->standard_output.contains(
      "unknown a  stopped after 100\u202F000 expansions"
  ));
  CHECK(result->standard_output.contains("1 undecided"));
}

TEST_CASE(
    "Every example runs without crashing and produces output",
    "[cli][corpus]"
) {
  const auto directory =
      GENERATE("examples/APf", "examples/APv", "examples/turing");
  const bool is_turing = std::string_view(directory) == "examples/turing";
  std::size_t example_count = 0;
  for (const auto& entry : std::filesystem::directory_iterator(
           test_support::SourceRoot() / directory
       )) {
    if (entry.path().extension() != (is_turing ? ".turing" : ".txt")) continue;
    CAPTURE(entry.path());
    ++example_count;
    const auto result = test_support::RunCli(
        {is_turing ? "turing" : "npda",
         "-config",
         entry.path().string(),
         "-trace",
         "y",
         "aabb"}
    );
    REQUIRE(result.has_value());
    CHECK((result->exit_code == 0 || result->exit_code == 1));
    CHECK(!(result->standard_output + result->error_output).empty());
  }
  CHECK(example_count > 0);
}

}  // namespace
