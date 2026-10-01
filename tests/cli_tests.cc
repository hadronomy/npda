#include <cstddef>
#include <filesystem>
#include <format>
#include <regex>
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

std::string StableOutput(const test_support::ProcessOutput& output) {
  std::string text = output.standard_output + output.error_output;
  text = std::regex_replace(text, std::regex(R"(\[\s*\d+\.\d+s\])"), "[TIME]");
  text = std::regex_replace(text, std::regex(R"(\d+\.\d+s\b)"), "TIME");
  return std::format("exit={}\n{}", output.exit_code, text);
}

TEST_CASE("CLI output matches the complete behavior fixtures",
          "[cli][output]") {
  const auto fixture = GENERATE(
      OutputFixture{"npda_result",
                    {"npda", "examples/APf/APf-1.txt", "aabb", "aab"}},
      OutputFixture{"npda_trace",
                    {"npda", "examples/APf/APf-1.txt", "aabb", "--trace",
                     "--trace-limit", "8"}},
      OutputFixture{"npda_reject_trace",
                    {"npda", "examples/APf/APf-1.txt", "aab", "--trace",
                     "--trace-limit", "8"}},
      OutputFixture{"npda_empty_stack",
                    {"npda", "examples/APv/APv-2.txt", "0110", "010"}},
      OutputFixture{"turing_result",
                    {"turing", "examples/turing/anbm.turing", "aabb", "abb"}},
      OutputFixture{"turing_trace",
                    {"turing", "examples/turing/anbm.turing", "aabb", "--trace",
                     "--trace-limit", "8"}},
      OutputFixture{"prf_counts", {"prf", "2", "3", "--mode", "counts-only"}},
      OutputFixture{"prf_full", {"prf", "1", "2", "--mode", "full"}},
      OutputFixture{"prf_off", {"prf", "2", "3", "--mode", "off"}},
      OutputFixture{"explain", {"explain", "E0007"}},
      OutputFixture{"explain_unknown", {"explain", "BOGUS"}});
  CAPTURE(fixture.name);
  const auto result = test_support::RunCli(fixture.arguments);
  REQUIRE(result.has_value());
  const auto expected =
      test_support::ReadFile(test_support::SourceRoot() / "tests/expected" /
                             std::format("{}.txt", fixture.name));
  CHECK(StableOutput(*result) == expected);
}

TEST_CASE("CLI help describes PRF trace modes", "[cli][help]") {
  const auto result = test_support::RunCli({"prf", "--help"});
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 0);
  CHECK(result->standard_output.contains("Trace mode"));
}

TEST_CASE("CLI reads NPDA input from a file", "[cli][io]") {
  test_support::TemporaryDirectory directory;
  const auto input_path = directory.Path() / "input.txt";
  test_support::WriteFile(input_path, "aabb\naab\n");
  const auto result = test_support::RunCli(
      {"npda", "examples/APf/APf-1.txt", "--in", input_path.string()});
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 0);
  CHECK(result->standard_output.contains("1 accepted, 1 rejected"));
}

TEST_CASE("CLI reads NPDA input from standard input", "[cli][io]") {
  const auto result =
      test_support::RunCli({"npda", "examples/APf/APf-1.txt"}, "aabb\n");
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 0);
  CHECK(result->standard_output.contains("1 accepted, 0 rejected"));
}

TEST_CASE("CLI writes NPDA traces to the selected file", "[cli][io]") {
  test_support::TemporaryDirectory directory;
  const auto input_path = directory.Path() / "input.txt";
  const auto output_path = directory.Path() / "trace.txt";
  test_support::WriteFile(input_path, "aabb\naab\n");
  const auto result = test_support::RunCli(
      {"npda", "examples/APf/APf-1.txt", "--in", input_path.string(), "--trace",
       "--out", output_path.string()});
  REQUIRE(result.has_value());
  CHECK(result->exit_code == 0);
  CHECK(test_support::ReadFile(output_path).contains("── Step 0"));
}

TEST_CASE("Every example runs without crashing and produces output",
          "[cli][corpus]") {
  const auto directory =
      GENERATE("examples/APf", "examples/APv", "examples/turing");
  const bool is_turing = std::string_view(directory) == "examples/turing";
  std::size_t example_count = 0;
  for (const auto& entry : std::filesystem::directory_iterator(
           test_support::SourceRoot() / directory)) {
    if (entry.path().extension() != (is_turing ? ".turing" : ".txt")) continue;
    CAPTURE(entry.path());
    ++example_count;
    const auto result = test_support::RunCli(
        {is_turing ? "turing" : "npda", entry.path().string(), "aabb"});
    REQUIRE(result.has_value());
    CHECK((result->exit_code == 0 || result->exit_code == 1));
    CHECK(!(result->standard_output + result->error_output).empty());
  }
  CHECK(example_count > 0);
}

}  // namespace
