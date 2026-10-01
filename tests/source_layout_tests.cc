#include <algorithm>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <regex>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>

#include "support/files.h"

namespace {

std::vector<std::filesystem::path> SourceFiles() {
  std::vector<std::filesystem::path> paths;
  for (const auto* directory : {"src", "include", "tests"}) {
    for (const auto& entry : std::filesystem::recursive_directory_iterator(
             test_support::SourceRoot() / directory)) {
      if (!entry.is_regular_file()) continue;
      const auto relative =
          entry.path().lexically_relative(test_support::SourceRoot());
      const auto extension = entry.path().extension();
      if (extension == ".h" || extension == ".cc" || extension == ".cppm") {
        paths.push_back(relative);
      }
    }
  }
  std::ranges::sort(paths);
  return paths;
}

std::string HeaderGuard(std::filesystem::path path) {
  if (path.generic_string().starts_with("include/")) {
    path = path.lexically_relative("include");
  }
  std::string guard = path.generic_string();
  for (char& character : guard) {
    const auto byte = static_cast<unsigned char>(character);
    character =
        std::isalnum(byte) ? static_cast<char>(std::toupper(byte)) : '_';
  }
  return "NPDA_" + guard + '_';
}

TEST_CASE("Project headers use path-based include guards",
          "[source][headers]") {
  for (const auto& path : SourceFiles()) {
    if (path.extension() != ".h") continue;
    CAPTURE(path);
    const std::string text =
        test_support::ReadFile(test_support::SourceRoot() / path);
    const std::string guard = HeaderGuard(path);
    CHECK(text.starts_with("#ifndef " + guard + "\n#define " + guard + '\n'));
  }
}

TEST_CASE("Project sources use includes and separated declaration blocks",
          "[source][format]") {
  const std::regex module_declaration(R"(^(export\s+)?(module|import)\b.*)");
  const std::regex namespace_opener(R"(^namespace [\w:]* \{)");
  const std::regex type_declaration(R"(^(struct|class|enum)\b.*)");
  for (const auto& path : SourceFiles()) {
    CAPTURE(path);
    CHECK(path.extension() != ".cppm");
    std::istringstream input(
        test_support::ReadFile(test_support::SourceRoot() / path));
    std::vector<std::string> lines;
    for (std::string line; std::getline(input, line);)
      lines.push_back(std::move(line));
    std::vector<std::string> errors;
    for (std::size_t index = 0; index < lines.size(); ++index) {
      const auto& line = lines[index];
      if (std::regex_match(line, module_declaration)) {
        errors.push_back("Named module declaration at line " +
                         std::to_string(index + 1));
      }
      if (index + 1 == lines.size()) continue;
      const auto& following = lines[index + 1];
      if ((line.starts_with("#include") &&
           following.starts_with("namespace ")) ||
          (std::regex_match(line, namespace_opener) && !following.empty()) ||
          (line.starts_with("using ") &&
           std::regex_match(following, type_declaration))) {
        errors.push_back("Missing blank line at line " +
                         std::to_string(index + 2));
      }
    }
    CAPTURE(errors);
    CHECK(errors.empty());
  }
}

}  // namespace
