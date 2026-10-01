#include "support/files.h"

#if defined(__APPLE__)
#include <unistd.h>
#else
#include <stdlib.h>
#endif

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>

namespace test_support {

std::filesystem::path SourceRoot() {
  const char* root = std::getenv("NPDA_SOURCE_ROOT");
  if (root == nullptr)
    throw std::runtime_error("Run the tests through xmake test");
  return root;
}

std::string ReadFile(const std::filesystem::path& path) {
  std::ifstream input(path, std::ios::binary);
  if (!input) throw std::runtime_error("Cannot read " + path.string());
  return {std::istreambuf_iterator<char>(input),
          std::istreambuf_iterator<char>()};
}

void WriteFile(const std::filesystem::path& path, std::string_view text) {
  std::ofstream output(path, std::ios::binary);
  output << text;
  if (!output) throw std::runtime_error("Cannot write " + path.string());
}

TemporaryDirectory::TemporaryDirectory() {
  std::string pattern =
      (std::filesystem::temp_directory_path() / "npda-tests-XXXXXX").string();
  const char* directory = mkdtemp(pattern.data());
  if (directory == nullptr) {
    throw std::runtime_error("Cannot create a temporary test directory");
  }
  path_ = directory;
}

TemporaryDirectory::~TemporaryDirectory() {
  std::error_code ignored;
  std::filesystem::remove_all(path_, ignored);
}

}  // namespace test_support
