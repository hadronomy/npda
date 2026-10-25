#include "support/files.h"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iterator>
#include <stdexcept>
#include <string>

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
  return {
      std::istreambuf_iterator<char>(input),
      std::istreambuf_iterator<char>()
  };
}

}  // namespace test_support
