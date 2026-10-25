#ifndef NPDA_TESTS_SUPPORT_FILES_H_
#define NPDA_TESTS_SUPPORT_FILES_H_

#include <filesystem>
#include <string>

namespace test_support {

/// Returns the repository root that `xmake test` gives.
std::filesystem::path SourceRoot();
std::string ReadFile(const std::filesystem::path& path);

}  // namespace test_support

#endif  // NPDA_TESTS_SUPPORT_FILES_H_
