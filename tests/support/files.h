#ifndef NPDA_TESTS_SUPPORT_FILES_H_
#define NPDA_TESTS_SUPPORT_FILES_H_

#include <filesystem>
#include <string>
#include <string_view>

namespace test_support {

/// Returns the repository root that `xmake test` gives.
std::filesystem::path SourceRoot();
std::string ReadFile(const std::filesystem::path& path);
void WriteFile(const std::filesystem::path& path, std::string_view text);

/// A new directory. The destructor removes the directory and all of its
/// contents.
class TemporaryDirectory {
 public:
  TemporaryDirectory();
  ~TemporaryDirectory();

  TemporaryDirectory(const TemporaryDirectory&) = delete;
  TemporaryDirectory& operator=(const TemporaryDirectory&) = delete;

  const std::filesystem::path& Path() const { return path_; }

 private:
  std::filesystem::path path_;
};

}  // namespace test_support

#endif  // NPDA_TESTS_SUPPORT_FILES_H_
