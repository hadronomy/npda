#ifndef NPDA_TESTS_SUPPORT_FILES_H_
#define NPDA_TESTS_SUPPORT_FILES_H_

#include <filesystem>
#include <string>
#include <string_view>

namespace test_support {

std::filesystem::path SourceRoot();
std::string ReadFile(const std::filesystem::path& path);
void WriteFile(const std::filesystem::path& path, std::string_view text);

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
