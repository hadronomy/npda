#ifndef NPDA_DIAGNOSTICS_SOURCE_H_
#define NPDA_DIAGNOSTICS_SOURCE_H_

#include <cstddef>
#include <functional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace diag {

struct SourceSpan {
  std::size_t lo = 0;  // inclusive
  std::size_t hi = 0;  // exclusive
  std::string file{};  // empty means the source under render

  [[nodiscard]] bool empty() const { return lo >= hi; }
};

struct SourceFile {
  std::string filename;
  std::string text;
  std::vector<std::size_t> line_starts;  // offsets of each line start

  [[nodiscard]] static SourceFile From(std::string filename, std::string text);

  [[nodiscard]] std::size_t line_count() const;

  [[nodiscard]] std::pair<std::size_t, std::size_t> line_col(
      std::size_t pos) const;

  [[nodiscard]] std::string_view line_view(std::size_t line_1) const;
};

// Owns named sources for multi-file renders. Memoizes parsed files.
// Heterogeneous lookup takes string_view keys with no allocation.
struct TransparentHash {
  using is_transparent = void;

  [[nodiscard]] std::size_t operator()(std::string_view s) const noexcept {
    return std::hash<std::string_view>{}(s);
  }
};

struct SourceCache {
  std::unordered_map<std::string, SourceFile, TransparentHash, std::equal_to<>>
      files;

  const SourceFile* Find(std::string_view name) const {
    const auto it = files.find(name);
    return it == files.end() ? nullptr : &it->second;
  }

  const SourceFile& Insert(SourceFile sf) {
    const auto [it, _] = files.insert_or_assign(sf.filename, std::move(sf));
    return it->second;
  }
};

}  // namespace diag

#endif  // NPDA_DIAGNOSTICS_SOURCE_H_
