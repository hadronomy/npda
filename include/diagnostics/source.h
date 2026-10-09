#ifndef NPDA_DIAGNOSTICS_SOURCE_H_
#define NPDA_DIAGNOSTICS_SOURCE_H_

#include <cstddef>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace diag {

/// Byte offsets `[lo, hi)` into the text of a `SourceFile`.
struct SourceSpan {
  std::size_t lo = 0;
  std::size_t hi = 0;

  [[nodiscard]] bool Empty() const { return lo >= hi; }
};

struct SourceFile {
  std::string filename;
  std::string text;
  /// The byte offset where each line starts.
  std::vector<std::size_t> line_starts;

  [[nodiscard]] static SourceFile From(std::string filename, std::string text);

  [[nodiscard]] std::size_t LineCount() const;

  /// Returns the line and the column of a byte offset. Both start at one. An
  /// offset after the end of the text gives the last position.
  [[nodiscard]] std::pair<std::size_t, std::size_t> LineColumn(
      std::size_t offset
  ) const;

  /// Returns the line `number`, without its line terminator. Lines start at
  /// one. Returns an empty view if the line does not exist. The view points
  /// into `text`.
  [[nodiscard]] std::string_view Line(std::size_t number) const;

  /// Returns the span of `part`. `part` must be a view into `text`.
  [[nodiscard]] SourceSpan SpanOf(std::string_view part) const;
};

}  // namespace diag

#endif  // NPDA_DIAGNOSTICS_SOURCE_H_
