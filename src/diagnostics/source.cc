#include "diagnostics/source.h"

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <string>
#include <string_view>
#include <utility>

namespace diag {

SourceFile SourceFile::From(std::string filename, std::string text) {
  SourceFile file{
      .filename = std::move(filename),
      .text = std::move(text),
      .line_starts = {0},
  };
  for (std::size_t index = 0; index < file.text.size(); ++index) {
    if (file.text[index] == '\n') file.line_starts.push_back(index + 1);
  }
  return file;
}

std::size_t SourceFile::LineCount() const { return line_starts.size(); }

std::pair<std::size_t, std::size_t> SourceFile::LineColumn(
    std::size_t offset
) const {
  offset = std::min(offset, text.size());
  const auto next = std::ranges::upper_bound(line_starts, offset);
  const auto line = static_cast<std::size_t>(
      std::max<std::ptrdiff_t>(std::distance(line_starts.begin(), next), 1)
  );
  return {line, offset - line_starts[line - 1] + 1};
}

std::string_view SourceFile::Line(std::size_t number) const {
  if (number == 0 || number > line_starts.size()) return {};
  const auto start = line_starts[number - 1];
  auto end =
      number < line_starts.size() ? line_starts[number] - 1 : text.size();
  if (end > start && text[end - 1] == '\r') --end;
  return std::string_view(text).substr(start, end - start);
}

SourceSpan SourceFile::SpanOf(std::string_view part) const {
  const auto lo = static_cast<std::size_t>(part.data() - text.data());
  return {.lo = lo, .hi = lo + part.size()};
}

}  // namespace diag
