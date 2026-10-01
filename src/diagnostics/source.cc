#include "diagnostics/source.h"

#include <algorithm>
#include <cstddef>
#include <iterator>
#include <string>
#include <string_view>
#include <utility>

namespace diag {

SourceFile SourceFile::From(std::string filename, std::string text) {
  SourceFile sf;
  sf.filename = std::move(filename);
  sf.text = std::move(text);

  std::size_t newline_count = 0;
  for (char c : sf.text) {
    if (c == '\n') {
      newline_count++;
    }
  }
  sf.line_starts.reserve(newline_count + 1);
  sf.line_starts.push_back(0);
  for (std::size_t i = 0; i < sf.text.size(); ++i) {
    if (sf.text[i] == '\n') sf.line_starts.push_back(i + 1);
  }
  return sf;
}

std::size_t SourceFile::line_count() const { return line_starts.size(); }

std::pair<std::size_t, std::size_t> SourceFile::line_col(
    std::size_t pos) const {
  if (line_starts.empty()) return {1, 1};
  pos = std::min(pos, text.size());
  auto it = std::upper_bound(line_starts.begin(), line_starts.end(), pos);
  std::size_t line =
      (it == line_starts.begin())
          ? 1
          : static_cast<std::size_t>(std::distance(line_starts.begin(), it));

  if (line == 0) line = 1;
  if (line > line_starts.size()) line = line_starts.size();

  std::size_t start = line_starts[line - 1];
  std::size_t col = (pos >= start) ? (pos - start + 1) : 1;
  return {line, col};
}

std::string_view SourceFile::line_view(std::size_t line_1) const {
  if (line_1 == 0 || line_1 > line_starts.size()) return {};
  std::size_t start = line_starts[line_1 - 1];
  std::size_t end = text.size();
  if (line_1 < line_starts.size()) end = line_starts[line_1] - 1;
  if (end > start && text[end - 1] == '\r') --end;
  return std::string_view(text).substr(start, end - start);
}

}  // namespace diag
