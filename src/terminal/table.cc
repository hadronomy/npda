#include "terminal/table.h"

#include <algorithm>
#include <cstddef>
#include <ostream>
#include <print>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "terminal/style.h"

namespace terminal {

void Table::AddRow(std::vector<Styled> cells) {
  lines_.emplace_back(std::move(cells));
}

void Table::AddLine(Styled line) { lines_.emplace_back(std::move(line)); }

void Table::Write(std::ostream& out) const {
  std::vector<std::size_t> widths;
  for (const auto& line : lines_) {
    const auto* cells = std::get_if<std::vector<Styled>>(&line);
    if (cells == nullptr) continue;
    widths.resize(std::max(widths.size(), cells->size()));
    for (std::size_t column = 0; column < cells->size(); ++column) {
      widths[column] = std::max(widths[column], (*cells)[column].width);
    }
  }
  for (const auto& line : lines_) {
    if (const auto* free = std::get_if<Styled>(&line)) {
      std::println(out, "  {}", free->text);
      continue;
    }
    const auto& cells = std::get<std::vector<Styled>>(line);
    auto end = cells.size();
    while (end > 0 && cells[end - 1].width == 0) --end;
    std::string text;
    for (std::size_t column = 0; column < end; ++column) {
      if (column > 0) text += "  ";
      text += column + 1 == end ? cells[column].text
                                : PadRight(cells[column], widths[column]);
    }
    std::println(out, "  {}", text);
  }
}

}  // namespace terminal
