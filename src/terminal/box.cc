#include "terminal/box.h"

#include <algorithm>
#include <cstddef>
#include <span>
#include <string>
#include <string_view>

#include "terminal/style.h"

namespace terminal {

namespace {

std::string Repeat(std::string_view glyph, std::size_t count) {
  std::string text;
  for (std::size_t index = 0; index < count; ++index) text += glyph;
  return text;
}

}  // namespace

BoxedRow DrawBoxedRow(std::span<const BoxCell> cells, const Style& style) {
  const auto& glyphs = style.glyphs();
  std::string top(glyphs.box_top_left);
  std::string bottom(glyphs.box_bottom_left);
  const auto border = style.Paint(Tone::kMuted, glyphs.box_vertical);
  BoxedRow row{.top = "", .middle = border, .bottom = "", .centers = {}};
  std::size_t column = 1;
  for (std::size_t index = 0; index < cells.size(); ++index) {
    const auto& cell = cells[index];
    const auto width = std::max<std::size_t>(DisplayWidth(cell.text), 1);
    const bool last = index + 1 == cells.size();
    top += Repeat(glyphs.box_horizontal, width + 2);
    top += last ? glyphs.box_top_right : glyphs.box_top_join;
    bottom += Repeat(glyphs.box_horizontal, width + 2);
    bottom += last ? glyphs.box_bottom_right : glyphs.box_bottom_join;
    row.middle +=
        " " + style.Paint(cell.tone, PadRight(cell.text, width)) + " " + border;
    row.centers.push_back(column + 1 + (width - 1) / 2);
    column += width + 3;
  }
  row.top = style.Paint(Tone::kMuted, top);
  row.bottom = style.Paint(Tone::kMuted, bottom);
  return row;
}

}  // namespace terminal
