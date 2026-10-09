#ifndef NPDA_SRC_TERMINAL_BOX_H_
#define NPDA_SRC_TERMINAL_BOX_H_

#include <cstddef>
#include <span>
#include <string>
#include <vector>

#include "terminal/style.h"

namespace terminal {

struct BoxCell {
  std::string text;
  Tone tone = Tone::kPlain;
};

/// A row of cells in a box with a height of three lines:
///
/// ```text
/// ┌───┬───┐
/// │ A │ S │
/// └───┴───┘
/// ```
struct BoxedRow {
  std::string top;
  std::string middle;
  std::string bottom;
  /// The column at the center of each cell. Columns start at the left border.
  std::vector<std::size_t> centers;
};

/// Draws `cells` in one row. Each cell has the width of its text.
[[nodiscard]] BoxedRow DrawBoxedRow(
    std::span<const BoxCell> cells,
    const Style& style
);

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_BOX_H_
