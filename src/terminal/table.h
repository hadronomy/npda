#ifndef NPDA_SRC_TERMINAL_TABLE_H_
#define NPDA_SRC_TERMINAL_TABLE_H_

#include <ostream>
#include <variant>
#include <vector>

#include "terminal/style.h"

namespace terminal {

/// Rows of cells with aligned columns, and free lines between the rows.
///
/// Each column has the width of its widest cell. A row ends at its last cell
/// that is not empty. Thus, no line ends with spaces.
class Table {
 public:
  void AddRow(std::vector<Styled> cells);

  /// Adds a line that is not a part of the columns.
  void AddLine(Styled line);

  /// Writes all of the lines. Each line starts with two spaces.
  void Write(std::ostream& out) const;

 private:
  std::vector<std::variant<std::vector<Styled>, Styled>> lines_;
};

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_TABLE_H_
