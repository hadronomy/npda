#ifndef NPDA_TURING_TAPE_H_
#define NPDA_TURING_TAPE_H_

#include <cstddef>
#include <deque>

#include "automata/symbol.h"

namespace turing {

using automata::Symbol;

/// A tape and its head. All of the cells outside `cells` hold the blank symbol.
struct Tape {
  std::deque<Symbol> cells;
  std::size_t head = 0;
};

/// Returns the symbol under the head.
[[nodiscard]] inline Symbol ReadHead(const Tape& tape, Symbol blank) {
  return tape.head < tape.cells.size() ? tape.cells[tape.head] : blank;
}

}  // namespace turing

#endif  // NPDA_TURING_TAPE_H_
