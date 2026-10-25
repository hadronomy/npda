#ifndef NPDA_TURING_TAPE_H_
#define NPDA_TURING_TAPE_H_

#include <cstddef>
#include <deque>

#include "automata/symbol.h"

namespace turing {

using automata::Symbol;

/// A tape and its head, with the blank symbol in all cells outside `cells`.
struct Tape {
  std::deque<Symbol> cells;
  std::size_t head = 0;
};

[[nodiscard]] inline Symbol ReadHead(const Tape& tape, Symbol blank) {
  return tape.head < tape.cells.size() ? tape.cells[tape.head] : blank;
}

}  // namespace turing

#endif  // NPDA_TURING_TAPE_H_
