#ifndef NPDA_TURING_TRANSITION_H_
#define NPDA_TURING_TRANSITION_H_

#include <optional>
#include <string_view>
#include <vector>

#include "automata/symbol.h"

namespace turing {

using automata::Symbol;

enum class HeadMove { kLeft, kRight, kStay };

/// Returns the letter that files use for a head move: `L`, `R`, or `S`.
[[nodiscard]] constexpr char ToChar(HeadMove move) noexcept {
  switch (move) {
    case HeadMove::kLeft:
      return 'L';
    case HeadMove::kRight:
      return 'R';
    case HeadMove::kStay:
      return 'S';
  }
  return 'S';
}

/// Returns the head move for a file letter, or nothing for other text.
[[nodiscard]] constexpr std::optional<HeadMove> ParseHeadMove(
    std::string_view text
) noexcept {
  if (text == "L") return HeadMove::kLeft;
  if (text == "R") return HeadMove::kRight;
  if (text == "S") return HeadMove::kStay;
  return std::nullopt;
}

/// What a transition does to one tape. In the independent mode, an action
/// either writes or moves: a write has `move == kStay`, and a move has
/// `write == read`.
struct TapeAction {
  Symbol read;
  Symbol write;
  HeadMove move;
};

/// `δ(from, reads) = (to, writes, moves)`, with one action for each tape.
struct Transition {
  Symbol from;
  std::vector<TapeAction> tapes;
  Symbol to;
};

}  // namespace turing

#endif  // NPDA_TURING_TRANSITION_H_
