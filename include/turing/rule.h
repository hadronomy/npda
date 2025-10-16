#pragma once

#include <vector>
#include "concepts.h"

namespace turing {

// Direction // tape head can move
enum class Direction { Left = 'L', Right = 'R', Stay = 'S' };

// Tape configuration options
enum class TapeDirection {
  Bidirectional,  // Infinite in both directions
  RightOnly       // Infinite only to the right
};

// Write/movement modes
enum class OperationMode {
  Simultaneous,  // Write and move happen together
  Independent    // Write first, then move (or vice versa)
};

template <Hashable State, Hashable TapeSym>
struct MultiTapeRule {
  State from{};
  std::vector<TapeSym> read{};  // symbols to read from each tape
  State to{};
  std::vector<TapeSym> write{};   // symbols to write to each tape
  std::vector<Direction> move{};  // movement for each tape head
};

// Rule is always multi; arity-1 = "single tape"
template <Hashable State, Hashable TapeSym>
using Rule = MultiTapeRule<State, TapeSym>;

// Helper to conveniently create an arity-1 rule (single tape)
template <Hashable State, Hashable TapeSym>
constexpr Rule<State, TapeSym> make_single_rule(
  const State& from,
  const TapeSym& read,
  const State& to,
  const TapeSym& write,
  Direction move
) {
  return Rule<State, TapeSym>{
    .from = from,
    .read = {read},
    .to = to,
    .write = {write},
    .move = {move},
  };
}

}  // namespace turing