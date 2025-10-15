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
struct SingleTapeRule {
  State from{};
  TapeSym read{};  // symbol to read from tape
  State to{};
  TapeSym write{};                    // symbol to write to tape
  Direction move = Direction::Right;  // head movement direction
};

template <Hashable State, Hashable TapeSym>
struct MultiTapeRule {
  State from{};
  std::vector<TapeSym> read{};  // symbols to read from each tape
  State to{};
  std::vector<TapeSym> write{};   // symbols to write to each tape
  std::vector<Direction> move{};  // movement for each tape head
};

// Variant rule type for single/multi-tape support
template <Hashable State, Hashable TapeSym>
struct Rule {
  bool is_multi_tape = false;
  SingleTapeRule<State, TapeSym> single{};
  MultiTapeRule<State, TapeSym> multi{};
};

}  // namespace turing