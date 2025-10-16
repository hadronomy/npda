#pragma once

#include <cstddef>
#include <optional>
#include <vector>

namespace turing {

template <typename State, typename TapeSym>
struct MultiTapeNode {
  State s{};
  std::vector<std::vector<TapeSym>> tapes{};  // multiple tapes
  std::vector<std::size_t> head_positions{};  // head position on each tape
  std::size_t parent = static_cast<std::size_t>(-1);
  std::optional<std::size_t> rule_idx{};
};

}  // namespace turing