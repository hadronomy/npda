#pragma once

#include <cstddef>
#include <optional>
#include <vector>

namespace npda {

template <typename State, typename StackSym>
struct Node {
  State s{};
  std::size_t pos = 0;  // index into input
  std::vector<StackSym> stack{};
  std::size_t parent = static_cast<std::size_t>(-1);
  std::optional<std::size_t> rule_idx{};
};

}  // namespace npda