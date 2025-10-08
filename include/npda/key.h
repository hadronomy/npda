#pragma once

#include <vector>

namespace npda {

template <typename State, typename StackSym>
struct Key {
  State s{};
  std::size_t pos = 0;
  std::vector<StackSym> stack{};
  bool operator==(const Key& o) const { return s == o.s && pos == o.pos && stack == o.stack; }
};

}  // namespace npda