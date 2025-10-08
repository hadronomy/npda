#pragma once

#include <optional>
#include <vector>
#include "concepts.h"

namespace npda {

template <Hashable State, Hashable Input, Hashable StackSym>
struct Rule {
  State from{};
  std::optional<Input> input{};         // std::nullopt = epsilon
  std::optional<StackSym> stack_top{};  // if set, must match top and pop
  State to{};
  std::vector<StackSym> push{};  // left-to-right; last becomes new top
};

}  // namespace npda