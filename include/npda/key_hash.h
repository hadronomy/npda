#pragma once

#include <cstddef>
#include <functional>
#include <vector>

#include "key.h"

namespace npda {

template <typename State, typename StackSym>
struct KeyHash {
  std::size_t operator()(const struct Key<State, StackSym>& k) const {
    std::size_t h = std::hash<State>{}(k.s);
    h = combine(h, std::hash<std::size_t>{}(k.pos));
    h = combine(h, vec_hash(k.stack));
    return h;
  }

  static std::size_t combine(std::size_t a, std::size_t b) {
    // 64-bit mix (splitmix64-ish)
    std::size_t x = a ^ (b + 0x9e3779b97f4a7c15ULL + (a << 6) + (a >> 2));
    return x;
  }

  static std::size_t vec_hash(const std::vector<StackSym>& v) {
    std::size_t h = 0xcbf29ce484222325ULL;  // FNV offset
    for (const auto& e : v) {
      std::size_t eh = std::hash<StackSym>{}(e);
      h ^= eh;
      h *= 0x100000001b3ULL;  // FNV prime
    }
    return h;
  }
};

}  // namespace npda