#pragma once

#include <concepts>
#include <functional>

namespace turing {

// Concept for types that can be used as hashable keys (states, symbols, etc.)
template <typename T>
concept Hashable = std::equality_comparable<T> && requires(const T& t) {
  { std::hash<T>{}(t) } -> std::convertible_to<std::size_t>;
};

}  // namespace turing