#pragma once

#include <concepts>
#include <functional>

namespace npda {

template <typename T>
concept Hashable = requires(T t) {
  { std::hash<T>{}(t) } -> std::convertible_to<std::size_t>;
};

}  // namespace npda