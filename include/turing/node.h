#pragma once

#include <cstddef>
#include <optional>
#include <variant>
#include <vector>

namespace turing {

template <typename State, typename TapeSym>
struct SingleTapeNode {
  State s{};
  std::vector<TapeSym> tape{};  // tape content, extends infinitely with blank symbols
  std::size_t head_pos = 0;     // head position on tape
  std::size_t parent = static_cast<std::size_t>(-1);
  std::optional<std::size_t> rule_idx{};
};

template <typename State, typename TapeSym>
struct MultiTapeNode {
  State s{};
  std::vector<std::vector<TapeSym>> tapes{};  // multiple tapes
  std::vector<std::size_t> head_positions{};  // head position on each tape
  std::size_t parent = static_cast<std::size_t>(-1);
  std::optional<std::size_t> rule_idx{};
};

// Variant node type for single/multi-tape support
template <typename State, typename TapeSym>
struct Node {
  std::variant<SingleTapeNode<State, TapeSym>, MultiTapeNode<State, TapeSym>> data{};

  // Default constructor initializes to single tape node
  Node() = default;

  // Constructors for specific types
  explicit Node(const SingleTapeNode<State, TapeSym>& node) : data(node) {}
  explicit Node(const MultiTapeNode<State, TapeSym>& node) : data(node) {}

  // Convenience methods to check type
  bool is_multi_tape() const { return std::holds_alternative<MultiTapeNode<State, TapeSym>>(data); }

  bool is_single_tape() const {
    return std::holds_alternative<SingleTapeNode<State, TapeSym>>(data);
  }

  // Accessors
  SingleTapeNode<State, TapeSym>& as_single() {
    return std::get<SingleTapeNode<State, TapeSym>>(data);
  }

  const SingleTapeNode<State, TapeSym>& as_single() const {
    return std::get<SingleTapeNode<State, TapeSym>>(data);
  }

  MultiTapeNode<State, TapeSym>& as_multi() {
    return std::get<MultiTapeNode<State, TapeSym>>(data);
  }

  const MultiTapeNode<State, TapeSym>& as_multi() const {
    return std::get<MultiTapeNode<State, TapeSym>>(data);
  }

  // Safe accessors that return nullptr if wrong type
  SingleTapeNode<State, TapeSym>* as_single_if() {
    return std::get_if<SingleTapeNode<State, TapeSym>>(&data);
  }

  const SingleTapeNode<State, TapeSym>* as_single_if() const {
    return std::get_if<SingleTapeNode<State, TapeSym>>(&data);
  }

  MultiTapeNode<State, TapeSym>* as_multi_if() {
    return std::get_if<MultiTapeNode<State, TapeSym>>(&data);
  }

  const MultiTapeNode<State, TapeSym>* as_multi_if() const {
    return std::get_if<MultiTapeNode<State, TapeSym>>(&data);
  }
};

}  // namespace turing