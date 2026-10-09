#include "npda/machine.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <functional>
#include <iterator>
#include <optional>
#include <ranges>
#include <span>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "automata/symbol.h"
#include "npda/definition.h"
#include "npda/run.h"
#include "npda/stack_view.h"
#include "npda/transition.h"

namespace npda {

namespace {

std::size_t Combine(std::size_t seed, std::size_t value) {
  return seed ^ (value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2));
}

/// All the stacks of a run, one cell for each different stack.
///
/// Interning gives equal stacks equal indices. Thus, a comparison or a hash of
/// a stack uses one integer, and a push shares all of the cells below it.
class StackTable {
 public:
  [[nodiscard]] StackView View(std::size_t stack) const {
    return {cells_, stack};
  }

  [[nodiscard]] Symbol Top(std::size_t stack) const {
    return cells_[stack].symbol;
  }

  [[nodiscard]] std::size_t Pop(std::size_t stack) const {
    return cells_[stack].below;
  }

  std::size_t Push(std::size_t stack, Symbol symbol) {
    const auto [entry, added] =
        index_.try_emplace({symbol, stack}, cells_.size());
    if (added) cells_.push_back({.symbol = symbol, .below = stack});
    return entry->second;
  }

 private:
  struct Key {
    Symbol symbol;
    std::size_t below;

    bool operator==(const Key&) const = default;
  };

  struct KeyHash {
    std::size_t operator()(const Key& key) const {
      return Combine(std::hash<Symbol>{}(key.symbol), key.below);
    }
  };

  std::vector<StackView::Cell> cells_;
  std::unordered_map<Key, std::size_t, KeyHash> index_;
};

/// A configuration with its stack as an index into a `StackTable`.
struct Node {
  Symbol state;
  std::size_t consumed;
  std::size_t stack;

  bool operator==(const Node&) const = default;
};

struct NodeHash {
  std::size_t operator()(const Node& node) const {
    const auto hash = Combine(std::hash<Symbol>{}(node.state), node.consumed);
    return Combine(hash, node.stack);
  }
};

Node Apply(const Transition& transition, const Node& from, StackTable& stacks) {
  auto stack = stacks.Pop(from.stack);
  for (const auto symbol : transition.push | std::views::reverse) {
    stack = stacks.Push(stack, symbol);
  }
  return {
      .state = transition.to,
      .consumed = from.consumed + (transition.input ? 1 : 0),
      .stack = stack,
  };
}

std::vector<std::size_t> PathTo(
    std::span<const std::optional<Origin>> origins,
    std::size_t id
) {
  std::vector<std::size_t> path;
  for (auto origin = origins[id]; origin; origin = origins[origin->parent]) {
    path.push_back(origin->transition);
  }
  std::ranges::reverse(path);
  return path;
}

}  // namespace

std::size_t Machine::MoveKeyHash::operator()(const MoveKey& key) const {
  const auto hash = Combine(
      std::hash<Symbol>{}(key.state),
      std::hash<std::optional<Symbol>>{}(key.input)
  );
  return Combine(hash, std::hash<Symbol>{}(key.top));
}

Machine::Machine(Definition definition) : definition_(std::move(definition)) {
  for (std::size_t index = 0; index < definition_.transitions.size(); ++index) {
    const auto& transition = definition_.transitions[index];
    moves_[{transition.from, transition.input, transition.top}].push_back(
        index
    );
  }
}

std::span<const std::size_t> Machine::MovesFor(const MoveKey& key) const {
  const auto it = moves_.find(key);
  if (it == moves_.end()) return {};
  return it->second;
}

std::expected<RunResult, LimitReached> Machine::Run(
    std::span<const Symbol> input,
    const RunOptions& options
) const {
  const auto* final_states =
      std::get_if<AcceptByFinalState>(&definition_.acceptance);
  const auto accepts = [&](const Node& node) {
    if (node.consumed != input.size()) return false;
    if (final_states == nullptr) return node.stack == StackView::kEmpty;
    return std::ranges::contains(final_states->states, node.state);
  };
  const auto applicable = [&](const Node& node, StackTable& stacks) {
    std::vector<std::size_t> transitions;
    if (node.stack == StackView::kEmpty) return transitions;
    const auto top = stacks.Top(node.stack);
    std::ranges::copy(
        MovesFor({node.state, std::nullopt, top}),
        std::back_inserter(transitions)
    );
    if (node.consumed < input.size()) {
      std::ranges::copy(
          MovesFor({node.state, input[node.consumed], top}),
          std::back_inserter(transitions)
      );
    }
    return transitions;
  };

  StackTable stacks;
  const Node start{
      .state = definition_.start_state,
      .consumed = 0,
      .stack = stacks.Push(StackView::kEmpty, definition_.initial_stack_symbol),
  };
  std::vector<Node> discovered{start};
  std::vector<std::optional<Origin>> origins{std::nullopt};
  std::unordered_map<Node, std::size_t, NodeHash> ids{{start, 0}};
  std::size_t expansions = 0;
  std::vector<Move> moves;
  for (std::size_t id = 0; id < discovered.size(); ++id) {
    const Node current = discovered[id];
    const bool accepting = accepts(current);
    moves.clear();
    const auto observe = [&] {
      if (!options.observer) return;
      options.observer(
          Visit{
              .id = id,
              .configuration =
                  {
                      .state = current.state,
                      .consumed = current.consumed,
                      .stack = stacks.View(current.stack),
                  },
              .origin = origins[id],
              .moves = moves,
              .accepting = accepting,
          }
      );
    };
    if (accepting) {
      observe();
      return RunResult{
          .accepting_path = PathTo(origins, id),
          .expansions = expansions,
      };
    }
    for (const auto transition : applicable(current, stacks)) {
      if (expansions == options.max_expansions) {
        observe();
        return std::unexpected(LimitReached{expansions});
      }
      ++expansions;
      const Node next =
          Apply(definition_.transitions[transition], current, stacks);
      const auto [entry, added] = ids.try_emplace(next, discovered.size());
      if (added) {
        discovered.push_back(next);
        origins.push_back(Origin{.parent = id, .transition = transition});
      }
      moves.push_back({
          .transition = transition,
          .target = entry->second,
          .repeated = !added,
      });
    }
    observe();
  }
  return RunResult{.accepting_path = std::nullopt, .expansions = expansions};
}

}  // namespace npda
