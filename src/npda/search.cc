#include "npda/search.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "npda/machine.h"

namespace npda {

namespace {

std::size_t CombineHash(std::size_t first, std::size_t second) {
  return first ^ (second + 0x9e3779b97f4a7c15ULL + (first << 6) + (first >> 2));
}

std::size_t HashStack(const std::vector<Symbol>& stack) {
  std::size_t hash = 0xcbf29ce484222325ULL;
  for (const auto symbol : stack) {
    hash ^= std::hash<Symbol>{}(symbol);
    hash *= 0x100000001b3ULL;
  }
  return hash;
}

bool StackMatches(const std::vector<Symbol>& stack,
                  const std::optional<Symbol>& expected_top) {
  return !expected_top || (!stack.empty() && stack.back() == *expected_top);
}

void ApplyStack(std::vector<Symbol>& stack, const Transition& transition) {
  if (transition.stack_top) stack.pop_back();
  // The first push symbol becomes the top of the stack.
  stack.insert(stack.end(), transition.push.rbegin(), transition.push.rend());
}

bool IsAccepting(const Definition& definition, const SearchNode& node,
                 std::span<const Symbol> input) {
  if (node.input_position != input.size()) return false;
  const bool final_state =
      std::ranges::contains(definition.accepting_states, node.state);
  const bool empty_stack = node.stack.empty();
  switch (definition.acceptance) {
    case AcceptancePolicy::kFinalState:
      return final_state;
    case AcceptancePolicy::kEmptyStack:
      return empty_stack;
    case AcceptancePolicy::kBoth:
      return final_state && empty_stack;
    case AcceptancePolicy::kAny:
      return final_state || empty_stack;
  }
  return false;
}

}  // namespace

std::size_t Machine::Search::ConfigurationHash::operator()(
    const ConfigurationKey& configuration) const {
  auto hash = std::hash<Symbol>{}(configuration.state);
  hash =
      CombineHash(hash, std::hash<std::size_t>{}(configuration.input_position));
  return CombineHash(hash, HashStack(configuration.stack));
}

Machine::Search::Search(const Machine& machine, std::span<const Symbol> input,
                        RunOptions options)
    : machine_(machine), input_(input), options_(std::move(options)) {
  // A large execution limit must not force a large allocation at startup.
  static constexpr std::size_t kMaxInitialCapacity = 65536;
  const auto capacity = std::min(options_.max_expansions, kMaxInitialCapacity);
  nodes_.reserve(capacity);
  visited_.reserve(capacity);
  AddNode(SearchNode{.state = machine_.definition_.start_state,
                     .stack = {machine_.definition_.stack_bottom},
                     .predecessor = std::nullopt});
}

std::expected<RunResult, Error> Machine::Search::Execute() {
  while (const auto node_index = TakeNextNode()) {
    ObserveVisit(*node_index);
    if (IsAccepting(machine_.definition_, nodes_[*node_index], input_)) {
      return Accept(*node_index);
    }
    if (auto expansion = ExpandNode(*node_index); !expansion) {
      Observe(EventKind::kRejected, furthest_node_index_);
      return std::unexpected(expansion.error());
    }
  }
  Observe(EventKind::kRejected, furthest_node_index_, true);
  return RunResult{false, expansions_, std::nullopt};
}

std::optional<std::size_t> Machine::Search::TakeNextNode() {
  if (frontier_.empty()) return std::nullopt;
  if (options_.search_order == SearchOrder::kBreadthFirst) {
    const auto index = frontier_.front();
    frontier_.pop_front();
    return index;
  }
  const auto index = frontier_.back();
  frontier_.pop_back();
  return index;
}

void Machine::Search::ObserveVisit(std::size_t node_index) {
  if (!options_.observer) return;
  explored_nodes_.push_back(node_index);
  Observe(EventKind::kStep, node_index);
}

void Machine::Search::Observe(EventKind kind, std::size_t node_index,
                              bool complete) const {
  if (!options_.observer) return;
  options_.observer(ExecutionEvent{kind, nodes_, node_index, input_,
                                   explored_nodes_, deadend_nodes_, expansions_,
                                   options_.track_witness, complete});
}

RunResult Machine::Search::Accept(std::size_t node_index) const {
  Observe(EventKind::kAccepted, node_index);
  std::optional<std::vector<std::size_t>> witness;
  if (options_.track_witness) {
    witness.emplace();
    auto predecessor = nodes_[node_index].predecessor;
    while (predecessor) {
      witness->push_back(predecessor->transition_index);
      predecessor = nodes_[predecessor->node_index].predecessor;
    }
    std::ranges::reverse(*witness);
  }
  return RunResult{true, expansions_, std::move(witness),
                   nodes_[node_index].stack.size()};
}

std::span<const std::size_t> Machine::Search::TransitionsFrom(
    Symbol state, std::optional<Symbol> input) const {
  const auto state_rules = machine_.transitions_by_state_.find(state);
  if (state_rules == machine_.transitions_by_state_.end()) return {};
  const auto input_rules = state_rules->second.find(input);
  if (input_rules == state_rules->second.end()) return {};
  return input_rules->second;
}

std::expected<void, Error> Machine::Search::ExpandNode(std::size_t node_index) {
  if (nodes_[node_index].input_position >
      nodes_[furthest_node_index_].input_position) {
    furthest_node_index_ = node_index;
  }
  const auto state = nodes_[node_index].state;
  const auto input_position = nodes_[node_index].input_position;
  const auto epsilon_rules = TransitionsFrom(state, std::nullopt);
  const auto consuming_rules =
      input_position < input_.size()
          ? TransitionsFrom(state, input_[input_position])
          : std::span<const std::size_t>{};
  const auto previous_size = nodes_.size();
  // Preserve rule order: epsilon transitions precede consuming transitions.
  for (const auto candidates : {epsilon_rules, consuming_rules}) {
    for (const auto transition_index : candidates) {
      if (auto expansion = FollowTransition(node_index, transition_index);
          !expansion) {
        return expansion;
      }
    }
  }
  if (options_.search_order == SearchOrder::kDepthFirst &&
      nodes_.size() == previous_size && options_.observer) {
    deadend_nodes_.push_back(node_index);
    Observe(EventKind::kDeadEnd, node_index);
  }
  return {};
}

std::expected<void, Error> Machine::Search::FollowTransition(
    std::size_t node_index, std::size_t transition_index) {
  const auto& transition = machine_.definition_.transitions[transition_index];
  const auto& parent = nodes_[node_index];
  if (!StackMatches(parent.stack, transition.stack_top)) return {};
  if (expansions_ >= options_.max_expansions) {
    return std::unexpected(Error{"max_expansions reached"});
  }
  SearchNode next{
      .state = transition.to_state,
      .input_position = parent.input_position + transition.input.has_value(),
      .stack = parent.stack,
      .predecessor = Predecessor{node_index, transition_index}};
  ApplyStack(next.stack, transition);
  ++expansions_;
  // AddNode can reallocate nodes_; the parent borrow ends before that call.
  AddNode(std::move(next));
  return {};
}

void Machine::Search::AddNode(SearchNode node) {
  ConfigurationKey configuration{node.state, node.input_position, node.stack};
  if (!visited_.insert(std::move(configuration)).second) return;
  nodes_.push_back(std::move(node));
  frontier_.push_back(nodes_.size() - 1);
}

}  // namespace npda
