#include "npda/machine.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <vector>

#include "npda/search.h"

namespace npda {

[[nodiscard]] std::string_view AcceptanceName(AcceptancePolicy p) {
  switch (p) {
    case AcceptancePolicy::kFinalState:
      return "final-state";
    case AcceptancePolicy::kEmptyStack:
      return "empty-stack";
    case AcceptancePolicy::kBoth:
      return "both";
    case AcceptancePolicy::kAny:
      return "any";
  }
  return "final-state";
}

Machine::Machine(Definition definition) : definition_(std::move(definition)) {
  for (std::size_t index = 0; index < definition_.transitions.size(); ++index) {
    const auto& transition = definition_.transitions[index];
    transitions_by_state_[transition.from_state][transition.input].push_back(
        index);
  }
}

std::expected<Machine, Error> Machine::Create(Definition definition) {
  if (!definition.start_state.valid()) {
    return std::unexpected(Error{"start state not set"});
  }
  if (definition.acceptance != AcceptancePolicy::kFinalState &&
      definition.acceptance != AcceptancePolicy::kEmptyStack &&
      definition.acceptance != AcceptancePolicy::kBoth &&
      definition.acceptance != AcceptancePolicy::kAny) {
    return std::unexpected(Error{"invalid acceptance policy"});
  }
  if (!definition.stack_bottom.valid()) {
    return std::unexpected(Error{"stack bottom not set"});
  }
  if (std::ranges::any_of(definition.accepting_states,
                          [](Symbol symbol) { return !symbol.valid(); })) {
    return std::unexpected(Error{"invalid accepting state"});
  }
  for (const auto& transition : definition.transitions) {
    if (!transition.from_state.valid() || !transition.to_state.valid() ||
        (transition.input && !transition.input->valid()) ||
        (transition.stack_top && !transition.stack_top->valid()) ||
        std::ranges::any_of(transition.push,
                            [](Symbol symbol) { return !symbol.valid(); })) {
      return std::unexpected(Error{"invalid transition symbol"});
    }
  }
  return Machine(std::move(definition));
}

std::expected<RunResult, Error> Machine::Run(std::span<const Symbol> input,
                                             const RunOptions& options) const {
  if (std::ranges::any_of(input,
                          [](Symbol symbol) { return !symbol.valid(); })) {
    return std::unexpected(Error{"invalid input symbol"});
  }
  if (options.search_order != SearchOrder::kBreadthFirst &&
      options.search_order != SearchOrder::kDepthFirst) {
    return std::unexpected(Error{"invalid search order"});
  }
  return Search(*this, input, options).Execute();
}

}  // namespace npda
