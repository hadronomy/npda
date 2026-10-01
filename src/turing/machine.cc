#include "turing/machine.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <span>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "turing/execution.h"
#include "turing/transition_text.h"

namespace turing {

Machine::Machine(Definition definition) : definition_(std::move(definition)) {
  for (std::size_t i = 0; i < definition_.transitions.size(); ++i) {
    transitions_by_state_[definition_.transitions[i].from_state].push_back(i);
  }
}

std::expected<Machine, Error> Machine::Create(Definition definition) {
  if (definition.config.num_tapes == 0)
    return std::unexpected(Error{"num_tapes must be positive"});
  if (definition.config.tape_direction != TapeDirection::kBidirectional &&
      definition.config.tape_direction != TapeDirection::kRightOnly)
    return std::unexpected(Error{"invalid tape direction"});
  if (definition.config.operation_mode != OperationMode::kIndependent &&
      definition.config.operation_mode != OperationMode::kSimultaneous)
    return std::unexpected(Error{"invalid operation mode"});
  if (!definition.start_state.valid())
    return std::unexpected(Error{"start state not set"});
  if (!definition.blank_symbol.valid())
    return std::unexpected(Error{"blank symbol not set"});
  if (std::ranges::any_of(definition.accepting_states,
                          [](Symbol symbol) { return !symbol.valid(); })) {
    return std::unexpected(Error{"invalid accepting state"});
  }
  Machine machine(std::move(definition));
  for (std::size_t i = 0; i < machine.definition_.transitions.size(); ++i) {
    const auto& transition = machine.definition_.transitions[i];
    const auto& config = machine.definition_.config;
    if (transition.tapes.size() != config.num_tapes) {
      return std::unexpected(Error{std::format(
          "rule #{} arity mismatch: expected {} entries in read/write/move", i,
          config.num_tapes)});
    }
    if (!transition.from_state.valid() || !transition.to_state.valid()) {
      return std::unexpected(Error{"invalid transition state"});
    }
    for (const auto& tape : transition.tapes) {
      if (!tape.read.valid() || !tape.write.valid())
        return std::unexpected(Error{"invalid tape symbol"});
      if (tape.move != Direction::kLeft && tape.move != Direction::kRight &&
          tape.move != Direction::kStay) {
        return std::unexpected(Error{"invalid move direction"});
      }
      if (!config.allow_stay && tape.move == Direction::kStay) {
        return std::unexpected(
            Error{std::format("rule #{} uses Stay while allow_stay=false", i)});
      }
      if (config.tape_direction == TapeDirection::kRightOnly &&
          tape.move == Direction::kLeft) {
        return std::unexpected(Error{std::format(
            "rule #{} uses Left while TapeDirection is Right-only", i)});
      }
    }
    for (const auto index :
         machine.transitions_by_state_.at(transition.from_state)) {
      if (index >= i) break;
      if (std::ranges::equal(machine.definition_.transitions[index].tapes,
                             transition.tapes, {}, &TapeTransition::read,
                             &TapeTransition::read)) {
        return std::unexpected(Error{std::format(
            "duplicate multi-tape transition for state '{}' and symbols ({})",
            transition.from_state, JoinReads(transition.tapes))});
      }
    }
  }
  return machine;
}

std::expected<RunResult, Error> Machine::Run(std::span<const Symbol> input,
                                             const RunOptions& options) const {
  if (std::ranges::any_of(input,
                          [](Symbol symbol) { return !symbol.valid(); })) {
    return std::unexpected(Error{"invalid input symbol"});
  }
  return Execution(*this, input, options).Execute();
}

}  // namespace turing
