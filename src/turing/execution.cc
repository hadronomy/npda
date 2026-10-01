#include "turing/execution.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "turing/machine.h"

namespace turing {

namespace {

Symbol ReadSymbol(const TapeState& tape, Symbol blank) {
  return tape.head_position < tape.cells.size() ? tape.cells[tape.head_position]
                                                : blank;
}

void WriteSymbol(TapeState& tape, Symbol symbol, Symbol blank) {
  if (tape.head_position >= tape.cells.size()) {
    tape.cells.resize(tape.head_position + 1, blank);
  }
  tape.cells[tape.head_position] = symbol;
}

void MoveHead(TapeState& tape, Direction direction, Symbol blank) {
  switch (direction) {
    case Direction::kRight:
      ++tape.head_position;
      return;
    case Direction::kStay:
      return;
    case Direction::kLeft:
      if (tape.head_position == 0) {
        tape.cells.push_front(blank);
      } else {
        --tape.head_position;
      }
      return;
  }
}

void ApplyTransition(TapeConfiguration& configuration,
                     const Transition& transition,
                     const Definition& definition) {
  const auto actions = std::views::zip(configuration.tapes, transition.tapes);
  if (definition.config.operation_mode == OperationMode::kSimultaneous) {
    for (auto&& [tape, action] : actions) {
      WriteSymbol(tape, action.write, definition.blank_symbol);
    }
    for (auto&& [tape, action] : actions) {
      MoveHead(tape, action.move, definition.blank_symbol);
    }
  } else {
    for (auto&& [tape, action] : actions) {
      WriteSymbol(tape, action.write, definition.blank_symbol);
      MoveHead(tape, action.move, definition.blank_symbol);
    }
  }
  configuration.state = transition.to_state;
}

TapeConfiguration InitialConfiguration(const Definition& definition,
                                       std::span<const Symbol> input) {
  TapeConfiguration configuration{
      .state = definition.start_state,
      .tapes = std::vector<TapeState>(definition.config.num_tapes),
      .predecessor = std::nullopt};
  configuration.tapes.front().cells.assign(input.begin(), input.end());
  return configuration;
}

}  // namespace

Machine::Execution::History::History(const TapeConfiguration& root)
    : nodes_{root} {}

void Machine::Execution::History::Append(
    const TapeConfiguration& configuration) {
  nodes_.push_back(configuration);
}

std::span<const TapeConfiguration> Machine::Execution::History::Nodes() const {
  return nodes_;
}

std::size_t Machine::Execution::History::LastIndex() const {
  return nodes_.size() - 1;
}

Machine::Execution::Execution(const Machine& machine,
                              std::span<const Symbol> input, RunOptions options)
    : machine_(machine),
      options_(std::move(options)),
      current_(InitialConfiguration(machine.definition_, input)) {
  if (options_.track_witness) witness_.emplace();
  if (options_.observer && options_.track_witness) history_.emplace(current_);
}

std::expected<RunResult, Error> Machine::Execution::Execute() {
  for (;;) {
    Observe(EventKind::kStep);
    if (std::ranges::contains(machine_.definition_.accepting_states,
                              current_.state)) {
      return Accept();
    }
    if (steps_ >= options_.max_steps) {
      return std::unexpected(Error{"max_steps reached"});
    }
    const auto transition_index = FindTransition();
    if (!transition_index) {
      Observe(EventKind::kNoTransition);
      return RunResult{false, steps_, std::nullopt,
                       {},    {},     machine_.definition_.config};
    }
    Advance(*transition_index);
  }
}

void Machine::Execution::Observe(EventKind kind) const {
  if (!options_.observer) return;
  const auto nodes = history_
                         ? history_->Nodes()
                         : std::span<const TapeConfiguration>(&current_, 1);
  const auto node_index = history_ ? history_->LastIndex() : 0;
  options_.observer(
      ExecutionEvent{kind, nodes, node_index, steps_, options_.track_witness});
}

std::optional<std::size_t> Machine::Execution::FindTransition() const {
  const auto rules = machine_.transitions_by_state_.find(current_.state);
  if (rules == machine_.transitions_by_state_.end()) return std::nullopt;
  const auto selected =
      std::ranges::find_if(rules->second, [&](std::size_t index) {
        return std::ranges::equal(
            machine_.definition_.transitions[index].tapes, current_.tapes,
            [&](const TapeTransition& action, const TapeState& tape) {
              return action.read ==
                     ReadSymbol(tape, machine_.definition_.blank_symbol);
            });
      });
  if (selected == rules->second.end()) return std::nullopt;
  return *selected;
}

void Machine::Execution::Advance(std::size_t transition_index) {
  ApplyTransition(current_, machine_.definition_.transitions[transition_index],
                  machine_.definition_);
  current_.predecessor = Predecessor{steps_, transition_index};
  ++steps_;
  if (witness_) witness_->push_back(transition_index);
  if (history_) history_->Append(current_);
}

RunResult Machine::Execution::Accept() {
  Observe(EventKind::kAccepted);
  RunResult result{.accepted = true,
                   .steps = steps_,
                   .witness = std::move(witness_),
                   .final_tapes = {},
                   .final_head_positions = {},
                   .config = machine_.definition_.config};
  result.final_tapes.reserve(current_.tapes.size());
  result.final_head_positions.reserve(current_.tapes.size());
  for (const auto& tape : current_.tapes) {
    auto symbols = tape.cells | std::views::transform([](Symbol symbol) {
                     return std::string(automata::SymbolName(symbol));
                   }) |
                   std::ranges::to<std::vector>();
    result.final_tapes.push_back(std::move(symbols));
    result.final_head_positions.push_back(tape.head_position);
  }
  return result;
}

}  // namespace turing
