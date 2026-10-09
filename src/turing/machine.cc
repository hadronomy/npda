#include "turing/machine.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <utility>
#include <vector>

#include "turing/config.h"
#include "turing/definition.h"
#include "turing/run.h"
#include "turing/tape.h"
#include "turing/transition.h"
#include "turing/transition_text.h"

namespace turing {

namespace {

void Write(Tape& tape, Symbol symbol, Symbol blank) {
  if (tape.head >= tape.cells.size()) tape.cells.resize(tape.head + 1, blank);
  tape.cells[tape.head] = symbol;
}

void Move(Tape& tape, HeadMove move, Symbol blank) {
  switch (move) {
    case HeadMove::kRight:
      ++tape.head;
      return;
    case HeadMove::kStay:
      return;
    case HeadMove::kLeft:
      if (tape.head == 0) {
        tape.cells.push_front(blank);
      } else {
        --tape.head;
      }
      return;
  }
}

void Apply(
    const Transition& transition,
    Symbol blank,
    Configuration& configuration
) {
  for (auto&& [tape, action] :
       std::views::zip(configuration.tapes, transition.tapes)) {
    Write(tape, action.write, blank);
    Move(tape, action.move, blank);
  }
  configuration.state = transition.to;
}

std::expected<void, Error> CheckRule(
    const Transition& rule,
    std::size_t index,
    const Config& config
) {
  if (rule.tapes.size() != config.num_tapes) {
    return std::unexpected(
        Error{std::format(
            "rule #{} has {} tape actions, but the machine has {} tapes",
            index,
            rule.tapes.size(),
            config.num_tapes
        )}
    );
  }
  for (const auto& action : rule.tapes) {
    if (!config.allow_stay && action.move == HeadMove::kStay) {
      return std::unexpected(
          Error{std::format(
              "rule #{} uses S, but the config sets allow_stay = false",
              index
          )}
      );
    }
    if (config.tape_direction == TapeDirection::kRightOnly &&
        action.move == HeadMove::kLeft) {
      return std::unexpected(
          Error{std::format(
              "rule #{} uses L, but the config sets tape_direction = "
              "right-only",
              index
          )}
      );
    }
  }
  return {};
}

}  // namespace

Machine::Machine(Definition definition) : definition_(std::move(definition)) {
  for (std::size_t index = 0; index < definition_.transitions.size(); ++index) {
    transitions_by_state_[definition_.transitions[index].from].push_back(index);
  }
}

std::expected<Machine, Error> Machine::Create(Definition definition) {
  if (definition.config.num_tapes == 0) {
    return std::unexpected(Error{"num_tapes must be positive"});
  }
  Machine machine(std::move(definition));
  const auto& rules = machine.definition_.transitions;
  for (std::size_t index = 0; index < rules.size(); ++index) {
    const auto& rule = rules[index];
    if (auto checked = CheckRule(rule, index, machine.definition_.config);
        !checked) {
      return std::unexpected(checked.error());
    }
    for (const auto earlier : machine.transitions_by_state_.at(rule.from)) {
      if (earlier >= index) break;
      if (std::ranges::equal(
              rules[earlier].tapes,
              rule.tapes,
              {},
              &TapeAction::read,
              &TapeAction::read
          )) {
        return std::unexpected(
            Error{std::format(
                "rules #{} and #{} both read ({}) in state '{}'",
                earlier,
                index,
                JoinReads(rule.tapes),
                rule.from
            )}
        );
      }
    }
  }
  return machine;
}

std::optional<std::size_t> Machine::FindTransition(
    const Configuration& configuration
) const {
  const auto rules = transitions_by_state_.find(configuration.state);
  if (rules == transitions_by_state_.end()) return std::nullopt;
  const auto reads = [&](std::size_t index) {
    return std::ranges::equal(
        definition_.transitions[index].tapes,
        configuration.tapes,
        [&](const TapeAction& action, const Tape& tape) {
          return action.read == ReadHead(tape, definition_.blank);
        }
    );
  };
  const auto match = std::ranges::find_if(rules->second, reads);
  if (match == rules->second.end()) return std::nullopt;
  return *match;
}

std::expected<RunResult, LimitReached> Machine::Run(
    std::span<const Symbol> input,
    const RunOptions& options
) const {
  Configuration current{
      .state = definition_.start_state,
      .tapes = std::vector<Tape>(definition_.config.num_tapes)
  };
  current.tapes.front().cells.assign(input.begin(), input.end());
  std::optional<std::size_t> transition;
  for (std::size_t steps = 0;; ++steps) {
    if (options.observer) {
      options.observer(
          Step{
              .number = steps,
              .configuration = current,
              .transition = transition
          }
      );
    }
    const bool accepted =
        std::ranges::contains(definition_.accepting_states, current.state);
    transition = accepted ? std::nullopt : FindTransition(current);
    if (!transition) {
      return RunResult{
          .accepted = accepted,
          .steps = steps,
          .halted = std::move(current)
      };
    }
    if (steps == options.max_steps) return std::unexpected(LimitReached{steps});
    Apply(definition_.transitions[*transition], definition_.blank, current);
  }
}

}  // namespace turing
