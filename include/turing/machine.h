#ifndef NPDA_TURING_MACHINE_H_
#define NPDA_TURING_MACHINE_H_

#include <cstddef>
#include <deque>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "automata/symbol.h"

namespace turing {

using automata::Symbol;

enum class Direction { kLeft = 'L', kRight = 'R', kStay = 'S' };

enum class TapeDirection {
  kBidirectional,  // Infinite in both directions
  kRightOnly       // Infinite only to the right
};

enum class OperationMode {
  kSimultaneous,  // Write and move happen together
  kIndependent    // Write first, then move (or vice versa)
};

// One tape of a rule: what to read, write, and where to move.
// Bundled so arity mismatch across sides is unrepresentable.
struct TapeTransition {
  automata::Symbol read{};
  automata::Symbol write{};
  Direction move{};
};

struct Transition {
  automata::Symbol from_state{};
  std::vector<TapeTransition> tapes{};  // one entry per tape
  automata::Symbol to_state{};
};

// Write direction as its file character.
[[nodiscard]] constexpr char ToChar(Direction d) noexcept {
  switch (d) {
    case Direction::kLeft:
      return 'L';
    case Direction::kRight:
      return 'R';
    case Direction::kStay:
      return 'S';
  }
  return 'S';  // unreachable; all enumerators covered above
}

// Read direction from its file character. Empty for anything else.
[[nodiscard]] inline std::optional<Direction> FromChar(
    std::string_view s) noexcept {
  if (s == "L") return Direction::kLeft;
  if (s == "R") return Direction::kRight;
  if (s == "S") return Direction::kStay;
  return std::nullopt;
}

struct MachineConfig {
  std::size_t num_tapes = 1;
  TapeDirection tape_direction = TapeDirection::kBidirectional;
  OperationMode operation_mode = OperationMode::kIndependent;
  bool allow_stay = true;
};

struct Error {
  std::string message;
};

struct Definition {
  MachineConfig config;
  Symbol start_state;
  Symbol blank_symbol;
  std::vector<Symbol> accepting_states;
  std::vector<Transition> transitions;
};

struct TapeState {
  std::deque<Symbol> cells;
  // Positions beyond stored cells read as blank symbols.
  std::size_t head_position = 0;
};

struct Predecessor {
  std::size_t node_index;
  std::size_t transition_index;
};

struct TapeConfiguration {
  Symbol state;
  std::vector<TapeState> tapes;
  std::optional<Predecessor> predecessor;
};

enum class EventKind { kStep, kAccepted, kNoTransition };

// Views remain valid only during the observer call.
struct ExecutionEvent {
  EventKind kind;
  std::span<const TapeConfiguration> nodes;
  std::size_t node_index;
  std::size_t steps;
  bool track_witness;
};

using Observer = std::function<void(const ExecutionEvent&)>;

struct RunOptions {
  std::size_t max_steps = 1'000'000;
  bool track_witness = true;
  Observer observer = {};
};

struct RunResult {
  bool accepted = false;
  std::size_t steps = 0;
  // Transition indices in execution order; absent when disabled or rejected.
  std::optional<std::vector<std::size_t>> witness;
  std::vector<std::vector<std::string>> final_tapes;
  std::vector<std::size_t> final_head_positions;
  MachineConfig config;
};

class Machine {
 public:
  // Validates symbols, tape arity, movement policy, and deterministic rules.
  [[nodiscard]] static std::expected<Machine, Error> Create(
      Definition definition);

  [[nodiscard]] const Definition& definition() const { return definition_; }

  // Each run owns its tape state. The first tape receives the input.
  // Options are copied at the start of a run. Input is borrowed for the call.
  [[nodiscard]] std::expected<RunResult, Error> Run(
      std::span<const Symbol> input, const RunOptions& options = {}) const;

 private:
  class Execution;

  explicit Machine(Definition definition);
  Definition definition_;
  std::unordered_map<Symbol, std::vector<std::size_t>> transitions_by_state_;
};

}  // namespace turing

#endif  // NPDA_TURING_MACHINE_H_
