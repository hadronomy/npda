#ifndef NPDA_NPDA_MACHINE_H_
#define NPDA_NPDA_MACHINE_H_

#include <cstddef>
#include <expected>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "automata/symbol.h"

namespace npda {

using automata::Symbol;

enum class AcceptancePolicy { kFinalState, kEmptyStack, kBoth, kAny };

struct Error {
  std::string message;
};

struct Transition {
  Symbol from_state;
  std::optional<Symbol> input;
  std::optional<Symbol> stack_top;
  Symbol to_state;
  // The first symbol becomes the new stack top.
  std::vector<Symbol> push;
};

struct Definition {
  Symbol start_state;
  Symbol stack_bottom;
  AcceptancePolicy acceptance = AcceptancePolicy::kFinalState;
  std::vector<Symbol> accepting_states;
  std::vector<Transition> transitions;
};

struct Predecessor {
  std::size_t node_index;
  std::size_t transition_index;
};

struct SearchNode {
  Symbol state;
  std::size_t input_position = 0;
  // The last element is the stack top.
  std::vector<Symbol> stack;
  // A root has no predecessor. Every other node has a complete search edge.
  std::optional<Predecessor> predecessor;
};

enum class EventKind { kStep, kDeadEnd, kAccepted, kRejected };

// Views remain valid only during the observer call. Observers cannot alter a
// run.
struct ExecutionEvent {
  EventKind kind;
  std::span<const SearchNode> nodes;
  std::size_t node_index;
  std::span<const Symbol> input;
  std::span<const std::size_t> explored_nodes;
  std::span<const std::size_t> deadend_nodes;
  std::size_t expansions;
  bool track_witness;
  bool complete;
};

using Observer = std::function<void(const ExecutionEvent&)>;

enum class SearchOrder { kBreadthFirst, kDepthFirst };

struct RunOptions {
  // Breadth-first search returns a witness with the fewest transitions.
  SearchOrder search_order = SearchOrder::kBreadthFirst;
  // Counts applicable transitions, including revisits of a configuration.
  std::size_t max_expansions = 1'000'000;
  bool track_witness = true;
  Observer observer = {};
};

struct RunResult {
  bool accepted = false;
  std::size_t expansions = 0;
  // Transition indices in execution order; absent when disabled or rejected.
  std::optional<std::vector<std::size_t>> witness;
  std::size_t stack_depth = 0;
};

[[nodiscard]] std::string_view AcceptanceName(AcceptancePolicy acceptance);

class Machine {
 public:
  // A successful construction validates the definition and builds its indices.
  [[nodiscard]] static std::expected<Machine, Error> Create(
      Definition definition);

  [[nodiscard]] const Definition& definition() const { return definition_; }

  // The machine is immutable. Each run owns its search state.
  // Options are copied at the start of a run. Input is borrowed for the call.
  [[nodiscard]] std::expected<RunResult, Error> Run(
      std::span<const Symbol> input, const RunOptions& options = {}) const;

 private:
  class Search;

  explicit Machine(Definition definition);
  Definition definition_;
  std::unordered_map<Symbol, std::unordered_map<std::optional<Symbol>,
                                                std::vector<std::size_t>>>
      transitions_by_state_;
};

}  // namespace npda

#endif  // NPDA_NPDA_MACHINE_H_
