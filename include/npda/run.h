#ifndef NPDA_NPDA_RUN_H_
#define NPDA_NPDA_RUN_H_

#include <cstddef>
#include <functional>
#include <optional>
#include <span>
#include <vector>

#include "automata/symbol.h"
#include "npda/stack_view.h"

namespace npda {

/// An instantaneous description `(q, w, γ)`: the state, the input `w` that
/// remains, and the stack.
struct Configuration {
  Symbol state;
  /// The number of input symbols that the automaton read. The input that
  /// remains is `w`.
  std::size_t consumed;
  StackView stack;
};

/// The parent configuration and the transition that gave a configuration for
/// the first time.
struct Origin {
  std::size_t parent;
  std::size_t transition;
};

/// A transition that applies to a configuration, and the configuration that the
/// transition gives.
struct Move {
  std::size_t transition;
  /// The visit id of the configuration that the move gives.
  std::size_t target;
  /// True if the search found `target` before, so it does not explore that
  /// configuration again.
  bool repeated;
};

/// One step of the search: a configuration and the moves that apply to it.
///
/// Visit ids start at zero, with each parent visited before its children.
/// Because the search stops at the first accepting configuration, a move can
/// point to a target that the search never visits. Both the stack and `moves`
/// remain valid only during the observer call.
struct Visit {
  std::size_t id;
  Configuration configuration;
  /// Empty for the start configuration.
  std::optional<Origin> origin;
  /// Lists ε-moves before moves that read the next symbol, with each group in
  /// file order, or no moves for an accepting configuration.
  std::span<const Move> moves;
  bool accepting;
};

using Observer = std::function<void(const Visit&)>;

struct RunOptions {
  /// Limits the moves that the search can apply, including moves to previously
  /// found configurations, because without it an ε-cycle that pushes symbols
  /// would never stop.
  std::size_t max_expansions = 1'000'000;
  Observer observer = {};
};

struct RunResult {
  /// The transitions of a shortest accepting path, in order. Empty if no path
  /// accepts the input.
  std::optional<std::vector<std::size_t>> accepting_path;
  std::size_t expansions = 0;

  [[nodiscard]] bool Accepted() const { return accepting_path.has_value(); }
};

/// The search applied `RunOptions::max_expansions` moves and has no verdict.
struct LimitReached {
  std::size_t expansions;
};

}  // namespace npda

#endif  // NPDA_NPDA_RUN_H_
