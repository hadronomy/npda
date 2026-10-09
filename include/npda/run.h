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

/// An instantaneous description `(q, w, γ)`: the state, the quantity of input
/// that the automaton read, and the stack.
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
  /// True if the search found `target` before. The search does not explore a
  /// configuration two times.
  bool repeated;
};

/// One step of the search: a configuration and the moves that apply to it.
///
/// Ids count the visits from zero. The search visits a parent before its
/// children. The search stops at the first accepting configuration. Thus, a
/// move can point to a target that the search does not visit. The stack and
/// `moves` are valid only during the observer call.
struct Visit {
  std::size_t id;
  Configuration configuration;
  /// Empty for the start configuration.
  std::optional<Origin> origin;
  /// First the ε-moves, then the moves that read the next symbol. Each group is
  /// in file order. Empty for an accepting configuration.
  std::span<const Move> moves;
  bool accepting;
};

using Observer = std::function<void(const Visit&)>;

struct RunOptions {
  /// The maximum number of moves that the search can apply. This number
  /// includes the moves to configurations that the search found before. Without
  /// this limit, an ε-cycle that pushes symbols does not stop.
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
