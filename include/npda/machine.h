#ifndef NPDA_NPDA_MACHINE_H_
#define NPDA_NPDA_MACHINE_H_

#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "automata/symbol.h"
#include "npda/definition.h"
#include "npda/run.h"

namespace npda {

/// A nondeterministic pushdown automaton.
///
/// A run does a breadth-first search of the configurations. Thus, if an
/// accepting path exists, the run finds a shortest one.
class Machine {
 public:
  explicit Machine(Definition definition);

  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const Definition& definition() const { return definition_; }

  /// Searches for an accepting path for `input`.
  ///
  /// Runs do not share data. Thus, many runs can use one machine at the same
  /// time.
  ///
  /// # Errors
  ///
  /// `LimitReached` if the search applies `options.max_expansions` moves and
  /// has no verdict.
  [[nodiscard]] std::expected<RunResult, LimitReached> Run(
      std::span<const Symbol> input,
      const RunOptions& options = {}
  ) const;

 private:
  struct MoveKey {
    Symbol state;
    std::optional<Symbol> input;
    Symbol top;

    bool operator==(const MoveKey&) const = default;
  };

  struct MoveKeyHash {
    std::size_t operator()(const MoveKey& key) const;
  };

  [[nodiscard]] std::span<const std::size_t> MovesFor(const MoveKey& key) const;

  Definition definition_;
  /// The indices of the transitions for each key. Each list is in file order.
  std::unordered_map<MoveKey, std::vector<std::size_t>, MoveKeyHash> moves_;
};

}  // namespace npda

#endif  // NPDA_NPDA_MACHINE_H_
