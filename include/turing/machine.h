#ifndef NPDA_TURING_MACHINE_H_
#define NPDA_TURING_MACHINE_H_

#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <vector>

#include "automata/symbol.h"
#include "turing/definition.h"
#include "turing/run.h"

namespace turing {

struct Error {
  std::string message;
};

/// A deterministic Turing machine with one or more tapes.
class Machine {
 public:
  /// Makes a machine from `definition` after it examines the definition.
  ///
  /// # Errors
  ///
  /// An `Error` if the machine has no tapes, or if a transition has the wrong
  /// number of tape actions. An `Error` also if an action breaks the variant
  /// of `config`: a write and a move in the independent mode, or S when
  /// `allow_stay` is false. An `Error` also if a transition reads the same
  /// symbols in the same state as an earlier transition.
  [[nodiscard]] static std::expected<Machine, Error> Create(
      Definition definition
  );

  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const Definition& definition() const { return definition_; }

  /// Returns the first symbol of `input` that is not in Σ. Returns nothing if
  /// `input` uses only Σ.
  [[nodiscard]] std::optional<Symbol> FindForeignSymbol(
      std::span<const Symbol> input
  ) const;

  /// Runs the machine on `input` until no transition applies.
  ///
  /// The input starts on the first tape, under the head. The other tapes start
  /// blank. The machine does not stop when it enters an accepting state: an
  /// accepting state with transitions out of it keeps the machine running. Runs
  /// do not share data. Thus, many runs can use one machine at the same time.
  ///
  /// # Errors
  ///
  /// `LimitReached` if the machine did not halt after `options.max_steps`
  /// moves.
  [[nodiscard]] std::expected<RunResult, LimitReached> Run(
      std::span<const Symbol> input,
      const RunOptions& options = {}
  ) const;

 private:
  explicit Machine(Definition definition);

  [[nodiscard]] std::optional<std::size_t> FindTransition(
      const Configuration& configuration
  ) const;

  Definition definition_;
  std::unordered_map<Symbol, std::vector<std::size_t>> transitions_by_state_;
};

}  // namespace turing

#endif  // NPDA_TURING_MACHINE_H_
