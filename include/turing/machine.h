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
  /// Checks `definition` and uses it to create a machine.
  ///
  /// # Errors
  ///
  /// An `Error` if the machine has no tapes, a transition has the wrong number
  /// of tape actions, or two transitions read the same symbols in the same
  /// state. Actions must also obey the variant in `config`: independent mode
  /// permits either a write or a move on each tape, and `allow_stay = false`
  /// forbids S moves.
  [[nodiscard]] static std::expected<Machine, Error> Create(
      Definition definition
  );

  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const Definition& definition() const { return definition_; }

  /// Returns the first symbol of `input` outside Σ, or nothing if every symbol
  /// belongs to Σ.
  [[nodiscard]] std::optional<Symbol> FindForeignSymbol(
      std::span<const Symbol> input
  ) const;

  /// Runs the machine on `input` until no transition applies.
  ///
  /// The input starts on the first tape under the head, while the other tapes
  /// start blank. An accepting state stops the machine only when no transition
  /// applies, so a run can continue through an accepting state. Because runs
  /// do not share data, many runs can use one machine at the same time.
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
