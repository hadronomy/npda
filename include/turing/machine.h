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
  /// An `Error` if a transition has the wrong number of tape actions, or uses a
  /// move that `config` does not allow. An `Error` also if a transition reads
  /// the same symbols in the same state as an earlier transition.
  [[nodiscard]] static std::expected<Machine, Error> Create(
      Definition definition
  );

  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const Definition& definition() const { return definition_; }

  /// Runs the machine on `input` until the machine halts.
  ///
  /// The input starts on the first tape, under the head. The other tapes start
  /// blank. Runs do not share data. Thus, many runs can use one machine at the
  /// same time.
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
