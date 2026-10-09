#ifndef NPDA_TURING_RUN_H_
#define NPDA_TURING_RUN_H_

#include <cstddef>
#include <functional>
#include <optional>
#include <vector>

#include "automata/symbol.h"
#include "turing/tape.h"

namespace turing {

struct Configuration {
  Symbol state;
  /// One tape for each tape of the machine. The input starts on the first tape.
  std::vector<Tape> tapes;
};

/// The configuration after `number` moves.
///
/// The configuration is valid only during the observer call.
struct Step {
  std::size_t number;
  const Configuration& configuration;
  /// The transition that gave this configuration. Empty for the start
  /// configuration.
  std::optional<std::size_t> transition;
};

using Observer = std::function<void(const Step&)>;

struct RunOptions {
  std::size_t max_steps = 1'000'000;
  Observer observer = {};
};

struct RunResult {
  /// True if the machine halts in an accepting state. A machine halts if it is
  /// in an accepting state or if no transition applies.
  bool accepted;
  std::size_t steps;
  Configuration halted;
};

/// The machine did not halt after `RunOptions::max_steps` moves.
struct LimitReached {
  std::size_t steps;
};

}  // namespace turing

#endif  // NPDA_TURING_RUN_H_
