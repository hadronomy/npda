#ifndef NPDA_TURING_DEFINITION_H_
#define NPDA_TURING_DEFINITION_H_

#include <cstddef>
#include <vector>

#include "automata/symbol.h"
#include "turing/config.h"
#include "turing/transition.h"

namespace turing {

/// A Turing machine as defined in its file.
struct Definition {
  Config config;
  std::size_t num_tapes = 1;
  /// Σ in file order, containing all the symbols that an input can use.
  std::vector<Symbol> input_alphabet;
  Symbol start_state;
  Symbol blank;
  std::vector<Symbol> accepting_states;
  /// The transitions, in file order. Traces number the transitions from one, so
  /// index 0 is δ1.
  std::vector<Transition> transitions;
};

}  // namespace turing

#endif  // NPDA_TURING_DEFINITION_H_
