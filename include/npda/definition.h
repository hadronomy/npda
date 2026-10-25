#ifndef NPDA_NPDA_DEFINITION_H_
#define NPDA_NPDA_DEFINITION_H_

#include <variant>
#include <vector>

#include "automata/symbol.h"
#include "npda/transition.h"

namespace npda {

/// An APf accepts after it reads all of the input and stops in one of `states`.
struct AcceptByFinalState {
  std::vector<Symbol> states;
};

/// An APv accepts after it reads all of the input and its stack is empty.
struct AcceptByEmptyStack {};

using Acceptance = std::variant<AcceptByFinalState, AcceptByEmptyStack>;

struct Definition {
  Symbol start_state;
  Symbol initial_stack_symbol;
  Acceptance acceptance;
  /// The transitions, in file order. Traces number the transitions from one, so
  /// index 0 is δ1.
  std::vector<Transition> transitions;
};

}  // namespace npda

#endif  // NPDA_NPDA_DEFINITION_H_
