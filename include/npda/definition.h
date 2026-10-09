#ifndef NPDA_NPDA_DEFINITION_H_
#define NPDA_NPDA_DEFINITION_H_

#include <variant>
#include <vector>

#include "automata/symbol.h"
#include "npda/transition.h"

namespace npda {

/// The automaton accepts if it reads all of the input and stops in one of
/// `states`. This is an APf.
struct AcceptByFinalState {
  std::vector<Symbol> states;
};

/// The automaton accepts if it reads all of the input and its stack is empty.
/// This is an APv.
struct AcceptByEmptyStack {};

using Acceptance = std::variant<AcceptByFinalState, AcceptByEmptyStack>;

/// A pushdown automaton, as its file gives it.
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
