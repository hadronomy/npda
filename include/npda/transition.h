#ifndef NPDA_NPDA_TRANSITION_H_
#define NPDA_NPDA_TRANSITION_H_

#include <optional>
#include <vector>

#include "automata/symbol.h"

namespace npda {

using automata::Symbol;

/// One move of the transition function: `(to, push) ∈ δ(from, input, top)`.
///
/// The move pops `top`. Thus, the move applies only if `top` is on top of the
/// stack. A move without `input` is an ε-move. It reads no symbol.
struct Transition {
  Symbol from;
  std::optional<Symbol> input;
  Symbol top;
  Symbol to;
  /// The symbols that replace `top`. `push.front()` becomes the new top.
  std::vector<Symbol> push;
};

}  // namespace npda

#endif  // NPDA_NPDA_TRANSITION_H_
