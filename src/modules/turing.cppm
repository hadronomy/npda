// Wrap the Turing machine headers in one module.
// Keep all includes in the global fragment above the module line.
// Re-export public names with export using.
// Change the headers to change the API. Add no code here.
module;
#include "turing/turing.h"

export module turing;

namespace turing {

// Core engine.
export using turing::Direction;
export using turing::Error;
export using turing::GraphvizOptions;
export using turing::Hashable;
export using turing::MultiTapeNode;
export using turing::MultiTapeRule;
export using turing::OperationMode;
export using turing::PairHash;
export using turing::Rule;
export using turing::RunOptions;
export using turing::RunResult;
export using turing::TMConfig;
export using turing::TapeDirection;
export using turing::TuringMachine;
export using turing::make_single_rule;

}  // namespace turing
