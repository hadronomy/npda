// Wrap the NPDA headers in one module.
// Keep all includes in the global fragment above the module line.
// Re-export public names with export using.
// Change the headers to change the API. Add no code here.
module;
#include "npda.h"

export module npda;

namespace npda {

// Core engine.
export using npda::AcceptBy;
export using npda::Error;
export using npda::Hashable;
export using npda::Key;
export using npda::KeyHash;
export using npda::Node;
export using npda::NPDA;
export using npda::Rule;
export using npda::RunOptions;
export using npda::RunResult;
export using npda::explain_rule;
export using npda::generate_non_accepting_state_color;
export using npda::wrap_text;

}  // namespace npda
