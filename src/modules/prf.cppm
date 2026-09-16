// Wrap the primitive recursive function header in one module.
// Keep all includes in the global fragment above the module line.
// Re-export public names with export using.
// Change the header to change the API. Add no code here.
module;
#include "prf.h"

export module prf;

namespace prf {

// Base class and combinators.
export using prf::Composition;
export using prf::Function;
export using prf::PrimitiveRecursion;
export using prf::Projection;
export using prf::Successor;
export using prf::Trace;
export using prf::Zero;
export using prf::compose;
export using prf::join_u64;
export using prf::primitive_rec;
export using prf::proj;
export using prf::succ;
export using prf::zero;

}  // namespace prf

namespace dsl {

// Tiny builder language for primitive recursive functions.
export using dsl::Arg;
export using dsl::_1;
export using dsl::_2;
export using dsl::_3;
export using dsl::_4;
export using dsl::_5;
export using dsl::_6;
export using dsl::_7;
export using dsl::_8;
export using dsl::Context;
export using dsl::R1;
export using dsl::Rn;

}  // namespace dsl
