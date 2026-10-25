#ifndef NPDA_SRC_PRF_TRACE_RENDER_H_
#define NPDA_SRC_PRF_TRACE_RENDER_H_

#include <ostream>
#include <span>
#include <string>

#include "prf/function.h"
#include "prf/trace.h"
#include "terminal/style.h"

namespace prf {

/// Returns the notation that `style` can show: Unicode indices and operators,
/// or their ASCII forms.
[[nodiscard]] Notation NotationFor(const terminal::Style& style);

/// Returns a call with its results, as in `pow(2, 3) = 8` or
/// `(P₃³ × P₁³)(2, 0, 1) = (1, 2)`.
[[nodiscard]] std::string FormatCall(
    const Function& function,
    std::span<const Natural> arguments,
    std::span<const Natural> results,
    const terminal::Style& style
);

/// Writes the recorded trace after a blank line, without the total call count,
/// or nothing if the trace is off.
///
/// In counts mode, functions with the same name share one row, with the largest
/// call count first. In tree mode, the output starts with each top-level call's
/// children, then gives the number of calls the trace does not keep.
void WriteTrace(
    std::ostream& out,
    const Trace& trace,
    const terminal::Style& style
);

}  // namespace prf

#endif  // NPDA_SRC_PRF_TRACE_RENDER_H_
