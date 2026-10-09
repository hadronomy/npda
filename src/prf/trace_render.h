#ifndef NPDA_SRC_PRF_TRACE_RENDER_H_
#define NPDA_SRC_PRF_TRACE_RENDER_H_

#include <cstdint>
#include <ostream>
#include <span>
#include <string>

#include "prf/trace.h"
#include "terminal/style.h"

namespace prf {

/// Returns the arguments with commas between them: `2, 3`.
[[nodiscard]] std::string JoinArguments(
    std::span<const std::uint64_t> arguments
);

/// Writes the calls that `trace` recorded below the top-level call.
///
/// In counts mode, it writes the number of calls of each function, largest
/// first, and then the total. In tree mode, it writes the calls that the
/// top-level call made, as a tree. If the trace is off, it writes nothing.
void WriteTrace(
    std::ostream& out,
    const Trace& trace,
    const terminal::Style& style
);

}  // namespace prf

#endif  // NPDA_SRC_PRF_TRACE_RENDER_H_
