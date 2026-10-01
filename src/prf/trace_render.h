#ifndef NPDA_SRC_PRF_TRACE_RENDER_H_
#define NPDA_SRC_PRF_TRACE_RENDER_H_

#include <ostream>
#include <span>
#include <string>
#include <string_view>

#include "prf/function.h"

namespace prf {

[[nodiscard]] std::string JoinArguments(
    std::span<const std::uint64_t> arguments,
    std::string_view separator = ", ");
void RenderTrace(const Trace& trace, std::ostream& output);

}  // namespace prf

#endif  // NPDA_SRC_PRF_TRACE_RENDER_H_
