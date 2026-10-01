#ifndef NPDA_TURING_TRACE_H_
#define NPDA_TURING_TRACE_H_

#include <functional>
#include <string_view>

#include "turing/machine.h"

namespace turing {

struct TraceOptions {
  // Pretty, colored per-step trace diagrams printed during run.
  bool enabled = false;
  // Optional sink; if not set and tracing is on, prints via std::print.
  std::function<void(std::string_view)> sink = {};

  // Trace formatting options
  bool colors = true;
  bool explanations = false;

  // Live per-step emission. Off means replay-only output on accept.
  bool show_full_trace = true;

  // Output cap. Live and replay steps each stop here with a hidden count.
  // Retain at least two rows so both ends of a path remain visible.
  std::size_t step_limit = 200;
};

void ShowConfiguration(const Definition& definition,
                       const TraceOptions& options = {});
void RenderTrace(const Definition& definition, const ExecutionEvent& event,
                 const TraceOptions& options);

}  // namespace turing

#endif  // NPDA_TURING_TRACE_H_
