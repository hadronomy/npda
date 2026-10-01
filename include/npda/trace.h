#ifndef NPDA_NPDA_TRACE_H_
#define NPDA_NPDA_TRACE_H_

#include <functional>
#include <string_view>

#include "npda/machine.h"

namespace npda {

struct TraceOptions {
  // Pretty, colored per-step trace diagrams printed during run.
  bool enabled = false;
  // Optional sink; if not set and tracing is on, prints via std::print.
  std::function<void(std::string_view)> sink = {};

  // Trace formatting options
  bool colors = true;
  bool compact = false;
  bool explanations = false;

  // Backtracking visualization options
  bool show_backtracking =
      true;  // Enable backtracking detection and visualization
  bool show_full_trace =
      true;  // Show complete execution trace including backtracks

  // Box table behind the linear stack row. Default off: chrome must
  // never double the line count of the default view.
  bool box = false;
  // Exploration tree. Default off; reject paths still print it.
  bool tree = false;

  // Output cap. Live steps, replay steps, and tree rows each stop here
  // with a hidden count. Bounds trace bytes regardless of max_expansions.
  // Retain at least two rows so both ends of a path remain visible.
  std::size_t step_limit = 200;
};

void RenderTrace(const Definition& definition, const ExecutionEvent& event,
                 const TraceOptions& options);

}  // namespace npda

#endif  // NPDA_NPDA_TRACE_H_
