#ifndef NPDA_SRC_NPDA_TRACE_H_
#define NPDA_SRC_NPDA_TRACE_H_

#include <cstddef>
#include <expected>
#include <ostream>
#include <span>
#include <string>

#include "automata/symbol.h"
#include "npda/definition.h"
#include "npda/machine.h"
#include "npda/run.h"
#include "npda/transition.h"
#include "terminal/style.h"

namespace npda {

enum class TraceView {
  /// One block for each configuration. The block shows the state, and the input
  /// and the stack as cells. It also shows each move out of the configuration
  /// and the configuration that the move gives.
  kDetailed,
  /// One line for each configuration, with the moves out of it below the line.
  kCompact,
  /// The search as a tree. Each configuration is below the configuration that
  /// gave it.
  kTree,
};

struct TraceOptions {
  TraceView view = TraceView::kDetailed;
  /// The maximum number of configurations that the trace shows. The search
  /// continues after this limit.
  std::size_t limit = 200;
  /// If true, the detailed view adds a note in words below each move.
  bool explain = false;
};

/// Returns the definition in one line:
///
/// ```text
/// final state F = {q3} · q0 = q1 · Z0 = S · 5 transitions
/// ```
[[nodiscard]] std::string Describe(
    const Definition& definition,
    const terminal::Style& style
);

/// Returns a rule in the form of the course: `(q1, a, S) → (q1, AS)`.
[[nodiscard]] std::string FormatTransition(
    const Transition& transition,
    const terminal::Style& style
);

/// Returns the labels of the transitions, with a space between them: `δ1 δ2
/// δ3`.
[[nodiscard]] std::string FormatPath(
    std::span<const std::size_t> path,
    const terminal::Style& style
);

/// Writes the numbered transitions. If `explain` is true, each rule also has a
/// note in words.
void WriteTransitions(
    std::ostream& out,
    const Definition& definition,
    const terminal::Style& style,
    bool explain
);

/// Runs `machine` on `input` and writes the search to `out`.
///
/// The trace shows each configuration that the search visits, in the order of
/// the visits. It also shows the moves out of each configuration. If the search
/// explored other branches before it accepted, the trace ends with the
/// accepting path.
///
/// # Errors
///
/// `LimitReached` if the search applies `max_expansions` moves and has no
/// verdict. The trace shows the configurations that the search visited.
[[nodiscard]] std::expected<RunResult, LimitReached> TraceRun(
    const Machine& machine,
    std::span<const Symbol> input,
    std::size_t max_expansions,
    const TraceOptions& options,
    std::ostream& out,
    const terminal::Style& style
);

}  // namespace npda

#endif  // NPDA_SRC_NPDA_TRACE_H_
