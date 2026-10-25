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
  /// One block per configuration, with its state, input and stack as cells,
  /// and each applicable move alongside the configuration that it produces.
  kDetailed,
  /// One line for each configuration, with the moves out of it below the line.
  kCompact,
  /// Places each configuration below its parent to show the search as a tree.
  kTree,
};

struct TraceOptions {
  TraceView view = TraceView::kDetailed;
  /// Limits the configurations shown in the trace while the search continues
  /// past this limit.
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
/// Shows configurations in visit order, each with its applicable moves.
/// If the search explored other branches before acceptance, the trace ends
/// with the accepting path.
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
