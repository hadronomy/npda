#ifndef NPDA_SRC_TURING_TRACE_H_
#define NPDA_SRC_TURING_TRACE_H_

#include <cstddef>
#include <expected>
#include <optional>
#include <ostream>
#include <span>
#include <string>

#include "automata/symbol.h"
#include "terminal/style.h"
#include "turing/config.h"
#include "turing/definition.h"
#include "turing/machine.h"
#include "turing/run.h"
#include "turing/tape.h"
#include "turing/transition.h"

namespace turing {

enum class TraceView {
  /// One block per step, showing the state, each tape as cells with a mark
  /// below the head, and the transition that produced the step.
  kDetailed,
  /// One line for each step.
  kCompact,
};

struct TraceOptions {
  TraceView view = TraceView::kDetailed;
  /// Limits the steps shown in the trace while the run continues past this
  /// limit.
  std::size_t limit = 200;
  /// If true, the detailed view adds a note in words below each transition.
  bool explain = false;
};

/// Returns the definition in one line:
///
/// ```text
/// q0 = q0 · b = _ · F = {qf} · 1 tape · bidirectional · … · 9 transitions
/// ```
[[nodiscard]] std::string Describe(
    const Definition& definition,
    const terminal::Style& style
);

/// Returns a transition such as `(q0, a) → (q1, X, R)`. With more tapes, the
/// condition includes one read per tape, and the result includes one write and
/// one move per tape. In the independent mode, each tape has one action:
/// `(q0, a) → (q1, X)` or `(q0, a) → (q1, R)`.
[[nodiscard]] std::string FormatTransition(
    const Transition& transition,
    OperationMode mode,
    const terminal::Style& style
);

/// Returns the tape in the form of the course: `ab[c]d`.
///
/// The text spans the first through the last nonblank cell, extending to the
/// head if it lies outside that span, and brackets the cell under the head.
/// With a `radius`, only the cells near the head remain, with an ellipsis at
/// each cut.
[[nodiscard]] std::string FormatTape(
    const Tape& tape,
    Symbol blank,
    const terminal::Style& style,
    std::optional<std::size_t> radius = std::nullopt
);

/// Writes the numbered transitions, with an explanation in words for each
/// transition if `explain` is true.
void WriteTransitions(
    std::ostream& out,
    const Definition& definition,
    const terminal::Style& style,
    bool explain
);

/// Runs `machine` on `input` and writes each step to `out`.
///
/// # Errors
///
/// `LimitReached` if the machine did not halt after `max_steps` moves, with the
/// completed steps still shown in the trace.
[[nodiscard]] std::expected<RunResult, LimitReached> TraceRun(
    const Machine& machine,
    std::span<const Symbol> input,
    std::size_t max_steps,
    const TraceOptions& options,
    std::ostream& out,
    const terminal::Style& style
);

}  // namespace turing

#endif  // NPDA_SRC_TURING_TRACE_H_
