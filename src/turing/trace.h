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
#include "turing/definition.h"
#include "turing/machine.h"
#include "turing/run.h"
#include "turing/tape.h"
#include "turing/transition.h"

namespace turing {

enum class TraceView {
  /// One block for each step. The block shows the state, each tape as cells
  /// with a mark below the head, and the transition that gave the step.
  kDetailed,
  /// One line for each step.
  kCompact,
};

struct TraceOptions {
  TraceView view = TraceView::kDetailed;
  /// The maximum number of steps that the trace shows. The run continues after
  /// this limit.
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

/// Returns a rule: `(q0, a) → (q1, X, R)`. With more tapes, the condition has
/// one read for each tape. The result has one write and one move for each tape.
[[nodiscard]] std::string FormatTransition(
    const Transition& transition,
    const terminal::Style& style
);

/// Returns the tape in the form of the course: `ab[c]d`.
///
/// The text starts at the first cell that is not blank and ends at the last
/// one. If the head is outside these cells, the text extends to the head. The
/// cell below the head is in brackets. With a `radius`, the text keeps only the
/// cells near the head, and an ellipsis shows each cut.
[[nodiscard]] std::string FormatTape(
    const Tape& tape,
    Symbol blank,
    const terminal::Style& style,
    std::optional<std::size_t> radius = std::nullopt
);

/// Writes the numbered transitions. If `explain` is true, each rule also has a
/// note in words.
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
/// `LimitReached` if the machine did not halt after `max_steps` moves. The
/// trace shows the steps that the machine did.
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
