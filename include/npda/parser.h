#ifndef NPDA_NPDA_PARSER_H_
#define NPDA_NPDA_PARSER_H_

#include <optional>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "npda/machine.h"

namespace npda {

struct ParseResult {
  /// All of the problems that the parser found, in source order. Their spans
  /// point into the source.
  diag::DiagnosticSet diagnostics;
  /// The machine. It is present only if `diagnostics` has no errors.
  std::optional<Machine> machine;
};

/// Reads a pushdown automaton in the file format of the course.
///
/// The file has the lines Q, Σ, Γ, q0, and Z0. Then it has an optional F line
/// and one transition on each line:
///
/// ```text
/// q1 q2 q3
/// a b
/// S A
/// q1
/// S
/// q3
/// q1 a S q1 AS
/// q2 . S q3 S
/// ```
///
/// An F line selects acceptance by final state (APf), while its absence selects
/// acceptance by empty stack (APv). In either format, `.` represents ε.
[[nodiscard]] ParseResult Parse(const diag::SourceFile& source);

}  // namespace npda

#endif  // NPDA_NPDA_PARSER_H_
