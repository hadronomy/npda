#ifndef NPDA_TURING_PARSER_H_
#define NPDA_TURING_PARSER_H_

#include <optional>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "turing/machine.h"

namespace turing {

struct ParseResult {
  /// All of the problems that the parser found, in source order. Their spans
  /// point into the source. Warnings can occur together with a machine.
  diag::DiagnosticSet diagnostics;
  /// The machine. It is present only if `diagnostics` has no errors.
  std::optional<Machine> machine;
};

/// Reads a Turing machine in the file format of the course.
///
/// The file starts with an optional `# /// config` block. Then it has the lines
/// Q, Σ, Γ, q0, and b, an optional F line, and one rule on each line. With N
/// tapes, a rule is `from read1 .. readN to write1 move1 .. writeN moveN`.
[[nodiscard]] ParseResult Parse(const diag::SourceFile& source);

}  // namespace turing

#endif  // NPDA_TURING_PARSER_H_
