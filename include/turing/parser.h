#ifndef NPDA_TURING_PARSER_H_
#define NPDA_TURING_PARSER_H_

#include <optional>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "turing/machine.h"

namespace turing {

struct ParseResult {
  /// All diagnostics in source order, with spans that point into the source.
  /// Warnings can accompany a machine, but errors prevent its creation.
  diag::DiagnosticSet diagnostics;
  /// The machine, present only if `diagnostics` has no errors.
  std::optional<Machine> machine;
};

/// Reads a Turing machine in the file format of the course.
///
/// The file starts with Q, Σ, Γ, q0, b, F, and the number of tapes N, followed
/// by one transition per line. For N tapes, each transition has the form
/// `from read1 .. readN to write1 move1 .. writeN moveN`, or
/// `from read1 .. readN to action1 .. actionN` in the independent mode. An
/// optional `# /// config` comment block selects the machine variant and can
/// appear anywhere in the file.
///
/// If the line after b is one positive integer that is not a state, the file
/// has no F line, and F is empty.
[[nodiscard]] ParseResult Parse(const diag::SourceFile& source);

}  // namespace turing

#endif  // NPDA_TURING_PARSER_H_
