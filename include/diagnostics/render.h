#ifndef NPDA_DIAGNOSTICS_RENDER_H_
#define NPDA_DIAGNOSTICS_RENDER_H_

#include <ostream>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"

namespace diag {

enum class ColorMode { kAuto, kAlways, kNever };

enum class CharSet { kAuto, kUnicode, kAscii };

struct RenderOptions {
  /// With `kAuto`, the renderer uses color only if standard error is a terminal
  /// that shows color.
  ColorMode color = ColorMode::kAuto;
  /// With `kAuto`, the renderer uses Unicode only if the locale is UTF-8.
  CharSet charset = CharSet::kAuto;
  /// If true, the renderer writes all of the diagnostics in full. If false,
  /// diagnostics with the same code become one list of locations.
  bool verbose = false;
};

/// Writes `diagnostics` with lines of `source`, grouped by severity and code.
/// The groups follow the order in which the parser found their first
/// diagnostic:
///
/// ```text
/// █ error[E0007]: start state not in Q
///   --> machine.txt:6:1
///   5 │ X y
///     │
///   6 │ A
///     │ ∿
///     │ ↑ this state is not listed in Q
///   7 │ X
/// ```
///
/// If two diagnostics have the same code and labels, the renderer writes only
/// the first.
void Render(
    std::ostream& output,
    const SourceFile& source,
    const DiagnosticSet& diagnostics,
    const RenderOptions& options = {}
);

}  // namespace diag

#endif  // NPDA_DIAGNOSTICS_RENDER_H_
