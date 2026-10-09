#ifndef NPDA_DIAGNOSTICS_DIAGNOSTIC_H_
#define NPDA_DIAGNOSTICS_DIAGNOSTIC_H_

#include <algorithm>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "diagnostics/source.h"

namespace diag {

enum class Severity { kError, kWarning };

/// A message about one part of the source text.
struct Label {
  SourceSpan span;
  std::string message;
};

/// One problem in a source file.
struct Diagnostic {
  Severity severity = Severity::kError;
  /// A code that does not change, for example `E0007`. `cc explain` gives the
  /// meaning of each code.
  std::string code;
  std::string message;
  std::vector<Label> labels;
  /// Advice that the renderer writes below the source lines.
  std::vector<std::string> notes;
};

struct DiagnosticSet {
  std::vector<Diagnostic> items;

  [[nodiscard]] bool HasErrors() const {
    return std::ranges::any_of(items, [](const Diagnostic& diagnostic) {
      return diagnostic.severity == Severity::kError;
    });
  }
};

/// Makes a diagnostic one part at a time:
///
/// ```cpp
/// diag::Error("E0007", "start state not in Q")
///     .Label(span, "this state is not listed in Q")
///     .Emit(diagnostics);
/// ```
class DiagnosticBuilder {
 public:
  DiagnosticBuilder(Severity severity, std::string code, std::string message)
      : diagnostic_{
            .severity = severity,
            .code = std::move(code),
            .message = std::move(message),
            .labels = {},
            .notes = {},
        } {}

  DiagnosticBuilder& Label(SourceSpan span, std::string message) {
    diagnostic_.labels.push_back({.span = span, .message = std::move(message)});
    return *this;
  }

  DiagnosticBuilder& Note(std::string note) {
    diagnostic_.notes.push_back(std::move(note));
    return *this;
  }

  void Emit(DiagnosticSet& diagnostics) {
    diagnostics.items.push_back(std::move(diagnostic_));
  }

 private:
  Diagnostic diagnostic_;
};

[[nodiscard]] DiagnosticBuilder Error(std::string code, std::string message);

[[nodiscard]] DiagnosticBuilder Warning(std::string code, std::string message);

/// The meaning of a diagnostic code for one type of machine.
struct CodeInfo {
  std::string_view code;
  /// `npda`, `turing`, or `common`.
  std::string_view machine;
  std::string_view title;
  /// How to correct the problem.
  std::string_view hint;
};

/// Returns the entries for `code`, one for each type of machine that uses the
/// code. Empty for an unknown code.
[[nodiscard]] std::vector<CodeInfo> Explain(std::string_view code);

}  // namespace diag

#endif  // NPDA_DIAGNOSTICS_DIAGNOSTIC_H_
