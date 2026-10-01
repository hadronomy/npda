#ifndef NPDA_DIAGNOSTICS_DIAGNOSTIC_H_
#define NPDA_DIAGNOSTICS_DIAGNOSTIC_H_

#include <algorithm>
#include <cstddef>
#include <functional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "diagnostics/source.h"

namespace diag {

enum class Severity { kError, kWarning, kNote, kHelp };

struct DiagnosticLabel {
  SourceSpan span{};
  bool primary = true;
  std::string message;
  int order = 0;  // paint sequence when labels share a line
};

struct Diagnostic {
  Severity severity = Severity::kError;
  std::string code;
  std::string message;
  std::vector<DiagnosticLabel> labels;
  std::vector<std::string> notes;

  // File key for multi-source renders. Empty means the source under render.
  [[nodiscard]] std::string_view file() const {
    for (const auto& lab : labels)
      if (!lab.span.file.empty()) return lab.span.file;
    return {};
  }
};

class DiagnosticBuilder;

struct DiagnosticSet {
  std::vector<Diagnostic> items;

  [[nodiscard]] bool HasErrors() const {
    return std::any_of(items.begin(), items.end(), [](const Diagnostic& d) {
      return d.severity == Severity::kError;
    });
  }

  void Emit(DiagnosticBuilder builder);
};

class DiagnosticBuilder {
 public:
  friend struct DiagnosticSet;

  DiagnosticBuilder(std::string code, std::string message,
                    Severity severity = Severity::kError) {
    d_.severity = severity;
    d_.code = std::move(code);
    d_.message = std::move(message);
  }

  DiagnosticBuilder& Label(SourceSpan span, std::string message,
                           bool primary = true, int order = 0) {
    d_.labels.push_back(DiagnosticLabel{.span = std::move(span),
                                        .primary = primary,
                                        .message = std::move(message),
                                        .order = order});
    return *this;
  }

  DiagnosticBuilder& Note(std::string note) {
    d_.notes.push_back(std::move(note));
    return *this;
  }

  void Emit(DiagnosticSet& dx) { dx.items.push_back(std::move(d_)); }

 private:
  Diagnostic d_;
};

[[nodiscard]] DiagnosticBuilder Error(std::string code, std::string message);
[[nodiscard]] DiagnosticBuilder Warning(std::string code, std::string message);

struct CodeInfo {
  std::string_view code;
  std::string_view machine;
  std::string_view title;
  std::string_view hint;
};

[[nodiscard]] std::vector<CodeInfo> Explain(std::string_view code);

}  // namespace diag

#endif  // NPDA_DIAGNOSTICS_DIAGNOSTIC_H_
