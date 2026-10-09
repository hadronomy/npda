#include "diagnostics/diagnostic.h"

#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace diag {

DiagnosticBuilder Error(std::string code, std::string message) {
  return {Severity::kError, std::move(code), std::move(message)};
}

DiagnosticBuilder Warning(std::string code, std::string message) {
  return {Severity::kWarning, std::move(code), std::move(message)};
}

namespace {

const std::vector<CodeInfo>& CodeTable() {
  static const std::vector<CodeInfo> kTable = {
      {"E0001",
       "common",
       "missing required section",
       "Add the named section. Sections come in a fixed order: Q, \u03a3, "
       "\u0393, q0, then Z0."},
      {"E0005",
       "common",
       "invalid q0 line",
       "Write exactly one token: the start state."},
      {"E0006",
       "npda",
       "invalid Z0 line",
       "Write exactly one token: the initial stack symbol."},
      {"E0007",
       "common",
       "start state not in Q",
       "Name a state from the Q line."},
      {"E0008",
       "npda",
       "initial stack symbol not in \u0393",
       "Name a symbol from the \u0393 line."},
      {"E0009",
       "common",
       "invalid accepting states line",
       "List only states from Q, or remove the line."},
      {"E0010",
       "npda",
       "transition too short",
       "Write from, input, stack top, target, then the push string."},
      {"E0011", "common", "unknown 'from' state", "Name a state from Q."},
      {"E0012",
       "npda",
       "unknown input symbol",
       "Use a \u03a3 symbol, or '.' for \u03b5."},
      {"E0013",
       "npda",
       "unknown stack top symbol",
       "Use a \u0393 symbol. Every move replaces the symbol on top of the "
       "stack."},
      {"E0014", "common", "unknown 'to' state", "Name a state from Q."},
      {"E0015",
       "npda",
       "unknown push symbol",
       "Push only \u0393 symbols, or '.' for \u03b5."},
  };
  return kTable;
}

}  // namespace

std::vector<CodeInfo> Explain(std::string_view code) {
  std::vector<CodeInfo> entries;
  for (const auto& entry : CodeTable()) {
    if (entry.code == code) entries.push_back(entry);
  }
  return entries;
}

}  // namespace diag
