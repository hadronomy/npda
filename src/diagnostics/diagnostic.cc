#include "diagnostics/diagnostic.h"

#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace diag {

[[nodiscard]] DiagnosticBuilder Error(std::string code, std::string message) {
  return DiagnosticBuilder(std::move(code), std::move(message),
                           Severity::kError);
}

[[nodiscard]] DiagnosticBuilder Warning(std::string code, std::string message) {
  return DiagnosticBuilder(std::move(code), std::move(message),
                           Severity::kWarning);
}

void DiagnosticSet::Emit(DiagnosticBuilder builder) {
  items.push_back(std::move(builder.d_));
}

namespace {

[[nodiscard]] const std::vector<CodeInfo>& CodeTable() {
  static const std::vector<CodeInfo> kTable = {
      {"E0001", "common", "missing required section",
       "Add the named section. Sections come in fixed order."},
      {"E0002", "common", "empty set Q",
       "List at least one state on the Q line."},
      {"E0003", "common", "empty alphabet \u03a3",
       "List at least one input symbol on the \u03a3 line."},
      {"E0004", "npda", "empty stack alphabet \u0393",
       "List at least one stack symbol on the \u0393 line."},
      {"E0004", "turing", "empty tape alphabet \u0393",
       "List at least one tape symbol on the \u0393 line."},
      {"E0005", "common", "invalid q0 line",
       "Write exactly one token: the start state."},
      {"E0006", "npda", "invalid Z0 line",
       "Write exactly one token: the bottom symbol."},
      {"E0006", "turing", "invalid blank symbol line",
       "Write exactly one token: the blank symbol."},
      {"E0007", "common", "start state not in Q",
       "Name a state from the Q line."},
      {"E0008", "npda", "stack bottom symbol not in \u0393",
       "Name a symbol from the \u0393 line."},
      {"E0008", "turing", "blank symbol not in \u0393",
       "Name a symbol from the \u0393 line."},
      {"E0009", "common", "invalid accepting states line",
       "List only states from Q, or drop the line."},
      {"E0010", "npda", "transition too short",
       "Write from, input, stack top, target, then pushes."},
      {"E0010", "turing", "transition must have exactly 5 parts",
       "Write from, read, target, write, move."},
      {"E0010", "turing", "multi-tape transition must have correct format",
       "Write from, reads, target, then write and move pairs."},
      {"E0011", "common", "unknown 'from' state", "Name a state from Q."},
      {"E0012", "npda", "unknown input symbol (or epsilon)",
       "Use a \u03a3 symbol or an epsilon spelling."},
      {"E0012", "turing", "unknown read symbol", "Use a \u0393 symbol."},
      {"E0013", "npda", "unknown stack top symbol (or epsilon)",
       "Use a \u0393 symbol or an epsilon spelling."},
      {"E0014", "common", "unknown 'to' state", "Name a state from Q."},
      {"E0015", "npda", "unknown push symbol",
       "Push only \u0393 symbols or epsilon."},
      {"E0015", "turing", "unknown write symbol", "Write only \u0393 symbols."},
      {"E0019", "turing", "invalid move direction", "Write L, R, or S."},
      {"E0017", "turing", "failed to build Turing Machine",
       "Fix the errors above first."},
      {"E0018", "turing", "blank symbol cannot be in \u03a3",
       "Remove the blank symbol from the \u03a3 line."},
      {"E0016", "npda", "failed to build NPDA", "Fix the errors above first."},
      {"C0001", "turing", "nested configuration block",
       "Close each block with '# ///' first."},
      {"C0002", "turing", "invalid configuration line: missing '='",
       "Write lines as 'key = value'."},
      {"C0003", "turing", "empty configuration key", "Write a key before '='."},
      {"C0004", "turing", "invalid configuration key",
       "Use letters, numbers, underscores, and hyphens."},
      {"C0005", "turing", "duplicate configuration key",
       "Delete the repeated line."},
      {"C0006", "turing", "empty configuration value",
       "Write a value after '='."},
      {"C0007", "turing", "invalid value for 'num_tapes'",
       "Write a positive integer such as 1, 2, or 3."},
      {"C0009", "turing", "invalid value for 'tape_direction'",
       "Write 'bidirectional' or 'right-only'."},
      {"C0010", "turing", "invalid value for 'operation_mode'",
       "Write 'simultaneous' or 'independent'."},
      {"C0011", "turing", "invalid value for 'allow_stay'",
       "Write 'true' or 'false'."},
      {"C0012", "turing", "unknown configuration key",
       "Check the spelling against the known keys."},
      {"C0013", "turing", "unclosed configuration block",
       "Close the block with '# ///'."},
  };
  return kTable;
}

}  // namespace

[[nodiscard]] std::vector<CodeInfo> Explain(std::string_view code) {
  std::vector<CodeInfo> out;
  for (const auto& e : CodeTable())
    if (e.code == code) out.push_back(e);
  return out;
}

}  // namespace diag
