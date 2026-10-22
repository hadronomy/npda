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
       "npda",
       "missing required section",
       "Add the named section. Sections come in a fixed order: Q, \u03a3, "
       "\u0393, q0, Z0, then F if the automaton accepts by final state."},
      {"E0001",
       "turing",
       "missing required section",
       "Add the named section. Sections come in a fixed order: Q, \u03a3, "
       "\u0393, q0, b, F, then the number of tapes."},
      {"E0005",
       "common",
       "invalid q0 line",
       "Write exactly one token: the start state."},
      {"E0006",
       "npda",
       "invalid Z0 line",
       "Write exactly one token: the initial stack symbol."},
      {"E0006",
       "turing",
       "invalid blank symbol line",
       "Write exactly one token: the blank symbol."},
      {"E0007",
       "common",
       "start state not in Q",
       "Name a state from the Q line."},
      {"E0008",
       "npda",
       "initial stack symbol not in \u0393",
       "Name a symbol from the \u0393 line."},
      {"E0008",
       "turing",
       "blank symbol not in \u0393",
       "Name a symbol from the \u0393 line."},
      {"E0009",
       "common",
       "invalid accepting states line",
       "List only states from Q, or remove the line."},
      {"E0010",
       "npda",
       "transition too short",
       "Write from, input, stack top, target, then the push string."},
      {"E0010",
       "turing",
       "transition has the wrong number of parts",
       "Write from, one read per tape, and target. Then give a symbol and a "
       "move for each tape, or one action for each tape in the independent "
       "mode."},
      {"E0011", "common", "unknown 'from' state", "Name a state from Q."},
      {"E0012",
       "npda",
       "unknown input symbol",
       "Use a \u03a3 symbol, or '.' for \u03b5."},
      {"E0012", "turing", "unknown read symbol", "Use a \u0393 symbol."},
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
      {"E0015", "turing", "unknown write symbol", "Write only \u0393 symbols."},
      {"E0016",
       "turing",
       "nondeterministic transitions",
       "Keep one transition for each state and read, or change the reads."},
      {"E0017",
       "turing",
       "failed to build Turing machine",
       "Make each rule match the number of tapes and the config."},
      {"E0018",
       "turing",
       "blank symbol cannot be in \u03a3",
       "Remove the blank symbol from the \u03a3 line."},
      {"E0019",
       "turing",
       "invalid move direction",
       "Write L, R, or S. Without allow_stay, write L or R."},
      {"E0020",
       "turing",
       "tape symbol must be one character",
       "Write each \u0393 symbol as one character."},
      {"E0021",
       "turing",
       "input symbol not in \u0393",
       "Add the symbol to the \u0393 line. \u03a3 is a part of \u0393."},
      {"E0022",
       "turing",
       "invalid number of tapes line",
       "After F, write one positive integer: the number of tapes."},
      {"E0023",
       "turing",
       "move not allowed",
       "Use L or R, or set allow_stay = true in the config block."},
      {"E0024",
       "turing",
       "tape symbol is also a move",
       "Rename the symbol, or set operation_mode = simultaneous."},
      {"E0025",
       "turing",
       "invalid tape action",
       "Write a \u0393 symbol or a move for each tape."},
      {"C0001",
       "turing",
       "nested configuration block",
       "Close each block with '# ///' first."},
      {"C0002",
       "turing",
       "invalid configuration line: missing '='",
       "Write lines as 'key = value'."},
      {"C0003", "turing", "empty configuration key", "Write a key before '='."},
      {"C0004",
       "turing",
       "invalid configuration key",
       "Use letters, numbers, underscores, and hyphens."},
      {"C0005",
       "turing",
       "duplicate configuration key",
       "Delete the repeated line."},
      {"C0006",
       "turing",
       "empty configuration value",
       "Write a value after '='."},
      {"C0009",
       "turing",
       "invalid value for 'tape_direction'",
       "Write 'bidirectional' or 'right-only'."},
      {"C0010",
       "turing",
       "invalid value for 'operation_mode'",
       "Write 'simultaneous' or 'independent'."},
      {"C0011",
       "turing",
       "invalid value for 'allow_stay'",
       "Write 'true' or 'false'."},
      {"C0012",
       "turing",
       "unknown configuration key",
       "Make sure that the key is one of the known keys."},
      {"C0013",
       "turing",
       "unclosed configuration block",
       "Close the block with '# ///'."},
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
