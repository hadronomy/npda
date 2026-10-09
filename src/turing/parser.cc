#include "turing/parser.h"

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "parsing/reader.h"
#include "turing/config_parser.h"
#include "turing/machine.h"
#include "turing/transition.h"

namespace turing {

namespace {

using parsing::Line;
using parsing::SymbolSet;
using parsing::Token;

/// Reads the rules of a machine with N tapes. A rule has 3N + 2 tokens:
///
/// ```text
/// from  read1 .. readN  to  write1 move1 .. writeN moveN
/// ```
///
/// With one tape, this is the usual `from read to write move`.
class RuleParser {
 public:
  RuleParser(
      std::size_t num_tapes,
      const SymbolSet& states,
      const SymbolSet& tape_alphabet,
      diag::DiagnosticSet& diagnostics
  )
      : num_tapes_(num_tapes),
        states_(states),
        tape_alphabet_(tape_alphabet),
        diagnostics_(diagnostics) {}

  [[nodiscard]] std::size_t Arity() const { return 3 * num_tapes_ + 2; }

  /// Returns true if the line is a correct rule. Reports nothing.
  [[nodiscard]] bool Matches(const Line& line) const {
    if (line.tokens.size() != Arity()) return false;
    const Fields fields = Split(line);
    if (!states_.contains(fields.from.symbol) ||
        !states_.contains(fields.to.symbol)) {
      return false;
    }
    for (std::size_t tape = 0; tape < num_tapes_; ++tape) {
      if (!tape_alphabet_.contains(fields.Read(tape).symbol) ||
          !tape_alphabet_.contains(fields.Write(tape).symbol) ||
          !ParseHeadMove(fields.Move(tape).symbol.name())) {
        return false;
      }
    }
    return true;
  }

  /// Reads one rule and reports each bad field. The result is correct only if
  /// the function reported nothing.
  std::optional<Transition> Parse(const Line& line) {
    if (line.tokens.size() != Arity()) {
      parsing::ReportError(
          diagnostics_,
          "E0010",
          std::format("transition must have {} parts", Arity()),
          line.span,
          num_tapes_ == 1 ? "expected: from read to write move"
                          : "expected: from read1..readN to write1 move1 .. "
                            "writeN moveN"
      );
      return std::nullopt;
    }
    const Fields fields = Split(line);
    CheckState(fields.from, "E0011", "unknown 'from' state");
    CheckState(fields.to, "E0014", "unknown 'to' state");
    Transition rule{
        .from = fields.from.symbol,
        .tapes = {},
        .to = fields.to.symbol
    };
    for (std::size_t tape = 0; tape < num_tapes_; ++tape) {
      CheckSymbol(fields.Read(tape), "E0012", "unknown read symbol");
      CheckSymbol(fields.Write(tape), "E0015", "unknown write symbol");
      const Token& move = fields.Move(tape);
      const auto head_move = ParseHeadMove(move.symbol.name());
      if (!head_move) {
        parsing::ReportError(
            diagnostics_,
            "E0019",
            "invalid move direction",
            move.span,
            "must be L, R, or S"
        );
      }
      rule.tapes.push_back(
          {.read = fields.Read(tape).symbol,
           .write = fields.Write(tape).symbol,
           .move = head_move.value_or(HeadMove::kStay)}
      );
    }
    return rule;
  }

 private:
  struct Fields {
    const std::vector<Token>& tokens;
    std::size_t num_tapes;
    const Token& from;
    const Token& to;

    [[nodiscard]] const Token& Read(std::size_t tape) const {
      return tokens[1 + tape];
    }

    [[nodiscard]] const Token& Write(std::size_t tape) const {
      return tokens[num_tapes + 2 + 2 * tape];
    }

    [[nodiscard]] const Token& Move(std::size_t tape) const {
      return tokens[num_tapes + 3 + 2 * tape];
    }
  };

  [[nodiscard]] Fields Split(const Line& line) const {
    return {
        .tokens = line.tokens,
        .num_tapes = num_tapes_,
        .from = line.tokens.front(),
        .to = line.tokens[num_tapes_ + 1]
    };
  }

  void CheckState(const Token& token, std::string code, std::string message) {
    if (states_.contains(token.symbol)) return;
    parsing::ReportError(
        diagnostics_,
        std::move(code),
        std::move(message),
        token.span,
        "state not in Q"
    );
  }

  void CheckSymbol(const Token& token, std::string code, std::string message) {
    if (tape_alphabet_.contains(token.symbol)) return;
    parsing::ReportError(
        diagnostics_,
        std::move(code),
        std::move(message),
        token.span,
        "symbol not in Γ"
    );
  }

  std::size_t num_tapes_;
  const SymbolSet& states_;
  const SymbolSet& tape_alphabet_;
  diag::DiagnosticSet& diagnostics_;
};

}  // namespace

ParseResult Parse(const diag::SourceFile& source) {
  auto [config, diagnostics] = ParseConfig(source);
  ParseResult result{
      .diagnostics = std::move(diagnostics),
      .machine = std::nullopt
  };
  parsing::Reader reader(source, result.diagnostics);
  const auto states = reader.ReadSet("Q");
  const auto input_alphabet = reader.ReadSet("Σ");
  const auto tape_alphabet = reader.ReadSet("Γ");

  const auto start =
      reader.ReadSingle("q0", "E0005", "invalid q0 line", "the start state");
  if (start && !states.contains(start->symbol)) {
    parsing::ReportError(
        result.diagnostics,
        "E0007",
        "start state not in Q",
        start->span,
        "this state is not listed in Q"
    );
  }
  const auto blank = reader.ReadSingle(
      "b",
      "E0006",
      "invalid blank symbol line",
      "the blank symbol"
  );
  if (blank && !tape_alphabet.contains(blank->symbol)) {
    parsing::ReportError(
        result.diagnostics,
        "E0008",
        "blank symbol not in Γ",
        blank->span,
        "this symbol is not listed in Γ"
    );
  }
  if (blank && input_alphabet.contains(blank->symbol)) {
    parsing::ReportError(
        result.diagnostics,
        "E0018",
        "blank symbol cannot be in Σ",
        blank->span,
        "this symbol is listed in Σ"
    );
  }

  RuleParser rules(config.num_tapes, states, tape_alphabet, result.diagnostics);
  auto accepting = reader.ReadAcceptingStates(
      states,
      [&](const Line& line) { return rules.Matches(line); },
      [&](const Line& line) { return line.tokens.size() == rules.Arity(); }
  );
  std::vector<Transition> transitions;
  for (const Line& line : reader.Rest()) {
    if (auto rule = rules.Parse(line)) transitions.push_back(*std::move(rule));
  }

  if (result.diagnostics.HasErrors() || !start || !blank) return result;
  auto machine = Machine::Create(
      {.config = config,
       .start_state = start->symbol,
       .blank = blank->symbol,
       .accepting_states = std::move(accepting).value_or(std::vector<Symbol>{}),
       .transitions = std::move(transitions)}
  );
  if (!machine) {
    parsing::ReportError(
        result.diagnostics,
        "E0017",
        "failed to build Turing machine",
        reader.LastSpan(),
        machine.error().message
    );
    return result;
  }
  result.machine = *std::move(machine);
  return result;
}

}  // namespace turing
