#include "turing/parser.h"

#include <algorithm>
#include <charconv>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_map>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "parsing/reader.h"
#include "turing/config.h"
#include "turing/config_parser.h"
#include "turing/machine.h"
#include "turing/transition.h"
#include "turing/transition_text.h"

namespace turing {

namespace {

using parsing::Line;
using parsing::SymbolSet;
using parsing::Token;

constexpr std::string_view kTapesSection = "number of tapes";

/// True if `text` is one character. Inputs split into characters, so a longer
/// tape symbol could never come from an input.
bool IsOneCharacter(std::string_view text) {
  return automata::InputSymbols(text).size() == 1;
}

std::optional<std::size_t> ParseCount(std::string_view text) {
  std::size_t count = 0;
  const auto* end = text.data() + text.size();
  const auto [last, error] = std::from_chars(text.data(), end, count);
  if (error != std::errc() || last != end || count == 0) return std::nullopt;
  return count;
}

/// True if `line` contains a single positive integer that is not a state,
/// identifying the number of tapes when the file omits F because it is empty.
bool IsTapeCountLine(const Line& line, const SymbolSet& states) {
  return line.tokens.size() == 1 &&
         ParseCount(line.tokens.front().symbol.name()) &&
         !states.contains(line.tokens.front().symbol);
}

std::optional<std::size_t> ReadTapeCount(
    parsing::Reader& reader,
    diag::DiagnosticSet& diagnostics
) {
  const Line* line = reader.Peek();
  const auto token = reader.ReadSingle(
      kTapesSection,
      "E0022",
      "invalid number of tapes line",
      "the number of tapes"
  );
  if (!token || line->tokens.size() != 1) return std::nullopt;
  const auto count = ParseCount(token->symbol.name());
  if (!count) {
    parsing::ReportError(
        diagnostics,
        "E0022",
        "invalid number of tapes line",
        token->span,
        "expected a positive integer"
    );
  }
  return count;
}

/// Reads the Σ line and reports each symbol outside Γ. Because Γ comes on the
/// next line, the caller supplies it through `tape_alphabet`.
std::vector<Symbol> CheckInputAlphabet(
    const Line* line,
    const SymbolSet& tape_alphabet,
    diag::DiagnosticSet& diagnostics
) {
  std::vector<Symbol> alphabet;
  if (line == nullptr) return alphabet;
  for (const auto& token : line->tokens) {
    if (!tape_alphabet.contains(token.symbol)) {
      parsing::ReportError(
          diagnostics,
          "E0021",
          "input symbol not in Γ",
          token.span,
          "this symbol is not listed in Γ"
      );
    }
    if (!std::ranges::contains(alphabet, token.symbol)) {
      alphabet.push_back(token.symbol);
    }
  }
  return alphabet;
}

/// Reports tape symbols longer than one character and, in the independent
/// mode, symbols that a transition can read as a move.
void CheckTapeAlphabet(
    const Line* line,
    const Config& config,
    diag::DiagnosticSet& diagnostics
) {
  if (line == nullptr) return;
  for (const auto& token : line->tokens) {
    const auto name = token.symbol.name();
    if (!IsOneCharacter(name)) {
      parsing::ReportError(
          diagnostics,
          "E0020",
          "tape symbol must be one character",
          token.span,
          "write one character"
      );
    }
    const auto move = ParseHeadMove(name);
    const bool is_move =
        move && (*move != HeadMove::kStay || config.allow_stay);
    if (config.operation_mode == OperationMode::kIndependent && is_move) {
      diag::Error("E0024", "tape symbol is also a move")
          .Label(token.span, "a transition would read this symbol as a move")
          .Note(
              "In the independent mode, an action is a symbol or a move. "
              "Rename the symbol, or set operation_mode = simultaneous."
          )
          .Emit(diagnostics);
    }
  }
}

/// Reads transitions for a machine with N tapes. Each transition starts with
/// `from read1 .. readN to`, followed by one action per tape. In the
/// simultaneous mode, each action combines a write and a move:
///
/// ```text
/// from  read1 .. readN  to  write1 move1 .. writeN moveN
/// ```
///
/// In the independent mode, the action is a write or a move:
///
/// ```text
/// from  read1 .. readN  to  action1 .. actionN
/// ```
///
/// With one tape, the simultaneous form is the usual `from read to write move`.
class RuleParser {
 public:
  RuleParser(
      std::size_t num_tapes,
      const Config& config,
      const SymbolSet& states,
      const SymbolSet& tape_alphabet,
      diag::DiagnosticSet& diagnostics
  )
      : num_tapes_(num_tapes),
        config_(config),
        states_(states),
        tape_alphabet_(tape_alphabet),
        diagnostics_(diagnostics) {}

  /// Reads one transition and reports each invalid field. The result is
  /// meaningful only if the call reported nothing.
  std::optional<Transition> Parse(const Line& line) {
    if (line.tokens.size() != Arity()) {
      diag::Error(
          "E0010",
          std::format("transition must have {} parts", Arity())
      )
          .Label(line.span, std::format("expected: {}", Shape()))
          .Emit(diagnostics_);
      return std::nullopt;
    }
    const auto& tokens = line.tokens;
    const Token& from = tokens.front();
    const Token& to = tokens[num_tapes_ + 1];
    CheckState(from, "E0011", "unknown 'from' state");
    CheckState(to, "E0014", "unknown 'to' state");
    Transition rule{.from = from.symbol, .tapes = {}, .to = to.symbol};
    for (std::size_t tape = 0; tape < num_tapes_; ++tape) {
      const Token& read = tokens[1 + tape];
      CheckSymbol(read, "E0012", "unknown read symbol");
      rule.tapes.push_back(
          config_.operation_mode == OperationMode::kSimultaneous
              ? ParseWriteAndMove(
                    read.symbol,
                    tokens[num_tapes_ + 2 + 2 * tape],
                    tokens[num_tapes_ + 3 + 2 * tape]
                )
              : ParseAction(read.symbol, tokens[num_tapes_ + 2 + tape])
      );
    }
    return rule;
  }

 private:
  [[nodiscard]] std::size_t Arity() const {
    const std::size_t per_tape =
        config_.operation_mode == OperationMode::kSimultaneous ? 3 : 2;
    return per_tape * num_tapes_ + 2;
  }

  [[nodiscard]] std::string_view Shape() const {
    const bool one = num_tapes_ == 1;
    if (config_.operation_mode == OperationMode::kSimultaneous) {
      return one ? "from read to write move"
                 : "from read1 .. readN to write1 move1 .. writeN moveN";
    }
    return one ? "from read to action"
               : "from read1 .. readN to action1 .. actionN";
  }

  [[nodiscard]] std::string_view Moves() const {
    return config_.allow_stay ? "L, R, or S" : "L or R";
  }

  TapeAction ParseWriteAndMove(
      Symbol read,
      const Token& write,
      const Token& move
  ) {
    CheckSymbol(write, "E0015", "unknown write symbol");
    const auto head_move = ParseHeadMove(move.symbol.name());
    if (!head_move) {
      parsing::ReportError(
          diagnostics_,
          "E0019",
          "invalid move direction",
          move.span,
          std::format("must be {}", Moves())
      );
    } else if (*head_move == HeadMove::kStay) {
      CheckStayAllowed(move);
    }
    return {
        .read = read,
        .write = write.symbol,
        .move = head_move.value_or(HeadMove::kStay)
    };
  }

  /// An action writes a symbol from Γ or moves the head. Because Γ excludes
  /// move letters in this mode, the two tests can run in either order.
  TapeAction ParseAction(Symbol read, const Token& action) {
    if (tape_alphabet_.contains(action.symbol)) {
      return {.read = read, .write = action.symbol, .move = HeadMove::kStay};
    }
    const auto head_move = ParseHeadMove(action.symbol.name());
    if (!head_move) {
      diag::Error("E0025", "invalid tape action")
          .Label(action.span, "not a Γ symbol or a move")
          .Note(
              std::format(
                  "An action writes a Γ symbol or moves the head: {}.",
                  Moves()
              )
          )
          .Emit(diagnostics_);
    } else if (*head_move == HeadMove::kStay) {
      CheckStayAllowed(action);
    }
    return {
        .read = read,
        .write = read,
        .move = head_move.value_or(HeadMove::kStay)
    };
  }

  void CheckStayAllowed(const Token& move) {
    if (config_.allow_stay) return;
    parsing::ReportError(
        diagnostics_,
        "E0023",
        "move not allowed",
        move.span,
        "the config sets allow_stay = false"
    );
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
  const Config& config_;
  const SymbolSet& states_;
  const SymbolSet& tape_alphabet_;
  diag::DiagnosticSet& diagnostics_;
};

/// Reports each rule that reads the same symbols in the same state as an
/// earlier rule. `lines[i]` is the line of `transitions[i]`.
void CheckDeterminism(
    const std::vector<Transition>& transitions,
    const std::vector<const Line*>& lines,
    diag::DiagnosticSet& diagnostics
) {
  std::unordered_map<std::string, std::size_t> first_rule;
  for (std::size_t index = 0; index < transitions.size(); ++index) {
    const auto& rule = transitions[index];
    // Tokens have no spaces, so a space cannot make two keys equal.
    const auto key =
        std::format("{} {}", rule.from, JoinReads(rule.tapes, " "));
    const auto [earlier, inserted] = first_rule.try_emplace(key, index);
    if (inserted) continue;
    diag::Error("E0016", "nondeterministic transitions")
        .Label(
            lines[index]->span,
            std::format(
                "δ{} already reads ({}) in state {}",
                earlier->second + 1,
                JoinReads(rule.tapes, ", "),
                rule.from
            )
        )
        .Note("A state can have only one transition for each read.")
        .Emit(diagnostics);
  }
}

}  // namespace

ParseResult Parse(const diag::SourceFile& source) {
  auto [config, diagnostics] = ParseConfig(source);
  ParseResult result{
      .diagnostics = std::move(diagnostics),
      .machine = std::nullopt
  };
  parsing::Reader reader(source, result.diagnostics);
  const auto states = reader.ReadSet("Q");
  const Line* input_line = reader.Peek();
  reader.ReadSet("Σ");
  const Line* tape_line = reader.Peek();
  const auto tape_alphabet = reader.ReadSet("Γ");
  const auto input_alphabet =
      CheckInputAlphabet(input_line, tape_alphabet, result.diagnostics);
  CheckTapeAlphabet(tape_line, config, result.diagnostics);

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
  if (blank && std::ranges::contains(input_alphabet, blank->symbol)) {
    parsing::ReportError(
        result.diagnostics,
        "E0018",
        "blank symbol cannot be in Σ",
        blank->span,
        "this symbol is listed in Σ"
    );
  }

  std::vector<Symbol> accepting;
  if (const Line* line = reader.Peek();
      line != nullptr && !IsTapeCountLine(*line, states)) {
    accepting = reader.ReadAcceptingLine(states);
  }
  const auto num_tapes = ReadTapeCount(reader, result.diagnostics);
  // Without the number of tapes, the length of a rule is not known.
  if (!num_tapes) return result;

  RuleParser
      rules(*num_tapes, config, states, tape_alphabet, result.diagnostics);
  std::vector<Transition> transitions;
  std::vector<const Line*> rule_lines;
  for (const Line& line : reader.Rest()) {
    if (auto rule = rules.Parse(line)) {
      transitions.push_back(*std::move(rule));
      rule_lines.push_back(&line);
    }
  }
  CheckDeterminism(transitions, rule_lines, result.diagnostics);

  if (result.diagnostics.HasErrors() || !start || !blank) return result;
  auto machine = Machine::Create({
      .config = config,
      .num_tapes = *num_tapes,
      .input_alphabet = input_alphabet,
      .start_state = start->symbol,
      .blank = blank->symbol,
      .accepting_states = std::move(accepting),
      .transitions = std::move(transitions),
  });
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
