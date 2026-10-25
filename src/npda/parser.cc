#include "npda/parser.h"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "npda/definition.h"
#include "npda/transition.h"
#include "parsing/reader.h"

namespace npda {

namespace {

using parsing::Line;
using parsing::SymbolSet;
using parsing::Token;

bool IsEpsilon(Symbol symbol) {
  const std::string_view text = symbol.name();
  return text == "." || text == "-" || text == "ε" || text == "λ" ||
         text == "eps" || text == "epsilon" || text == "lambda";
}

/// The last symbol of one division of the start of a push token, and the
/// position where that symbol starts.
struct Piece {
  std::size_t start;
  Symbol symbol;
};

/// Divides a push token such as `AAS` into Γ symbols.
///
/// Because Γ symbols can have more than one character, the division must cover
/// the whole token. A token that already names a symbol stays whole.
std::optional<std::vector<Symbol>> SplitPush(
    Symbol token,
    const SymbolSet& alphabet
) {
  if (IsEpsilon(token)) return std::vector<Symbol>{};
  if (alphabet.contains(token)) return std::vector{token};
  const std::string_view text = token.name();
  std::vector<std::optional<Piece>> pieces(text.size() + 1);
  for (std::size_t start = 0; start < text.size(); ++start) {
    if (start > 0 && !pieces[start]) continue;
    for (const auto symbol : alphabet) {
      const std::string_view name = symbol.name();
      const std::size_t end = start + name.size();
      if (end > text.size() || pieces[end] ||
          !text.substr(start).starts_with(name)) {
        continue;
      }
      pieces[end] = Piece{start, symbol};
    }
  }
  if (!pieces.back()) return std::nullopt;
  std::vector<Symbol> symbols;
  for (std::size_t end = text.size(); end > 0; end = pieces[end]->start) {
    symbols.push_back(pieces[end]->symbol);
  }
  std::ranges::reverse(symbols);
  return symbols;
}

class TransitionParser {
 public:
  TransitionParser(
      const SymbolSet& states,
      const SymbolSet& input_alphabet,
      const SymbolSet& stack_alphabet,
      diag::DiagnosticSet& diagnostics
  )
      : states_(states),
        input_alphabet_(input_alphabet),
        stack_alphabet_(stack_alphabet),
        diagnostics_(diagnostics) {}

  /// Checks whether the line is a valid transition without reporting errors.
  [[nodiscard]] bool Matches(const Line& line) const {
    const auto& tokens = line.tokens;
    return tokens.size() >= 4 && states_.contains(tokens[0].symbol) &&
           IsInputSymbol(tokens[1].symbol) &&
           stack_alphabet_.contains(tokens[2].symbol) &&
           states_.contains(tokens[3].symbol) &&
           std::ranges::all_of(
               tokens | std::views::drop(4),
               [&](const Token& token) {
                 return SplitPush(token.symbol, stack_alphabet_).has_value();
               }
           );
  }

  /// Reads `from input top to push` as `(to, push) ∈ δ(from, input, top)` and
  /// reports each unknown field. The result is correct only if the function
  /// reported nothing.
  std::optional<Transition> Parse(const Line& line) {
    const auto& tokens = line.tokens;
    if (tokens.size() < 4) {
      parsing::ReportError(
          diagnostics_,
          "E0010",
          "transition too short",
          line.span,
          "expected: from input top to [push...]"
      );
      return std::nullopt;
    }
    const Token& from = tokens[0];
    const Token& input = tokens[1];
    const Token& top = tokens[2];
    const Token& to = tokens[3];
    if (!states_.contains(from.symbol)) {
      Report(from, "E0011", "unknown 'from' state", "state not in Q");
    }
    if (!IsInputSymbol(input.symbol)) {
      Report(
          input,
          "E0012",
          "unknown input symbol",
          "use a symbol from Σ, or '.' for ε"
      );
    }
    if (!stack_alphabet_.contains(top.symbol)) {
      Report(
          top,
          "E0013",
          "unknown stack top symbol",
          "every move replaces a symbol from Γ"
      );
    }
    if (!states_.contains(to.symbol)) {
      Report(to, "E0014", "unknown 'to' state", "state not in Q");
    }
    std::vector<Symbol> push;
    for (const Token& token : tokens | std::views::drop(4)) {
      if (const auto symbols = SplitPush(token.symbol, stack_alphabet_)) {
        push.insert(push.end(), symbols->begin(), symbols->end());
      } else {
        Report(
            token,
            "E0015",
            "unknown push symbol",
            "push symbols from Γ, or '.' for ε"
        );
      }
    }
    return Transition{
        .from = from.symbol,
        .input = IsEpsilon(input.symbol) ? std::nullopt
                                         : std::optional(input.symbol),
        .top = top.symbol,
        .to = to.symbol,
        .push = std::move(push)
    };
  }

 private:
  [[nodiscard]] bool IsInputSymbol(Symbol symbol) const {
    return IsEpsilon(symbol) || input_alphabet_.contains(symbol);
  }

  void Report(
      const Token& token,
      std::string_view code,
      std::string_view message,
      std::string_view label
  ) {
    parsing::ReportError(
        diagnostics_,
        std::string(code),
        std::string(message),
        token.span,
        std::string(label)
    );
  }

  const SymbolSet& states_;
  const SymbolSet& input_alphabet_;
  const SymbolSet& stack_alphabet_;
  diag::DiagnosticSet& diagnostics_;
};

}  // namespace

ParseResult Parse(const diag::SourceFile& source) {
  ParseResult result;
  parsing::Reader reader(source, result.diagnostics);
  const auto states = reader.ReadSet("Q");
  const auto input_alphabet = reader.ReadSet("Σ");
  const auto stack_alphabet = reader.ReadSet("Γ");

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
  const auto initial = reader.ReadSingle(
      "Z0",
      "E0006",
      "invalid Z0 line",
      "the initial stack symbol"
  );
  if (initial && !stack_alphabet.contains(initial->symbol)) {
    parsing::ReportError(
        result.diagnostics,
        "E0008",
        "initial stack symbol not in Γ",
        initial->span,
        "this symbol is not listed in Γ"
    );
  }

  TransitionParser
      transitions(states, input_alphabet, stack_alphabet, result.diagnostics);
  auto accepting = reader.ReadAcceptingStates(
      states,
      [&](const Line& line) { return transitions.Matches(line); },
      [](const Line& line) { return line.tokens.size() >= 4; }
  );
  std::vector<Transition> rules;
  for (const Line& line : reader.Rest()) {
    if (auto rule = transitions.Parse(line)) rules.push_back(*std::move(rule));
  }

  if (result.diagnostics.HasErrors() || !start || !initial) return result;
  Acceptance acceptance = AcceptByEmptyStack{};
  if (accepting) acceptance = AcceptByFinalState{*std::move(accepting)};
  result.machine.emplace(
      Definition{
          .start_state = start->symbol,
          .initial_stack_symbol = initial->symbol,
          .acceptance = std::move(acceptance),
          .transitions = std::move(rules)
      }
  );
  return result;
}

}  // namespace npda
