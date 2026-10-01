#include "npda/parser.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <expected>
#include <istream>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "npda/machine.h"
#include "parsing/lexer.h"

namespace npda::parse {

using automata::Symbol;
using lex::AddSimpleError;
using lex::AddSymbolError;
using lex::LexedDocument;
using lex::LineHasTokens;
using lex::NextNonempty;
using lex::ReadAll;
using lex::Token;
using lex::TokenLine;
using lex::ToSet;

namespace {

[[nodiscard]] bool IsEpsilonToken(std::string_view token) {
  return token == "-" || token == "." || token == "ε" || token == "λ" ||
         token == "eps" || token == "lambda" || token == "epsilon";
}

// One segmentation rule serves both optional-section detection and push
// parsing.
class StackAlphabet {
 public:
  explicit StackAlphabet(const std::unordered_set<Symbol>& symbols) {
    for (const auto symbol : symbols) {
      symbols_.emplace_back(symbol, automata::SymbolName(symbol));
    }
  }

  [[nodiscard]] std::optional<std::vector<Symbol>> Segment(
      Symbol symbol) const {
    const auto text = automata::SymbolName(symbol);
    if (IsEpsilonToken(text)) return std::vector<Symbol>{};
    for (const auto& [candidate, name] : symbols_) {
      if (name == text) return std::vector<Symbol>{candidate};
    }
    const auto unvisited = std::numeric_limits<std::size_t>::max();

    struct Position {
      std::size_t previous;
      Symbol symbol;
    };

    std::vector<Position> positions(text.size() + 1, {unvisited, {}});
    positions.front().previous = 0;
    for (std::size_t offset = 0; offset < text.size(); ++offset) {
      if (positions[offset].previous == unvisited) continue;
      for (const auto& [candidate, name] : symbols_) {
        const auto end = offset + name.size();
        if (!name.empty() && end <= text.size() &&
            positions[end].previous == unvisited &&
            text.substr(offset, name.size()) == name) {
          positions[end] = {offset, candidate};
        }
      }
    }
    if (positions.back().previous == unvisited) return std::nullopt;
    std::vector<Symbol> result;
    for (auto offset = text.size(); offset > 0;
         offset = positions[offset].previous) {
      result.push_back(positions[offset].symbol);
    }
    std::ranges::reverse(result);
    return result;
  }

 private:
  std::vector<std::pair<Symbol, std::string_view>> symbols_;
};

[[nodiscard]] bool LooksLikeTransition(
    const std::vector<Token>& tokens, const std::unordered_set<Symbol>& states,
    const std::unordered_set<Symbol>& input_alphabet,
    const std::unordered_set<Symbol>& stack_alphabet,
    const StackAlphabet& pushes) {
  if (tokens.size() < 4) return false;
  if (!states.contains(tokens[0].text) || !states.contains(tokens[3].text))
    return false;
  if (!IsEpsilonToken(automata::SymbolName(tokens[1].text)) &&
      !input_alphabet.contains(tokens[1].text))
    return false;
  if (!IsEpsilonToken(automata::SymbolName(tokens[2].text)) &&
      !stack_alphabet.contains(tokens[2].text))
    return false;
  for (std::size_t index = 4; index < tokens.size(); ++index) {
    if (!pushes.Segment(tokens[index].text)) return false;
  }
  return true;
}

}  // namespace

[[nodiscard]] ParseResult ParseWithDiagnostics(std::istream& is,
                                               std::string filename) {
  ParseResult out;
  out.source = diag::SourceFile::From(filename, ReadAll(is));
  const auto st = lex::Lex(out.source);
  const diag::SourceSpan eof = lex::EofSpan(out.source);

  diag::DiagnosticSet dx;

  std::size_t i = 0;

  // 1) Q (recoverable)
  std::vector<Token> state_tokens;
  const std::optional<std::size_t> states_line = NextNonempty(st, i);
  if (!lex::RequireLine(dx, states_line, "Q", eof)) {
    state_tokens = {};
  } else {
    state_tokens = st.lines[*states_line].tokens;
    i = *states_line + 1;
    if (state_tokens.empty()) {
      AddSimpleError(dx, "E0002", "empty set Q",
                     st.lines[*states_line].line_span,
                     "no states found on this line");
    }
  }

  // 2) Σ (recoverable)
  std::vector<Token> input_symbol_tokens;
  const std::optional<std::size_t> input_alphabet_line = NextNonempty(st, i);
  if (!lex::RequireLine(dx, input_alphabet_line, "Σ", eof)) {
    input_symbol_tokens = {};
  } else {
    input_symbol_tokens = st.lines[*input_alphabet_line].tokens;
    i = *input_alphabet_line + 1;
    if (input_symbol_tokens.empty()) {
      AddSimpleError(dx, "E0003", "empty alphabet Σ",
                     st.lines[*input_alphabet_line].line_span,
                     "no input symbols found");
    }
  }

  // 3) Γ (recoverable)
  std::vector<Token> stack_symbol_tokens;
  const std::optional<std::size_t> storage_alphabet_line = NextNonempty(st, i);
  if (!lex::RequireLine(dx, storage_alphabet_line, "Γ", eof)) {
    stack_symbol_tokens = {};
  } else {
    stack_symbol_tokens = st.lines[*storage_alphabet_line].tokens;
    i = *storage_alphabet_line + 1;
    if (stack_symbol_tokens.empty()) {
      AddSimpleError(dx, "E0004", "empty stack alphabet Γ",
                     st.lines[*storage_alphabet_line].line_span,
                     "no stack symbols found");
    }
  }

  // Build sets (may be empty after errors)
  const auto states = ToSet(state_tokens);
  const auto input_alphabet = ToSet(input_symbol_tokens);
  const auto stack_alphabet = ToSet(stack_symbol_tokens);
  const StackAlphabet pushes(stack_alphabet);

  // 4) q0 (recoverable)
  Token start_state_token{automata::Intern("<q0?>"), eof};
  const std::optional<std::size_t> start_state_line = NextNonempty(st, i);
  if (!lex::RequireLine(dx, start_state_line, "q0", eof)) {
    // keep default placeholder
  } else {
    const TokenLine& start_state_tokens = st.lines[*start_state_line];
    i = *start_state_line + 1;
    if (start_state_tokens.tokens.size() != 1) {
      AddSimpleError(dx, "E0005", "invalid q0 line",
                     start_state_tokens.line_span,
                     "expected exactly one token (the start state)");
      if (!start_state_tokens.tokens.empty())
        start_state_token = start_state_tokens.tokens.front();
    } else {
      start_state_token = start_state_tokens.tokens[0];
    }
    if (states.count(start_state_token.text) == 0) {
      AddSymbolError(dx, "E0007", "start state not in Q", start_state_token,
                     "this state is not listed in Q");
    }
  }

  // 5) Z0 (recoverable)
  Token bottom_symbol_token{automata::Intern("<Z0?>"), eof};
  const std::optional<std::size_t> i_z0 = NextNonempty(st, i);
  if (!lex::RequireLine(dx, i_z0, "Z0", eof)) {
    // keep default
  } else {
    const TokenLine& l_z0 = st.lines[*i_z0];
    i = *i_z0 + 1;
    if (l_z0.tokens.size() != 1) {
      AddSimpleError(dx, "E0006", "invalid Z0 line", l_z0.line_span,
                     "expected exactly one token (the bottom symbol)");
      if (!l_z0.tokens.empty()) bottom_symbol_token = l_z0.tokens.front();
    } else {
      bottom_symbol_token = l_z0.tokens[0];
    }
    if (stack_alphabet.count(bottom_symbol_token.text) == 0) {
      AddSymbolError(dx, "E0008", "stack bottom symbol not in Γ",
                     bottom_symbol_token, "this symbol is not listed in Γ");
    }
  }

  // 6) F (optional) with recovery
  std::vector<Token> accepting_state_tokens;
  bool has_accepting_states = false;
  const std::optional<std::size_t> accepting_states_line = NextNonempty(st, i);
  if (accepting_states_line.has_value()) {
    const TokenLine& accepting_tokens = st.lines[*accepting_states_line];
    // A line is an F line only if every token names a known state.
    // A transition-shaped line stays a transition, so typos in rules
    // report transition errors instead of F errors. Any other line is
    // neither: report its unknown tokens and skip it.
    const bool is_transition =
        LooksLikeTransition(accepting_tokens.tokens, states, input_alphabet,
                            stack_alphabet, pushes);
    const bool all_states = std::ranges::all_of(
        accepting_tokens.tokens,
        [&](const Token& t) { return states.contains(t.text); });
    if (!is_transition && all_states) {
      has_accepting_states = true;
      accepting_state_tokens = accepting_tokens.tokens;
      i = *accepting_states_line + 1;
    } else if (!is_transition) {
      // Short of transition arity: cannot be a rule. Report unknown
      // tokens and skip the line. Longer lines fall through to the
      // transitions loop, which reports per-field errors.
      if (accepting_tokens.tokens.size() < 4) {
        for (const auto& t : accepting_tokens.tokens) {
          if (states.count(t.text) == 0) {
            AddSymbolError(dx, "E0009", "invalid accepting states line", t,
                           "unknown state '" +
                               std::string(automata::SymbolName(t.text)) + "'",
                           "F must contain only states from Q");
          }
        }
        i = *accepting_states_line + 1;
      }
    }
  }

  // 7) Transitions with per-field recovery
  std::vector<Transition> rules;
  if (i < st.lines.size()) {
    rules.reserve(st.lines.size() - i);
  }

  for (std::size_t k = i; k < st.lines.size(); ++k) {
    const TokenLine& l = st.lines[k];
    if (!LineHasTokens(l)) continue;

    const auto& t = l.tokens;

    if (t.size() < 4) {
      AddSimpleError(dx, "E0010", "transition too short", l.line_span,
                     "expected: from input top to [push...]");
      continue;  // recover by skipping this line
    }

    // Fields
    const Token& from = t[0];
    const Token& in = t[1];
    const Token& top = t[2];
    const Token& to = t[3];

    // bad_in/bad_top reshape the rule below; unknown states are
    // hard errors and the rule is still recorded for diagnostics.
    bool bad_in = false;
    bool bad_top = false;

    if (states.count(from.text) == 0) {
      AddSymbolError(dx, "E0011", "unknown 'from' state", from,
                     "state not in Q");
    }
    const bool in_is_ok = IsEpsilonToken(automata::SymbolName(in.text)) ||
                          input_alphabet.count(in.text) > 0;
    if (!in_is_ok) {
      AddSymbolError(dx, "E0012", "unknown input symbol (or epsilon)", in,
                     "use symbol from Σ or 'eps' for epsilon");
      bad_in = true;
    }
    const bool top_is_ok = IsEpsilonToken(automata::SymbolName(top.text)) ||
                           stack_alphabet.count(top.text) > 0;
    if (!top_is_ok) {
      AddSymbolError(dx, "E0013", "unknown stack top symbol (or epsilon)", top,
                     "use symbol from Γ or 'eps' to ignore top");
      bad_top = true;
    }
    if (states.count(to.text) == 0) {
      AddSymbolError(dx, "E0014", "unknown 'to' state", to, "state not in Q");
    }

    Transition r;
    r.from_state = from.text;
    r.input = (bad_in || IsEpsilonToken(automata::SymbolName(in.text)))
                  ? std::nullopt
                  : std::optional<Symbol>(in.text);
    r.stack_top = (bad_top || IsEpsilonToken(automata::SymbolName(top.text)))
                      ? std::nullopt
                      : std::optional<Symbol>(top.text);
    r.to_state = to.text;

    if (t.size() > 4) r.push.reserve(t.size() - 4);
    for (std::size_t m = 4; m < t.size(); ++m) {
      const Token& ps = t[m];
      if (IsEpsilonToken(automata::SymbolName(ps.text))) continue;

      if (stack_alphabet.count(ps.text) > 0) {
        r.push.push_back(ps.text);
        continue;
      }

      if (auto parts = pushes.Segment(ps.text)) {
        // epsilon -> empty vector; concatenation -> parts
        for (auto& s : *parts) r.push.push_back(std::move(s));
      } else {
        AddSymbolError(dx, "E0015", "unknown push symbol", ps,
                       "push symbols must be from Γ or 'eps'");
        // drop invalid push chunk, keep others
      }
    }

    rules.push_back(std::move(r));
  }

  // Build NPDA regardless; we will suppress returning it if errors exist.
  Definition definition;
  definition.start_state = start_state_token.text;
  definition.stack_bottom = bottom_symbol_token.text;

  if (has_accepting_states && !accepting_state_tokens.empty()) {
    std::vector<Symbol> f;
    f.reserve(accepting_state_tokens.size());
    for (const auto& t : accepting_state_tokens) f.push_back(t.text);
    definition.acceptance = npda::AcceptancePolicy::kFinalState;
    definition.accepting_states = std::move(f);
  } else {
    // Recovery/default: accept by empty stack
    definition.acceptance = npda::AcceptancePolicy::kEmptyStack;
  }

  definition.transitions = std::move(rules);

  auto built = Machine::Create(std::move(definition));
  if (!built) {
    // Report as an error but still include prior diagnostics
    AddSimpleError(dx, "E0016", "failed to build NPDA",
                   st.lines.empty() ? eof : st.lines.back().line_span,
                   built.error().message);
  }

  if (dx.HasErrors()) {
    return {std::unexpected(std::move(dx)), std::move(out.source)};
  }

  return {std::expected<Machine, diag::DiagnosticSet>(*std::move(built)),
          std::move(out.source)};
}

}  // namespace npda::parse
