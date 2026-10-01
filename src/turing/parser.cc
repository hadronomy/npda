#include "turing/parser.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <expected>
#include <istream>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "parsing/lexer.h"
#include "turing/config_parser.h"
#include "turing/machine.h"

namespace turing::parse {

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

[[nodiscard]] bool LooksLikeSingleTapeTransition(
    const std::vector<Token>& tokens, const std::unordered_set<Symbol>& q,
    const std::unordered_set<Symbol>& g) {
  if (tokens.size() != 5) return false;

  const auto& from = tokens[0];
  const auto& read = tokens[1];
  const auto& to = tokens[2];
  const auto& write = tokens[3];
  const auto& move = tokens[4];
  const std::string_view move_text = automata::SymbolName(move.text);

  if (q.count(from.text) == 0) return false;
  if (g.count(read.text) == 0) return false;  // Γ
  if (q.count(to.text) == 0) return false;
  if (g.count(write.text) == 0) return false;
  if (move_text != "L" && move_text != "R" && move_text != "S") return false;

  return true;
}

[[nodiscard]] bool LooksLikeMultiTapeTransition(
    const std::vector<Token>& tokens, const std::unordered_set<Symbol>& q,
    const std::unordered_set<Symbol>& g, std::size_t num_tapes) {
  // Expect: from read1 ... readN to write1 move1 write2 move2 ... writeN moveN
  const std::size_t expected_size = 2 + num_tapes * 3;
  if (tokens.size() != expected_size) return false;

  const auto& from = tokens[0];
  if (q.count(from.text) == 0) return false;

  const std::size_t to_idx = 1 + num_tapes;
  if (to_idx >= tokens.size()) return false;
  const auto& to = tokens[to_idx];
  if (q.count(to.text) == 0) return false;

  // read symbols
  for (std::size_t i = 0; i < num_tapes; ++i) {
    if (1 + i >= tokens.size()) return false;
    if (g.count(tokens[1 + i].text) == 0) return false;
  }

  // write/move pairs
  for (std::size_t i = 0; i < num_tapes; ++i) {
    const std::size_t pair_base = 1 + num_tapes + 1 + 2 * i;
    if (pair_base + 1 >= tokens.size()) return false;
    const auto& write = tokens[pair_base];
    const auto& move = tokens[pair_base + 1];
    const std::string_view move_text = automata::SymbolName(move.text);
    if (g.count(write.text) == 0) return false;
    if (move_text != "L" && move_text != "R" && move_text != "S") return false;
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
  MachineConfig config;

  // Parse structured configuration
  auto config_result = ParseConfiguration(out.source);
  config = config_result.config;

  // Accumulate configuration diagnostics
  for (auto& diag : config_result.diagnostics.items) {
    dx.items.push_back(std::move(diag));
  }

  // 1) Q (states)
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

  // 2) Σ (input alphabet)
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

  // 3) Γ (tape alphabet)
  std::vector<Token> tape_symbol_tokens;
  const std::optional<std::size_t> storage_alphabet_line = NextNonempty(st, i);
  if (!lex::RequireLine(dx, storage_alphabet_line, "Γ", eof)) {
    tape_symbol_tokens = {};
  } else {
    tape_symbol_tokens = st.lines[*storage_alphabet_line].tokens;
    i = *storage_alphabet_line + 1;
    if (tape_symbol_tokens.empty()) {
      AddSimpleError(dx, "E0004", "empty tape alphabet Γ",
                     st.lines[*storage_alphabet_line].line_span,
                     "no tape symbols found");
    }
  }

  // Build sets
  const auto states = ToSet(state_tokens);
  const auto input_alphabet = ToSet(input_symbol_tokens);
  const auto tape_alphabet = ToSet(tape_symbol_tokens);

  // 4) q0 (start state)
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

  // 5) b (blank symbol)
  Token blank_symbol_token{automata::Intern("<b?>"), eof};
  const std::optional<std::size_t> blank_symbol_line = NextNonempty(st, i);
  if (!lex::RequireLine(dx, blank_symbol_line, "b", eof)) {
    // keep default
  } else {
    const TokenLine& blank_tokens = st.lines[*blank_symbol_line];
    i = *blank_symbol_line + 1;
    if (blank_tokens.tokens.size() != 1) {
      AddSimpleError(dx, "E0006", "invalid blank symbol line",
                     blank_tokens.line_span,
                     "expected exactly one token (the blank symbol)");
      if (!blank_tokens.tokens.empty())
        blank_symbol_token = blank_tokens.tokens.front();
    } else {
      blank_symbol_token = blank_tokens.tokens[0];
    }
    if (tape_alphabet.count(blank_symbol_token.text) == 0) {
      AddSymbolError(dx, "E0008", "blank symbol not in Γ", blank_symbol_token,
                     "this symbol is not listed in Γ");
    }
    if (input_alphabet.count(blank_symbol_token.text) == 1) {
      AddSymbolError(dx, "E0018", "blank symbol cannot be in Σ",
                     blank_symbol_token, "this symbol is listed in Σ");
    }
  }

  // 6) F (accepting states) - optional
  std::vector<Token> accepting_state_tokens;
  bool has_accepting_states = false;
  const std::optional<std::size_t> accepting_states_line = NextNonempty(st, i);
  if (accepting_states_line.has_value()) {
    const TokenLine& accepting_tokens = st.lines[*accepting_states_line];

    // Check if this looks like transitions or accepting states
    bool is_transition = false;
    if (config.num_tapes == 1) {
      is_transition = LooksLikeSingleTapeTransition(accepting_tokens.tokens,
                                                    states, tape_alphabet);
    } else {
      is_transition = LooksLikeMultiTapeTransition(
          accepting_tokens.tokens, states, tape_alphabet, config.num_tapes);
    }

    if (!is_transition) {
      // A line is an F line only if every token names a known state.
      // A transition-shaped line stays a transition, so typos in rules
      // report transition errors instead of F errors. Any other line is
      // neither: report its unknown tokens and skip it.
      const bool all_states = std::ranges::all_of(
          accepting_tokens.tokens,
          [&](const Token& t) { return states.contains(t.text); });
      if (all_states) {
        has_accepting_states = true;
        accepting_state_tokens = accepting_tokens.tokens;
        i = *accepting_states_line + 1;
      } else {
        // Short of transition arity: cannot be a rule. Report unknown
        // tokens and skip the line. Longer lines fall through to the
        // transitions loop, which reports per-field errors.
        const std::size_t arity =
            (config.num_tapes == 1) ? 5 : 2 + config.num_tapes * 3;
        if (accepting_tokens.tokens.size() != arity) {
          for (const auto& t : accepting_tokens.tokens) {
            if (states.count(t.text) == 0) {
              AddSymbolError(dx, "E0009", "invalid accepting states line", t,
                             "unknown state '" +
                                 std::string(automata::SymbolName(t.text)) +
                                 "'",
                             "F must contain only states from Q");
            }
          }
          i = *accepting_states_line + 1;
        }
      }
    }
  }

  // 7) Transitions
  std::vector<Transition> rules;
  if (i < st.lines.size()) {
    rules.reserve(st.lines.size() - i);
  }

  for (std::size_t k = i; k < st.lines.size(); ++k) {
    const TokenLine& l = st.lines[k];
    if (!LineHasTokens(l)) continue;

    const auto& t = l.tokens;

    if (config.num_tapes == 1) {
      // Single tape transition: from read to write move
      if (t.size() != 5) {
        AddSimpleError(dx, "E0010", "transition must have exactly 5 parts",
                       l.line_span, "expected: from read to write move");
        continue;  // recover by skipping this line
      }

      const Token& from = t[0];
      const Token& read = t[1];
      const Token& to = t[2];
      const Token& write = t[3];
      const Token& move = t[4];
      const std::string_view move_text = automata::SymbolName(move.text);

      // bad_* reshape nothing here: unknown fields are hard errors
      // and the rule is still recorded for diagnostics.
      if (states.count(from.text) == 0) {
        AddSymbolError(dx, "E0011", "unknown 'from' state", from,
                       "state not in Q");
      }
      if (tape_alphabet.count(read.text) == 0) {
        AddSymbolError(dx, "E0012", "unknown read symbol", read,
                       "symbol not in Γ");
      }
      if (states.count(to.text) == 0) {
        AddSymbolError(dx, "E0014", "unknown 'to' state", to, "state not in Q");
      }
      if (tape_alphabet.count(write.text) == 0) {
        AddSymbolError(dx, "E0015", "unknown write symbol", write,
                       "symbol not in Γ");
      }
      if (move_text != "L" && move_text != "R" && move_text != "S") {
        AddSymbolError(dx, "E0019", "invalid move direction", move,
                       "must be L, R, or S");
      }

      // Build arity-1 multi rule (even for invalids to aid recovery)
      Direction dir = Direction::kStay;
      if (const auto d = FromChar(move_text)) {
        dir = *d;
      }

      Transition rule;
      rule.from_state = from.text;
      rule.to_state = to.text;
      rule.tapes = {{read.text, write.text, dir}};

      rules.push_back(std::move(rule));
    } else {
      // Multi-tape transition
      const std::size_t expected_size = 2 + config.num_tapes * 3;
      if (t.size() != expected_size) {
        AddSimpleError(
            dx, "E0010", "multi-tape transition must have correct format",
            l.line_span,
            "expected: from read1 ... readN to write1 move1 ... writeN moveN");
        continue;
      }

      const Token& from = t[0];
      if (states.count(from.text) == 0) {
        AddSymbolError(dx, "E0011", "unknown 'from' state", from,
                       "state not in Q");
        continue;
      }

      const std::size_t to_idx = 1 + config.num_tapes;
      const Token& to = t[to_idx];
      if (states.count(to.text) == 0) {
        AddSymbolError(dx, "E0014", "unknown 'to' state", to, "state not in Q");
        continue;
      }

      Transition rule;
      rule.from_state = from.text;
      rule.to_state = to.text;
      rule.tapes.reserve(config.num_tapes);
      std::vector<Symbol> reads(config.num_tapes);

      bool valid = true;

      // Parse read symbols
      for (std::size_t tape_idx = 0; tape_idx < config.num_tapes; ++tape_idx) {
        const std::size_t read_idx = 1 + tape_idx;
        if (read_idx >= t.size()) {
          valid = false;
          break;
        }
        const Token& read = t[read_idx];
        if (tape_alphabet.count(read.text) == 0) {
          AddSymbolError(dx, "E0012", "unknown read symbol", read,
                         "symbol not in Γ");
          valid = false;
        } else {
          reads[tape_idx] = read.text;
        }
      }

      // Parse (write_i, move_i) pairs after 'to'
      for (std::size_t tape_idx = 0; tape_idx < config.num_tapes; ++tape_idx) {
        const std::size_t pair_base = 1 + config.num_tapes + 1 + 2 * tape_idx;
        if (pair_base + 1 >= t.size()) {
          valid = false;
          break;
        }
        const Token& write = t[pair_base];
        const Token& move = t[pair_base + 1];
        const std::string_view move_text = automata::SymbolName(move.text);

        Symbol write_sym{};
        if (tape_alphabet.count(write.text) == 0) {
          AddSymbolError(dx, "E0015", "unknown write symbol", write,
                         "symbol not in Γ");
          valid = false;
        } else {
          write_sym = write.text;
        }

        Direction dir = Direction::kStay;
        if (const auto d = FromChar(move_text)) {
          dir = *d;
        } else {
          AddSymbolError(dx, "E0019", "invalid move direction", move,
                         "must be L, R, or S");
          valid = false;
        }
        rule.tapes.push_back({reads[tape_idx], write_sym, dir});
      }

      if (valid) {
        rules.push_back(std::move(rule));
      }
    }
  }

  // Build Turing Machine regardless; suppress returning it if errors exist
  Definition definition;
  definition.config = config;
  definition.start_state = start_state_token.text;
  definition.blank_symbol = blank_symbol_token.text;

  if (has_accepting_states && !accepting_state_tokens.empty()) {
    std::vector<Symbol> f;
    f.reserve(accepting_state_tokens.size());
    for (const auto& t : accepting_state_tokens) f.push_back(t.text);
    definition.accepting_states = std::move(f);
  }

  definition.transitions = std::move(rules);

  auto built = Machine::Create(std::move(definition));
  if (!built) {
    // Report as an error but still include prior diagnostics
    AddSimpleError(dx, "E0017", "failed to build Turing Machine",
                   st.lines.empty() ? eof : st.lines.back().line_span,
                   built.error().message);
  }

  if (dx.HasErrors()) {
    return {std::unexpected(std::move(dx)), std::move(out.source), {}};
  }
  return {std::expected<Machine, diag::DiagnosticSet>(std::move(*built)),
          std::move(out.source), std::move(dx)};
}

}  // namespace turing::parse
