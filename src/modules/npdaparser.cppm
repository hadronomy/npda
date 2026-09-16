// Own the NPDA text format parser.
// C headers stay in the global fragment above the module line.
// Import modules below it. Never place a C include below the imports.
module;
#include <cctype>
#include <cstdio>

export module npda.parser;
import std;
import diag;
import lex;
import npda;
// C++23. Parser with rustc-style diagnostics and error recovery.

export namespace npda::parse {

// Shared lexer entities (defined in the lex module).
using lex::Token;
using lex::Line;
using lex::SpecTokens;
using lex::read_all;
using lex::add_simple_error;
using lex::add_symbol_error;
using lex::line_has_tokens;
using lex::next_nonempty;
using lex::to_set;
using lex::Symbol;

using PDA = npda::NPDA<Symbol, Symbol, Symbol>;
using Rule = npda::Rule<Symbol, Symbol, Symbol>;

struct ParseResult {
  std::expected<PDA, diag::Diagnostics> value;
  diag::SourceFile source;
};

[[nodiscard]] inline bool is_eps_tok(std::string_view s) {
  // Accept multiple epsilon spellings, including "."
  switch (s.size()) {
    case 1:
      return s == "-" || s == "ε" || s == "λ" || s == ".";
    case 3:
      return s == "eps";
    case 6:
      return s == "lambda";
    case 7:
      return s == "epsilon";
    default:
      return false;
  }
}

[[nodiscard]] inline bool looks_like_transition(
  const std::vector<Token>& toks,
  const std::unordered_set<Symbol>& Q,
  const std::unordered_set<Symbol>& S,
  const std::unordered_set<Symbol>& G
) {
  if (toks.size() < 4)
    return false;

  const auto& from = toks[0];
  const auto& in = toks[1];
  const auto& top = toks[2];
  const auto& to = toks[3];

  const auto in_ok = is_eps_tok(lex::name(in.text)) || S.count(in.text) > 0;
  const auto top_ok = is_eps_tok(lex::name(top.text)) || G.count(top.text) > 0;

  if (Q.count(from.text) == 0)
    return false;
  if (!in_ok)
    return false;
  if (!top_ok)
    return false;
  if (Q.count(to.text) == 0)
    return false;

  // Views of Γ for segmentation probes. The table owns the bytes,
  // and no interning happens here, so the views stay valid.
  std::vector<std::string_view> gsyms;
  gsyms.reserve(G.size());
  for (const Symbol& g : G)
    gsyms.push_back(lex::name(g));

  // Allow concatenated push strings like "AS" if they can be segmented
  // entirely into Γ symbols.
  const auto can_segment = [&](Symbol sym) -> bool {
    const std::string_view w = lex::name(sym);
    if (is_eps_tok(w))
      return true;
    for (std::string_view g : gsyms)
      if (g == w)
        return true;
    const std::size_t n = w.size();
    if (n == 0)
      return true;
    std::vector<char> dp(n + 1, 0);
    dp[0] = 1;
    for (std::size_t i = 0; i < n; ++i) {
      if (!dp[i])
        continue;
      for (std::string_view g : gsyms) {
        const std::size_t len = g.size();
        if (i + len <= n && w.substr(i, len) == g) {
          dp[i + len] = 1;
        }
      }
    }
    return dp[n] != 0;
  };

  for (std::size_t i = 4; i < toks.size(); ++i) {
    if (!can_segment(toks[i].text))
      return false;
  }
  return true;
}

[[nodiscard]] inline ParseResult parse_with_diagnostics(std::istream& is, std::string filename) {
  ParseResult out;
  out.source = diag::SourceFile::from(filename, read_all(is));
  const auto st = lex::lex(out.source);
  const diag::Span eof = lex::eof_span(out.source);

  diag::Diagnostics dx;

  std::size_t i = 0;

  // 1) Q (recoverable)
  std::vector<Token> QToks;
  const std::optional<std::size_t> iQ = next_nonempty(st, i);
  if (!lex::require_line(dx, iQ, "Q", eof)) {
    QToks = {};
  } else {
    QToks = st.lines[*iQ].tokens;
    i = *iQ + 1;
    if (QToks.empty()) {
      add_simple_error(
        dx, "E0002", "empty set Q", st.lines[*iQ].line_span, "no states found on this line"
      );
    }
  }

  // 2) Σ (recoverable)
  std::vector<Token> SToks;
  const std::optional<std::size_t> iS = next_nonempty(st, i);
  if (!lex::require_line(dx, iS, "Σ", eof)) {
    SToks = {};
  } else {
    SToks = st.lines[*iS].tokens;
    i = *iS + 1;
    if (SToks.empty()) {
      add_simple_error(
        dx, "E0003", "empty alphabet Σ", st.lines[*iS].line_span, "no input symbols found"
      );
    }
  }

  // 3) Γ (recoverable)
  std::vector<Token> GToks;
  const std::optional<std::size_t> iG = next_nonempty(st, i);
  if (!lex::require_line(dx, iG, "Γ", eof)) {
    GToks = {};
  } else {
    GToks = st.lines[*iG].tokens;
    i = *iG + 1;
    if (GToks.empty()) {
      add_simple_error(
        dx, "E0004", "empty stack alphabet Γ", st.lines[*iG].line_span, "no stack symbols found"
      );
    }
  }

  // Build sets (may be empty after errors)
  const auto Qset = to_set(QToks);
  const auto Sset = to_set(SToks);
  const auto Gset = to_set(GToks);

  // 4) q0 (recoverable)
  Token q0Tok{lex::intern("<q0?>"), eof};
  const std::optional<std::size_t> iq0 = next_nonempty(st, i);
  if (!lex::require_line(dx, iq0, "q0", eof)) {
    // keep default placeholder
  } else {
    const Line& Lq0 = st.lines[*iq0];
    i = *iq0 + 1;
    if (Lq0.tokens.size() != 1) {
      add_simple_error(
        dx,
        "E0005",
        "invalid q0 line",
        Lq0.line_span,
        "expected exactly one token (the start state)"
      );
      if (!Lq0.tokens.empty())
        q0Tok = Lq0.tokens.front();
    } else {
      q0Tok = Lq0.tokens[0];
    }
    if (Qset.count(q0Tok.text) == 0) {
      add_symbol_error(dx, "E0007", "start state not in Q", q0Tok, "this state is not listed in Q");
    }
  }

  // 5) Z0 (recoverable)
  Token Z0Tok{lex::intern("<Z0?>"), eof};
  const std::optional<std::size_t> iZ0 = next_nonempty(st, i);
  if (!lex::require_line(dx, iZ0, "Z0", eof)) {
    // keep default
  } else {
    const Line& LZ0 = st.lines[*iZ0];
    i = *iZ0 + 1;
    if (LZ0.tokens.size() != 1) {
      add_simple_error(
        dx,
        "E0006",
        "invalid Z0 line",
        LZ0.line_span,
        "expected exactly one token (the bottom symbol)"
      );
      if (!LZ0.tokens.empty())
        Z0Tok = LZ0.tokens.front();
    } else {
      Z0Tok = LZ0.tokens[0];
    }
    if (Gset.count(Z0Tok.text) == 0) {
      add_symbol_error(
        dx, "E0008", "stack bottom symbol not in Γ", Z0Tok, "this symbol is not listed in Γ"
      );
    }
  }

  // 6) F (optional) with recovery
  std::vector<Token> Ftok;
  bool has_F_syntax = false;
  const std::optional<std::size_t> iF = next_nonempty(st, i);
  if (iF.has_value()) {
    const Line& LF = st.lines[*iF];
    // A line is an F line only if every token names a known state.
    // A transition-shaped line stays a transition, so typos in rules
    // report transition errors instead of F errors. Any other line is
    // neither: report its unknown tokens and skip it.
    const bool is_transition = looks_like_transition(LF.tokens, Qset, Sset, Gset);
    const bool all_states = std::ranges::all_of(LF.tokens, [&](const Token& t) {
      return Qset.contains(t.text);
    });
    if (!is_transition && all_states) {
      has_F_syntax = true;
      Ftok = LF.tokens;
      i = *iF + 1;
    } else if (!is_transition) {
      // Short of transition arity: cannot be a rule. Report unknown
      // tokens and skip the line. Longer lines fall through to the
      // transitions loop, which reports per-field errors.
      if (LF.tokens.size() < 4) {
        for (const auto& t : LF.tokens) {
          if (Qset.count(t.text) == 0) {
            diag::Diagnostic d;
            d.severity = diag::Severity::Error;
            d.code = "E0009";
            d.message = "invalid accepting states line";
            d.labels.push_back(diag::Label{
              .span = t.span,
              .primary = true,
              .message = "unknown state '" + std::string(lex::name(t.text)) + "'",
            });
            d.notes.push_back("F must contain only states from Q");
            dx.items.push_back(std::move(d));
          }
        }
        i = *iF + 1;
      }
    }
  }

  // 7) Transitions with per-field recovery
  std::vector<Rule> rules;
  if (i < st.lines.size()) {
    rules.reserve(st.lines.size() - i);
  }

  // Helper to segment a concatenated push token into Γ symbols.
  // Views of Γ for segmentation. Materialized per call; the table
  // owns the bytes and nothing interns here, so views stay valid.
  const auto segment_push = [&](const Token& tok) -> std::optional<std::vector<Symbol>> {
    const std::string_view w = lex::name(tok.text);
    if (is_eps_tok(w))
      return std::vector<Symbol>{};  // epsilon
    std::vector<std::string_view> gsyms;
    gsyms.reserve(Gset.size());
    for (const Symbol& g : Gset)
      gsyms.push_back(lex::name(g));
    for (std::string_view g : gsyms)
      if (g == w)
        return std::vector<Symbol>{tok.text};

    const std::size_t n = w.size();
    constexpr std::size_t unvisited = static_cast<std::size_t>(-1);
    std::vector<std::size_t> prev(n + 1, unvisited);
    std::vector<std::size_t> len_at(n + 1, 0);
    prev[0] = 0;

    for (std::size_t i = 0; i < n; ++i) {
      if (prev[i] == unvisited)
        continue;
      for (std::string_view g : gsyms) {
        const std::size_t len = g.size();
        if (i + len <= n && w.substr(i, len) == g) {
          if (prev[i + len] == unvisited) {
            prev[i + len] = i;
            len_at[i + len] = len;
          }
        }
      }
    }

    if (prev[n] == unvisited)
      return std::nullopt;

    std::vector<Symbol> parts;
    for (std::size_t i = n; i > 0;) {
      const std::size_t p = prev[i];
      const std::size_t len = len_at[i];
      parts.emplace_back(lex::intern(w.substr(p, len)));
      i = p;
    }
    std::reverse(parts.begin(), parts.end());
    return parts;
  };

  for (std::size_t k = i; k < st.lines.size(); ++k) {
    const Line& L = st.lines[k];
    if (!line_has_tokens(L))
      continue;

    const auto& T = L.tokens;

    if (T.size() < 4) {
      add_simple_error(
        dx, "E0010", "transition too short", L.line_span, "expected: from input top to [push...]"
      );
      continue;  // recover by skipping this line
    }

    // Fields
    const Token& from = T[0];
    const Token& in = T[1];
    const Token& top = T[2];
    const Token& to = T[3];

    // bad_in/bad_top reshape the rule below; unknown states are
    // hard errors and the rule is still recorded for diagnostics.
    bool bad_in = false;
    bool bad_top = false;

    if (Qset.count(from.text) == 0) {
      add_symbol_error(dx, "E0011", "unknown 'from' state", from, "state not in Q");
    }
    const bool in_is_ok = is_eps_tok(lex::name(in.text)) || Sset.count(in.text) > 0;
    if (!in_is_ok) {
      add_symbol_error(
        dx,
        "E0012",
        "unknown input symbol (or epsilon)",
        in,
        "use symbol from Σ or 'eps' for epsilon"
      );
      bad_in = true;
    }
    const bool top_is_ok = is_eps_tok(lex::name(top.text)) || Gset.count(top.text) > 0;
    if (!top_is_ok) {
      add_symbol_error(
        dx,
        "E0013",
        "unknown stack top symbol (or epsilon)",
        top,
        "use symbol from Γ or 'eps' to ignore top"
      );
      bad_top = true;
    }
    if (Qset.count(to.text) == 0) {
      add_symbol_error(dx, "E0014", "unknown 'to' state", to, "state not in Q");
    }

    Rule r;
    r.from = from.text;
    r.input = (bad_in || is_eps_tok(lex::name(in.text))) ? std::nullopt : std::optional<Symbol>(in.text);
    r.stack_top =
      (bad_top || is_eps_tok(lex::name(top.text))) ? std::nullopt : std::optional<Symbol>(top.text);
    r.to = to.text;

    if (T.size() > 4)
      r.push.reserve(T.size() - 4);
    for (std::size_t m = 4; m < T.size(); ++m) {
      const Token& ps = T[m];
      if (is_eps_tok(lex::name(ps.text)))
        continue;

      if (Gset.count(ps.text) > 0) {
        r.push.push_back(ps.text);
        continue;
      }

      if (auto parts = segment_push(ps)) {
        // epsilon -> empty vector; concatenation -> parts
        for (auto& s : *parts)
          r.push.push_back(std::move(s));
      } else {
        add_symbol_error(
          dx, "E0015", "unknown push symbol", ps, "push symbols must be from Γ or 'eps'"
        );
        // drop invalid push chunk, keep others
      }
    }

    rules.push_back(std::move(r));
  }

  // Build NPDA regardless; we will suppress returning it if errors exist.
  auto b = PDA::Builder();
  b.start(q0Tok.text).stack_bottom(Z0Tok.text);

  if (has_F_syntax && !Ftok.empty()) {
    std::vector<Symbol> F;
    F.reserve(Ftok.size());
    for (const auto& t : Ftok)
      F.push_back(t.text);
    b.accept_by(npda::AcceptBy::FinalState).accepting(F.begin(), F.end());
  } else {
    // Recovery/default: accept by empty stack
    b.accept_by(npda::AcceptBy::EmptyStack);
  }

  for (auto& r : rules)
    b.rule(r);

  auto built = std::move(b).build();
  if (!built) {
    // Report as an error but still include prior diagnostics
    add_simple_error(
      dx,
      "E0016",
      "failed to build NPDA",
      st.lines.empty() ? eof : st.lines.back().line_span,
      built.error().message
    );
  }

  if (dx.has_errors()) {
    return {std::unexpected(std::move(dx)), std::move(out.source)};
  }

  return {std::expected<PDA, diag::Diagnostics>(*std::move(built)), std::move(out.source)};
}

}  // namespace npda::parse
