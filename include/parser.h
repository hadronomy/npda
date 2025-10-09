// npda_parse.hpp
// C++23. Parser with rustc-style diagnostics and error recovery.
// Depends on: npda.hpp, diag.hpp

#pragma once

#include <cctype>
#include <cstdio>
#include <expected>
#include <istream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "diag.h"
#include "npda.h"

namespace npda::parse {

using PDA = npda::NPDA<std::string, std::string, std::string>;
using Rule = npda::Rule<std::string, std::string, std::string>;

struct ParseResult {
  std::expected<PDA, diag::Diagnostics> value;
  diag::SourceFile source;
};

struct Token {
  std::string text;
  diag::Span span;
};

struct Line {
  std::size_t num_1 = 1;      // 1-based
  std::vector<Token> tokens;  // tokens before '#'
  diag::Span line_span;       // entire line (without '\n')
};

struct SpecTokens {
  std::vector<Line> lines;  // all lines with tokens
};

// Read entire stream
inline std::string read_all(std::istream& is) {
  std::ostringstream oss;
  oss << is.rdbuf();
  return oss.str();
}

inline SpecTokens lex(const diag::SourceFile& src) {
  SpecTokens st;
  st.lines.reserve(src.line_count());

  for (std::size_t li = 1; li <= src.line_count(); ++li) {
    const std::string_view sv = src.line_view(li);
    const std::size_t line_start = src.line_starts[li - 1];
    const std::size_t line_end = line_start + sv.size();

    Line L;
    L.num_1 = li;
    L.line_span = diag::Span{line_start, line_end};

    // Cut comments
    const std::size_t cut = sv.find('#');
    const std::size_t upto = (cut == std::string_view::npos) ? sv.size() : cut;

    // Tokenize (whitespace-separated)
    std::size_t i = 0;
    const auto is_space = [](char c) noexcept {
      return std::isspace(static_cast<unsigned char>(c)) != 0;
    };

    while (i < upto) {
      while (i < upto && is_space(sv[i]))
        ++i;
      if (i >= upto)
        break;

      std::size_t j = i;
      while (j < upto && !is_space(sv[j]))
        ++j;

      const std::size_t tok_lo = line_start + i;
      const std::size_t tok_hi = line_start + j;
      L.tokens.push_back(Token{
        std::string(sv.substr(i, j - i)),
        diag::Span{tok_lo, tok_hi},
      });

      i = j;
    }

    st.lines.push_back(std::move(L));
  }

  return st;
}

inline bool is_eps_tok(std::string_view s) {
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

inline void add_simple_error(
  diag::Diagnostics& dx,
  std::string code,
  std::string msg,
  diag::Span where,
  std::string label_msg
) {
  diag::Diagnostic d;
  d.severity = diag::Severity::Error;
  d.code = std::move(code);
  d.message = std::move(msg);
  d.labels.push_back(diag::Label{
    .span = where,
    .primary = true,
    .message = std::move(label_msg),
  });
  dx.items.push_back(std::move(d));
}

inline void add_symbol_error(
  diag::Diagnostics& dx,
  std::string code,
  std::string msg,
  const Token& t,
  std::string label_msg
) {
  add_simple_error(dx, std::move(code), std::move(msg), t.span, std::move(label_msg));
}

inline bool line_has_tokens(const Line& L) {
  return !L.tokens.empty();
}

// Next non-empty line >= i
inline std::optional<std::size_t> next_nonempty(const SpecTokens& st, std::size_t i) {
  const std::size_t n = st.lines.size();
  for (std::size_t k = i; k < n; ++k) {
    if (line_has_tokens(st.lines[k]))
      return k;
  }
  return std::nullopt;
}

// Turn tokens to set of strings
inline std::unordered_set<std::string> to_set(const std::vector<Token>& toks) {
  std::unordered_set<std::string> s;
  s.reserve(toks.size());
  for (const auto& t : toks)
    s.insert(t.text);
  return s;
}

inline bool looks_like_transition(
  const std::vector<Token>& toks,
  const std::unordered_set<std::string>& Q,
  const std::unordered_set<std::string>& S,
  const std::unordered_set<std::string>& G
) {
  if (toks.size() < 4)
    return false;

  const auto& from = toks[0];
  const auto& in = toks[1];
  const auto& top = toks[2];
  const auto& to = toks[3];

  const auto in_ok = is_eps_tok(in.text) || S.count(in.text) > 0;
  const auto top_ok = is_eps_tok(top.text) || G.count(top.text) > 0;

  if (Q.count(from.text) == 0)
    return false;
  if (!in_ok)
    return false;
  if (!top_ok)
    return false;
  if (Q.count(to.text) == 0)
    return false;

  // Allow concatenated push strings like "AS" if they can be segmented
  // entirely into Γ symbols.
  const auto can_segment = [&](std::string_view w) -> bool {
    if (is_eps_tok(w))
      return true;
    if (G.count(std::string(w)) > 0)
      return true;
    const std::size_t n = w.size();
    if (n == 0)
      return true;
    std::vector<char> dp(n + 1, 0);
    dp[0] = 1;
    for (std::size_t i = 0; i < n; ++i) {
      if (!dp[i])
        continue;
      for (const auto& g : G) {
        const std::size_t len = g.size();
        if (i + len <= n && w.substr(i, len) == g) {
          dp[i + len] = 1;
        }
      }
    }
    return dp[n] != 0;
  };

  for (std::size_t i = 4; i < toks.size(); ++i) {
    const auto& tk = toks[i].text;
    if (!can_segment(tk))
      return false;
  }
  return true;
}

inline ParseResult parse_with_diagnostics(std::istream& is, std::string filename) {
  ParseResult out;
  out.source = diag::SourceFile::from(filename, read_all(is));
  const auto st = lex(out.source);

  diag::Diagnostics dx;

  const auto eof_span = [&]() -> diag::Span {
    if (out.source.text.empty())
      return {0, 0};
    const std::size_t n = out.source.text.size();
    return {n ? n - 1 : 0, n};
  };

  const auto need_line = [&](std::optional<std::size_t> idx, std::string name) {
    if (!idx.has_value()) {
      add_simple_error(
        dx,
        "E0001",
        "missing required section: " + name,
        eof_span(),
        "file ends before section '" + name + "'"
      );
      return false;
    }
    return true;
  };

  std::size_t i = 0;

  // 1) Q (recoverable)
  std::vector<Token> QToks;
  const std::optional<std::size_t> iQ = next_nonempty(st, i);
  if (!need_line(iQ, "Q")) {
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
  if (!need_line(iS, "Σ")) {
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
  if (!need_line(iG, "Γ")) {
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
  Token q0Tok{"<q0?>", eof_span()};
  const std::optional<std::size_t> iq0 = next_nonempty(st, i);
  if (!need_line(iq0, "q0")) {
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
  Token Z0Tok{"<Z0?>", eof_span()};
  const std::optional<std::size_t> iZ0 = next_nonempty(st, i);
  if (!need_line(iZ0, "Z0")) {
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
    if (!looks_like_transition(LF.tokens, Qset, Sset, Gset)) {
      has_F_syntax = true;
      Ftok.reserve(LF.tokens.size());
      for (const auto& t : LF.tokens) {
        if (Qset.count(t.text) == 0) {
          diag::Diagnostic d;
          d.severity = diag::Severity::Error;
          d.code = "E0009";
          d.message = "invalid accepting states line";
          d.labels.push_back(diag::Label{
            .span = t.span,
            .primary = true,
            .message = "unknown state '" + t.text + "'",
          });
          d.notes.push_back("F must contain only states from Q");
          dx.items.push_back(std::move(d));
        } else {
          Ftok.push_back(t);
        }
      }
      i = *iF + 1;
    }
  }

  // 7) Transitions with per-field recovery
  std::vector<Rule> rules;
  if (i < st.lines.size()) {
    rules.reserve(st.lines.size() - i);
  }

  // Helper to segment a concatenated push token into Γ symbols.
  const auto segment_push = [&](const Token& tok) -> std::optional<std::vector<std::string>> {
    const std::string& w = tok.text;
    if (is_eps_tok(w))
      return std::vector<std::string>{};  // epsilon
    if (Gset.count(w) > 0)
      return std::vector<std::string>{w};

    const std::size_t n = w.size();
    std::vector<int> prev(n + 1, -1);
    std::vector<std::size_t> len_at(n + 1, 0);
    prev[0] = 0;

    for (std::size_t i = 0; i < n; ++i) {
      if (prev[i] < 0)
        continue;
      for (const auto& g : Gset) {
        const std::size_t len = g.size();
        if (i + len <= n && w.compare(i, len, g) == 0) {
          if (prev[i + len] < 0) {
            prev[i + len] = static_cast<int>(i);
            len_at[i + len] = len;
          }
        }
      }
    }

    if (prev[n] < 0)
      return std::nullopt;

    std::vector<std::string> parts;
    for (std::size_t i = n; i > 0;) {
      const std::size_t p = static_cast<std::size_t>(prev[i]);
      const std::size_t len = len_at[i];
      parts.emplace_back(w.substr(p, len));
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

    // Recovery flags
    bool bad_from = false;
    bool bad_in = false;
    bool bad_top = false;
    bool bad_to = false;

    if (Qset.count(from.text) == 0) {
      add_symbol_error(dx, "E0011", "unknown 'from' state", from, "state not in Q");
      bad_from = true;
    }
    const bool in_is_ok = is_eps_tok(in.text) || Sset.count(in.text) > 0;
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
    const bool top_is_ok = is_eps_tok(top.text) || Gset.count(top.text) > 0;
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
      bad_to = true;
    }

    Rule r;
    r.from = from.text;
    r.input = (bad_in || is_eps_tok(in.text)) ? std::nullopt : std::optional<std::string>(in.text);
    r.stack_top =
      (bad_top || is_eps_tok(top.text)) ? std::nullopt : std::optional<std::string>(top.text);
    r.to = to.text;

    if (T.size() > 4)
      r.push.reserve(T.size() - 4);
    for (std::size_t m = 4; m < T.size(); ++m) {
      const Token& ps = T[m];
      if (is_eps_tok(ps.text))
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

    (void)bad_from;
    (void)bad_to;
    rules.push_back(std::move(r));
  }

  // Build NPDA regardless; we will suppress returning it if errors exist.
  auto b = PDA::Builder();
  b.start(q0Tok.text).stack_bottom(Z0Tok.text);

  if (has_F_syntax && !Ftok.empty()) {
    std::vector<std::string> F;
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
      st.lines.empty() ? eof_span() : st.lines.back().line_span,
      built.error().message
    );
  }

  if (dx.has_errors()) {
    return {std::unexpected(std::move(dx)), out.source};
  }

  return {std::expected<PDA, diag::Diagnostics>(*std::move(built)), out.source};
}

}  // namespace npda::parse