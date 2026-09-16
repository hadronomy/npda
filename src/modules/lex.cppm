// Shared lexer: tokens, lines, and spec diagnostic helpers.
// Both machine parsers import this module.
export module lex;
import std;
import diag;

export namespace lex {

// An interned symbol: process-lifetime ID for a distinct string.
// Equality and hashing compare IDs, never bytes. A default Symbol
// matches nothing; engines overwrite it before use.
struct Symbol {
  std::uint32_t id = none;
  static constexpr std::uint32_t none = static_cast<std::uint32_t>(-1);

  [[nodiscard]] constexpr bool valid() const noexcept { return id != none; }
  [[nodiscard]] constexpr bool operator==(const Symbol&) const noexcept = default;
};

// Process-lifetime string table behind Symbol. Entries never move
// (deque) and never free, so views into the table stay valid.
class Interner {
 public:
  [[nodiscard]] Symbol intern(std::string_view s);
  [[nodiscard]] std::string_view lookup(Symbol sym) const;

 private:
  mutable std::mutex mutex_;
  std::deque<std::string> strings_;
  std::unordered_map<std::string_view, std::uint32_t> table_;
  std::uint32_t next_{};
};

// The shared table. One instance program-wide.
Interner& shared_table();

// Add a string to the table, or find its ID when present.
[[nodiscard]] Symbol intern(std::string_view s);

// Read back an interned string. Never retain the view across intern().
[[nodiscard]] std::string_view name(Symbol s);

// Non-inline definitions live here so exactly one object file emits
// them: inline copies in importers cannot satisfy cross-object uses.
Symbol Interner::intern(std::string_view s) {
  std::lock_guard lock(mutex_);
  if (const auto it = table_.find(s); it != table_.end())
    return Symbol{it->second};
  const std::uint32_t id = next_++;
  strings_.emplace_back(s);
  table_.try_emplace(std::string_view(strings_.back()), id);
  return Symbol{id};
}

std::string_view Interner::lookup(Symbol sym) const {
  std::lock_guard lock(mutex_);
  return strings_.at(sym.id);
}

Interner& shared_table() {
  static Interner table;
  return table;
}

Symbol intern(std::string_view s) {
  return shared_table().intern(s);
}

std::string_view name(Symbol s) {
  return shared_table().lookup(s);
}

}  // namespace lex

namespace std {
// Hash by ID. Makes Symbol satisfy the Hashable concept.
template <>
struct hash<lex::Symbol> {
  std::size_t operator()(lex::Symbol s) const noexcept {
    return std::hash<std::uint32_t>{}(s.id);
  }
};

// Format through the table, so "{}" keeps working on symbols.
template <typename Char>
struct formatter<lex::Symbol, Char> {
  formatter<std::string_view, Char> base_;
  constexpr auto parse(auto& pc) { return base_.parse(pc); }
  template <typename Ctx>
  auto format(lex::Symbol s, Ctx& ctx) const {
    return base_.format(lex::name(s), ctx);
  }
};
}  // namespace std

export namespace lex {
struct Token {
  Symbol text;
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
[[nodiscard]] inline std::string read_all(std::istream& is) {
  std::ostringstream oss;
  oss << is.rdbuf();
  return oss.str();
}

[[nodiscard]] inline SpecTokens lex(const diag::SourceFile& src) {
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
        intern(sv.substr(i, j - i)),
        diag::Span{tok_lo, tok_hi},
      });

      i = j;
    }

    st.lines.push_back(std::move(L));
  }

  return st;
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

[[nodiscard]] inline bool line_has_tokens(const Line& L) {
  return !L.tokens.empty();
}

// Next non-empty line >= i
[[nodiscard]] inline std::optional<std::size_t> next_nonempty(const SpecTokens& st, std::size_t i) {
  const std::size_t n = st.lines.size();
  for (std::size_t k = i; k < n; ++k) {
    if (line_has_tokens(st.lines[k]))
      return k;
  }
  return std::nullopt;
}

// Turn tokens into a set of symbols
[[nodiscard]] inline std::unordered_set<Symbol> to_set(const std::vector<Token>& toks) {
  std::unordered_set<Symbol> s;
  s.reserve(toks.size());
  for (const auto& t : toks)
    s.insert(t.text);
  return s;
}

[[nodiscard]] inline diag::Span eof_span(const diag::SourceFile& src) {
  if (src.text.empty())
    return {0, 0};
  const std::size_t n = src.text.size();
  return {n ? n - 1 : 0, n};
}

inline bool require_line(
  diag::Diagnostics& dx, std::optional<std::size_t> idx, std::string name, diag::Span eof
) {
  if (!idx.has_value()) {
    add_simple_error(
      dx, "E0001", "missing required section: " + name, eof, "file ends before section '" + name + "'"
    );
    return false;
  }
  return true;
}

}  // namespace lex
