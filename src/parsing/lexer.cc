#include "parsing/lexer.h"

#include <cctype>
#include <cstddef>
#include <istream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"

namespace lex {

// Read entire stream
[[nodiscard]] std::string ReadAll(std::istream& is) {
  std::ostringstream oss;
  oss << is.rdbuf();
  return oss.str();
}

[[nodiscard]] LexedDocument Lex(const diag::SourceFile& src) {
  LexedDocument st;
  st.lines.reserve(src.line_count());

  for (std::size_t li = 1; li <= src.line_count(); ++li) {
    const std::string_view sv = src.line_view(li);
    const std::size_t line_start = src.line_starts[li - 1];
    const std::size_t line_end = line_start + sv.size();

    TokenLine l;
    l.num_1 = li;
    l.line_span = diag::SourceSpan{line_start, line_end};

    // Cut comments
    const std::size_t cut = sv.find('#');
    const std::size_t upto = (cut == std::string_view::npos) ? sv.size() : cut;

    // Tokenize (whitespace-separated)
    std::size_t i = 0;
    const auto is_space = [](char c) noexcept {
      return std::isspace(static_cast<unsigned char>(c)) != 0;
    };

    while (i < upto) {
      while (i < upto && is_space(sv[i])) ++i;
      if (i >= upto) break;

      std::size_t j = i;
      while (j < upto && !is_space(sv[j])) ++j;

      const std::size_t tok_lo = line_start + i;
      const std::size_t tok_hi = line_start + j;
      l.tokens.push_back(Token{
          Intern(sv.substr(i, j - i)),
          diag::SourceSpan{tok_lo, tok_hi},
      });

      i = j;
    }

    st.lines.push_back(std::move(l));
  }

  return st;
}

void AddSimpleError(diag::DiagnosticSet& dx, std::string code, std::string msg,
                    diag::SourceSpan where, std::string label_msg) {
  diag::Error(std::move(code), std::move(msg))
      .Label(where, std::move(label_msg))
      .Emit(dx);
}

void AddSymbolError(diag::DiagnosticSet& dx, std::string code, std::string msg,
                    const Token& t, std::string label_msg,
                    std::string_view note) {
  AddSimpleError(dx, std::move(code), std::move(msg), t.span,
                 std::move(label_msg));
  if (!note.empty()) dx.items.back().notes.push_back(std::string(note));
}

[[nodiscard]] bool LineHasTokens(const TokenLine& l) {
  return !l.tokens.empty();
}

// Next non-empty line >= i
[[nodiscard]] std::optional<std::size_t> NextNonempty(const LexedDocument& st,
                                                      std::size_t i) {
  const std::size_t n = st.lines.size();
  for (std::size_t k = i; k < n; ++k) {
    if (LineHasTokens(st.lines[k])) return k;
  }
  return std::nullopt;
}

// Turn tokens into a set of symbols
[[nodiscard]] std::unordered_set<Symbol> ToSet(const std::vector<Token>& toks) {
  std::unordered_set<Symbol> s;
  s.reserve(toks.size());
  for (const auto& t : toks) s.insert(t.text);
  return s;
}

[[nodiscard]] diag::SourceSpan EofSpan(const diag::SourceFile& src) {
  if (src.text.empty()) return {0, 0};
  const std::size_t n = src.text.size();
  return {n ? n - 1 : 0, n};
}

bool RequireLine(diag::DiagnosticSet& dx, std::optional<std::size_t> idx,
                 std::string name, diag::SourceSpan eof) {
  if (!idx.has_value()) {
    AddSimpleError(dx, "E0001", "missing required section: " + name, eof,
                   "file ends before section '" + name + "'");
    return false;
  }
  return true;
}

}  // namespace lex
