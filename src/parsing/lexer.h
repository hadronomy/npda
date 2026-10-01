#ifndef NPDA_SRC_PARSING_LEXER_H_
#define NPDA_SRC_PARSING_LEXER_H_

#include <istream>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"

namespace lex {

using automata::Intern;
using automata::Symbol;
using automata::SymbolName;

struct Token {
  Symbol text;
  diag::SourceSpan span;
};

struct TokenLine {
  std::size_t num_1 = 1;       // 1-based
  std::vector<Token> tokens;   // tokens before '#'
  diag::SourceSpan line_span;  // entire line (without '\n')
};

struct LexedDocument {
  std::vector<TokenLine> lines;  // all lines with tokens
};

[[nodiscard]] std::string ReadAll(std::istream& input);
[[nodiscard]] LexedDocument Lex(const diag::SourceFile& source);
void AddSimpleError(diag::DiagnosticSet& diagnostics, std::string code,
                    std::string message, diag::SourceSpan where,
                    std::string label);
void AddSymbolError(diag::DiagnosticSet& diagnostics, std::string code,
                    std::string message, const Token& token, std::string label,
                    std::string_view note = {});
[[nodiscard]] bool LineHasTokens(const TokenLine& line);
[[nodiscard]] std::optional<std::size_t> NextNonempty(
    const LexedDocument& tokens, std::size_t begin);
[[nodiscard]] std::unordered_set<Symbol> ToSet(
    const std::vector<Token>& tokens);
[[nodiscard]] diag::SourceSpan EofSpan(const diag::SourceFile& source);
bool RequireLine(diag::DiagnosticSet& diagnostics,
                 std::optional<std::size_t> index, std::string section,
                 diag::SourceSpan end);

}  // namespace lex

#endif  // NPDA_SRC_PARSING_LEXER_H_
