#ifndef NPDA_SRC_PARSING_READER_H_
#define NPDA_SRC_PARSING_READER_H_

#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"

namespace parsing {

using automata::Symbol;
using SymbolSet = std::unordered_set<Symbol>;

/// A word in a machine file, separated from other words by spaces or tabs.
struct Token {
  Symbol symbol;
  diag::SourceSpan span;
};

/// A source line with one or more tokens before its comment.
struct Line {
  std::vector<Token> tokens;
  diag::SourceSpan span;
};

/// Adds an error with one label.
void ReportError(
    diag::DiagnosticSet& diagnostics,
    std::string code,
    std::string message,
    diag::SourceSpan span,
    std::string label
);

/// Reads the first lines that the two machine formats share, one line for each
/// section:
///
/// ```text
/// Q    states
/// Σ    input alphabet
/// Γ    stack or tape alphabet
/// q0   start state
/// Z0   one special Γ symbol: the initial stack symbol or the blank
/// F    accepting states
/// ```
///
/// The reader adds diagnostics as it reads and returns usable values after
/// errors so that one run can report as many errors as possible. Once the
/// sections are read, the caller reads the remaining lines, such as
/// transitions.
class Reader {
 public:
  Reader(const diag::SourceFile& source, diag::DiagnosticSet& diagnostics);

  /// Returns the lines that the reader did not read.
  [[nodiscard]] std::span<const Line> Rest() const;

  /// Returns the next line without reading it, or null at the end of the file.
  [[nodiscard]] const Line* Peek() const;

  /// Returns the span of the last line, or the end of the file if there are no
  /// lines.
  [[nodiscard]] diag::SourceSpan LastSpan() const;

  /// Reads the tokens of the next line, reporting E0001 at the end of the file.
  SymbolSet ReadSet(std::string_view section);

  /// Reads the next line, which must have exactly one token.
  ///
  /// At the end of the file, reports E0001 and returns nothing. If the line has
  /// more tokens, reports `code` and returns the first token.
  std::optional<Token> ReadSingle(
      std::string_view section,
      std::string code,
      std::string message,
      std::string_view role
  );

  /// Reads the next line as F, keeping only states and reporting E0009 for each
  /// token that is not a state.
  std::vector<Symbol> ReadAcceptingLine(const SymbolSet& states);

  /// Reads the optional F line.
  ///
  /// A line is the F line only if each token is a state and the line is not a
  /// rule. Thus, an error in the first rule gives a rule error, not an F error.
  /// A line can be neither an F line nor a rule. If this line also does not
  /// have the shape of a rule, the reader reports its unknown tokens and skips
  /// it.
  template <typename IsRule, typename HasRuleShape>
  std::optional<std::vector<Symbol>> ReadAcceptingStates(
      const SymbolSet& states,
      IsRule is_rule,
      HasRuleShape has_rule_shape
  ) {
    if (next_ == lines_.size()) return std::nullopt;
    const Line& line = lines_[next_];
    if (is_rule(line)) return std::nullopt;
    std::vector<Symbol> accepting;
    for (const auto& token : line.tokens) {
      if (states.contains(token.symbol)) accepting.push_back(token.symbol);
    }
    if (accepting.size() == line.tokens.size()) {
      ++next_;
      return accepting;
    }
    if (!has_rule_shape(line)) {
      ReportUnknownAcceptingStates(line, states);
      ++next_;
    }
    return std::nullopt;
  }

 private:
  const Line* Next(std::string_view section);
  void ReportUnknownAcceptingStates(const Line& line, const SymbolSet& states);

  std::vector<Line> lines_;
  std::size_t next_ = 0;
  diag::SourceSpan end_of_file_;
  diag::DiagnosticSet& diagnostics_;
};

}  // namespace parsing

#endif  // NPDA_SRC_PARSING_READER_H_
