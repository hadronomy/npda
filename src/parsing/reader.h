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

/// A word of a machine file. Spaces or tabs divide the words.
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

/// Reads the NPDA definition, one line for each section:
///
/// ```text
/// Q    states
/// Σ    input alphabet
/// Γ    stack alphabet
/// q0   start state
/// Z0   initial stack symbol
/// F    accepting states, optional
/// ```
///
/// The caller reads the remaining lines, which are the transitions. The reader
/// adds problems to the diagnostic set. Each read returns a usable value after
/// a problem. Thus, one run reports as many errors as possible.
class Reader {
 public:
  Reader(const diag::SourceFile& source, diag::DiagnosticSet& diagnostics);

  /// Returns the lines that the reader did not read.
  [[nodiscard]] std::span<const Line> Rest() const;

  /// Returns the span of the last line. If there are no lines, returns the end
  /// of the file.
  [[nodiscard]] diag::SourceSpan LastSpan() const;

  /// Reads the tokens of the next line. At the end of the file, reports E0001.
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
