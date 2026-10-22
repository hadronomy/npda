#include "parsing/reader.h"

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"

namespace parsing {

namespace {

bool IsSpace(char character) {
  return std::isspace(static_cast<unsigned char>(character)) != 0;
}

/// Returns the tokens before `#` on each line that has tokens. Spaces or tabs
/// divide the tokens.
std::vector<Line> Lex(const diag::SourceFile& source) {
  std::vector<Line> lines;
  for (std::size_t number = 1; number <= source.LineCount(); ++number) {
    const std::string_view text = source.Line(number);
    const std::size_t start = source.line_starts[number - 1];
    const std::size_t end = std::min(text.find('#'), text.size());
    Line line{.tokens = {}, .span = {start, start + text.size()}};
    for (std::size_t i = 0; i < end;) {
      if (IsSpace(text[i])) {
        ++i;
        continue;
      }
      std::size_t j = i;
      while (j < end && !IsSpace(text[j])) ++j;
      line.tokens.push_back(
          {.symbol = automata::Intern(text.substr(i, j - i)),
           .span = {start + i, start + j}}
      );
      i = j;
    }
    if (!line.tokens.empty()) lines.push_back(std::move(line));
  }
  return lines;
}

diag::SourceSpan EndOfFile(const diag::SourceFile& source) {
  const std::size_t size = source.text.size();
  return size == 0 ? diag::SourceSpan{0, 0} : diag::SourceSpan{size - 1, size};
}

}  // namespace

void ReportError(
    diag::DiagnosticSet& diagnostics,
    std::string code,
    std::string message,
    diag::SourceSpan span,
    std::string label
) {
  diag::Error(std::move(code), std::move(message))
      .Label(span, std::move(label))
      .Emit(diagnostics);
}

Reader::Reader(const diag::SourceFile& source, diag::DiagnosticSet& diagnostics)
    : lines_(Lex(source)),
      end_of_file_(EndOfFile(source)),
      diagnostics_(diagnostics) {}

std::span<const Line> Reader::Rest() const {
  return std::span(lines_).subspan(next_);
}

const Line* Reader::Peek() const {
  return next_ == lines_.size() ? nullptr : &lines_[next_];
}

diag::SourceSpan Reader::LastSpan() const {
  return lines_.empty() ? end_of_file_ : lines_.back().span;
}

const Line* Reader::Next(std::string_view section) {
  if (next_ == lines_.size()) {
    ReportError(
        diagnostics_,
        "E0001",
        std::format("missing required section: {}", section),
        end_of_file_,
        std::format("file ends before section '{}'", section)
    );
    return nullptr;
  }
  return &lines_[next_++];
}

SymbolSet Reader::ReadSet(std::string_view section) {
  SymbolSet symbols;
  if (const Line* line = Next(section)) {
    for (const auto& token : line->tokens) symbols.insert(token.symbol);
  }
  return symbols;
}

std::optional<Token> Reader::ReadSingle(
    std::string_view section,
    std::string code,
    std::string message,
    std::string_view role
) {
  const Line* line = Next(section);
  if (line == nullptr) return std::nullopt;
  if (line->tokens.size() != 1) {
    ReportError(
        diagnostics_,
        std::move(code),
        std::move(message),
        line->span,
        std::format("expected exactly one token ({})", role)
    );
  }
  return line->tokens.front();
}

std::vector<Symbol> Reader::ReadAcceptingLine(const SymbolSet& states) {
  std::vector<Symbol> accepting;
  const Line* line = Next("F");
  if (line == nullptr) return accepting;
  ReportUnknownAcceptingStates(*line, states);
  for (const auto& token : line->tokens) {
    if (states.contains(token.symbol)) accepting.push_back(token.symbol);
  }
  return accepting;
}

void Reader::ReportUnknownAcceptingStates(
    const Line& line,
    const SymbolSet& states
) {
  for (const auto& token : line.tokens) {
    if (states.contains(token.symbol)) continue;
    diag::Error("E0009", "invalid accepting states line")
        .Label(token.span, std::format("unknown state '{}'", token.symbol))
        .Note("F must contain only states from Q")
        .Emit(diagnostics_);
  }
}

}  // namespace parsing
