#ifndef NPDA_SRC_AUTOMATA_TEXT_H_
#define NPDA_SRC_AUTOMATA_TEXT_H_

#include <cstddef>
#include <ostream>
#include <ranges>
#include <span>
#include <string>
#include <string_view>

#include "automata/symbol.h"
#include "terminal/style.h"

/// Text forms for the output of all machines. A transition or a set of states
/// has the same form in a header, a trace, and a verdict.
namespace automata {

/// Returns the symbols without spaces between them. Machine files write push
/// strings in this form.
template <std::ranges::input_range Symbols>
[[nodiscard]] std::string Concat(Symbols&& symbols) {
  std::string text;
  for (const Symbol symbol : symbols) text += symbol.name();
  return text;
}

/// Separates symbols with spaces so `Z0 A` stays two symbols.
template <std::ranges::input_range Symbols>
[[nodiscard]] std::string Spaced(Symbols&& symbols) {
  std::string text;
  for (const Symbol symbol : symbols) {
    if (!text.empty()) text += ' ';
    text += symbol.name();
  }
  return text;
}

/// Returns a set in braces: `{q1, q2}`.
[[nodiscard]] std::string FormatSet(std::span<const Symbol> symbols);

/// Returns the label of the transition at `index`. The label for index 2 is δ3.
[[nodiscard]] std::string TransitionLabel(
    std::size_t index,
    const terminal::Style& style
);

[[nodiscard]] std::size_t Digits(std::size_t value);

/// A rule in two parts, divided at its arrow: `(q1, a, S)` and `(q1, AS)`.
struct RuleText {
  std::string condition;
  std::string result;
};

/// The column widths that align the arrows of all the rules of one machine.
struct RuleLayout {
  std::size_t label = 0;
  std::size_t condition = 0;
  std::size_t result = 0;

  [[nodiscard]] static RuleLayout For(std::span<const RuleText> rules);

  /// Returns the number of columns of the widest rule.
  [[nodiscard]] std::size_t Width(const terminal::Style& style) const;
};

/// Returns `δ7  (p, ε, S) → (q, S)`. The label and the condition have the
/// widths of `layout`.
[[nodiscard]] terminal::Styled FormatRule(
    std::size_t index,
    const RuleText& rule,
    const RuleLayout& layout,
    const terminal::Style& style
);

/// Writes a `transitions` section, with one numbered rule on each line.
///
/// If `notes` is not empty, it must hold one note per rule, which follows that
/// rule in the output.
void WriteTransitionList(
    std::ostream& out,
    std::span<const RuleText> rules,
    std::span<const std::string> notes,
    const terminal::Style& style
);

/// Writes a section heading: `title`, and then `subject` if it is not empty.
void WriteHeading(
    std::ostream& out,
    std::string_view title,
    std::string_view subject,
    const terminal::Style& style
);

/// Writes the notice that a trace shows in place of the items after its limit.
///
/// `what` is the name of one item, for example `step`.
void WriteHiddenNotice(
    std::ostream& out,
    std::size_t hidden,
    std::string_view what,
    const terminal::Style& style
);

}  // namespace automata

#endif  // NPDA_SRC_AUTOMATA_TEXT_H_
