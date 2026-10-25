#include "automata/text.h"

#include <algorithm>
#include <cstddef>
#include <format>
#include <ostream>
#include <print>
#include <span>
#include <string>
#include <string_view>

#include "automata/symbol.h"
#include "terminal/style.h"

namespace automata {

using terminal::PadRight;
using terminal::Style;
using terminal::Tone;

std::string FormatSet(std::span<const Symbol> symbols) {
  std::string text = "{";
  for (const Symbol symbol : symbols) {
    if (text.size() > 1) text += ", ";
    text += symbol.name();
  }
  return text + "}";
}

std::string TransitionLabel(std::size_t index, const Style& style) {
  return std::format("{}{}", style.glyphs().delta, index + 1);
}

std::size_t Digits(std::size_t value) {
  std::size_t digits = 1;
  for (; value >= 10; value /= 10) ++digits;
  return digits;
}

RuleLayout RuleLayout::For(std::span<const RuleText> rules) {
  RuleLayout layout{.label = 1 + Digits(rules.size())};
  for (const auto& rule : rules) {
    layout.condition =
        std::max(layout.condition, terminal::DisplayWidth(rule.condition));
    layout.result =
        std::max(layout.result, terminal::DisplayWidth(rule.result));
  }
  return layout;
}

std::size_t RuleLayout::Width(const Style& style) const {
  return label + 2 + condition + 1 +
         terminal::DisplayWidth(style.glyphs().arrow) + 1 + result;
}

terminal::Styled FormatRule(
    std::size_t index,
    const RuleText& rule,
    const RuleLayout& layout,
    const Style& style
) {
  const auto label = PadRight(TransitionLabel(index, style), layout.label);
  return style.Span(Tone::kAccent, label) +
         style.Span(
             Tone::kPlain,
             std::format("  {} ", PadRight(rule.condition, layout.condition))
         ) +
         style.Span(Tone::kMuted, style.glyphs().arrow) +
         style.Span(Tone::kPlain, std::format(" {}", rule.result));
}

void WriteTransitionList(
    std::ostream& out,
    std::span<const RuleText> rules,
    std::span<const std::string> notes,
    const Style& style
) {
  WriteHeading(out, "transitions", "", style);
  const auto layout = RuleLayout::For(rules);
  for (std::size_t index = 0; index < rules.size(); ++index) {
    const auto rule = FormatRule(index, rules[index], layout, style);
    if (notes.empty()) {
      std::println(out, "  {}", rule.text);
    } else {
      std::println(
          out,
          "  {}  {}",
          PadRight(rule, layout.Width(style)),
          style.Paint(Tone::kMuted, notes[index])
      );
    }
  }
}

void WriteHeading(
    std::ostream& out,
    std::string_view title,
    std::string_view subject,
    const Style& style
) {
  if (subject.empty()) {
    std::println(out, "\n{}", style.Paint(Tone::kHeading, title));
  } else {
    std::println(
        out,
        "\n{} {}",
        style.Paint(Tone::kHeading, title),
        style.Paint(Tone::kStrong, subject)
    );
  }
}

void WriteHiddenNotice(
    std::ostream& out,
    std::size_t hidden,
    std::string_view what,
    const Style& style
) {
  const auto text = std::format(
      "{} The trace does not show {} more {}{}. Increase -trace-limit to "
      "show them.",
      style.glyphs().ellipsis,
      style.Number(hidden),
      what,
      hidden == 1 ? "" : "s"
  );
  std::println(out, "  {}", style.Paint(Tone::kMuted, text));
}

}  // namespace automata
