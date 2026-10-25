#include "prf/trace_render.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <map>
#include <ostream>
#include <print>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include "prf/function.h"
#include "prf/trace.h"
#include "terminal/style.h"

namespace prf {

namespace {

using terminal::Style;
using terminal::Tone;

/// Paints each piece of `expression` in the hue of its role, so that the
/// defined names, the functions that compute, the projections that route
/// arguments, and the operators that join them read apart at a glance.
std::string Paint(const Expression& expression, const Style& style) {
  std::string text;
  for (const auto& token : expression) {
    switch (token.role) {
      case Role::kPlain:
        text += token.text;
        break;
      case Role::kNamed:
        text += style.Paint(Tone::kDefined, token.text);
        break;
      case Role::kBasic:
        text += style.Paint(Tone::kAccent, token.text);
        break;
      case Role::kProjection:
        text += style.Paint(Tone::kSecondary, token.text);
        break;
      case Role::kOperator:
        text += style.Paint(Tone::kMuted, token.text);
        break;
    }
  }
  return text;
}

std::string JoinValues(std::span<const Natural> values, const Style& style) {
  std::string text;
  for (const auto value : values) {
    if (!text.empty()) text += ", ";
    text += style.Number(value);
  }
  return text;
}

void WriteChildren(
    std::ostream& out,
    const TraceNode& node,
    const std::string& indent,
    const Style& style
) {
  const auto& glyphs = style.glyphs();
  for (std::size_t index = 0; index < node.children.size(); ++index) {
    const TraceNode& child = *node.children[index];
    const bool last = index + 1 == node.children.size();
    const auto results = child.results.value_or(Tuple{});
    std::println(
        out,
        "{}{}{}",
        indent,
        style.Paint(Tone::kMuted, last ? glyphs.tree_last : glyphs.tree_branch),
        FormatCall(*child.function, child.arguments, results, style)
    );
    WriteChildren(
        out,
        child,
        indent + style.Paint(Tone::kMuted, last ? "   " : glyphs.tree_pipe),
        style
    );
  }
}

void WriteCounts(std::ostream& out, const Trace& trace, const Style& style) {
  // Separate function objects can have the same name, such as Z, so their
  // counts share one row.
  const auto notation = NotationFor(style);
  std::map<std::string, std::pair<Expression, std::uint64_t>> by_name;
  for (const auto& [function, count] : trace.counts()) {
    auto name = function->Name(notation);
    auto& row = by_name[Text(name)];
    row.first = std::move(name);
    row.second += count;
  }
  std::vector<std::pair<Expression, std::uint64_t>> rows;
  for (auto& [text, row] : by_name) rows.push_back(std::move(row));
  std::ranges::stable_sort(rows, [](const auto& left, const auto& right) {
    return left.second > right.second;
  });
  const auto width = std::max<std::size_t>(
      terminal::DisplayWidth(style.Number(trace.Calls())),
      5
  );
  std::println(
      out,
      "\n  {}  {}",
      style.Paint(Tone::kMuted, terminal::PadLeft("calls", width)),
      style.Paint(Tone::kMuted, "function")
  );
  for (const auto& [name, count] : rows) {
    std::println(
        out,
        "  {}  {}",
        terminal::PadLeft(style.Number(count), width),
        Paint(name, style)
    );
  }
}

void WriteTree(std::ostream& out, const Trace& trace, const Style& style) {
  std::println(out);
  // The result line shows the top-level call, so the tree starts with its
  // children.
  for (const auto& root : trace.roots()) WriteChildren(out, *root, "", style);
  if (const auto hidden = trace.HiddenCalls(); hidden > 0) {
    const auto notice = std::format(
        "{} The trace does not show {} more call{}. Increase -trace-limit to "
        "show them.",
        style.glyphs().ellipsis,
        style.Number(hidden),
        hidden == 1 ? "" : "s"
    );
    std::println(out, "\n  {}", style.Paint(Tone::kMuted, notice));
  }
}

}  // namespace

Notation NotationFor(const Style& style) {
  return style.unicode() ? Notation::kUnicode : Notation::kAscii;
}

std::string FormatCall(
    const Function& function,
    std::span<const Natural> arguments,
    std::span<const Natural> results,
    const Style& style
) {
  auto text = std::format(
      "{}({})",
      Paint(function.Operand(NotationFor(style)), style),
      JoinValues(arguments, style)
  );
  if (results.size() == 1) {
    text += std::format(" = {}", style.Number(results.front()));
  } else if (!results.empty()) {
    text += std::format(" = ({})", JoinValues(results, style));
  }
  return text;
}

void WriteDefinition(
    std::ostream& out,
    const Function& function,
    const Style& style
) {
  const auto definitions = function.Definitions(NotationFor(style));
  std::size_t width = 0;
  for (const auto& definition : definitions) {
    for (const auto* equation : {&definition.limit, &definition.recursion}) {
      width = std::max(width, terminal::DisplayWidth(Text(equation->left)));
    }
  }
  for (std::size_t index = 0; index < definitions.size(); ++index) {
    // A blank line between definitions keeps each pair of equations together.
    if (index != 0) std::println(out);
    const auto& definition = definitions[index];
    for (const auto* equation : {&definition.limit, &definition.recursion}) {
      const auto padding = width - terminal::DisplayWidth(Text(equation->left));
      std::println(
          out,
          "{}{} {} {}",
          Paint(equation->left, style),
          std::string(padding, ' '),
          style.Paint(Tone::kMuted, "="),
          Paint(equation->right, style)
      );
    }
  }
}

void WriteTrace(std::ostream& out, const Trace& trace, const Style& style) {
  switch (trace.mode()) {
    case TraceMode::kOff:
      return;
    case TraceMode::kCounts:
      WriteCounts(out, trace, style);
      return;
    case TraceMode::kTree:
      WriteTree(out, trace, style);
      return;
  }
}

}  // namespace prf
