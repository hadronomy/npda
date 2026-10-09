#include "prf/trace_render.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <ostream>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "prf/trace.h"
#include "terminal/style.h"

namespace prf {

namespace {

using terminal::Style;
using terminal::Tone;

/// Exponentiation makes deep trees. Below this depth, the tree shows a count in
/// place of a subtree.
constexpr std::size_t kMaxDepth = 20;

std::size_t CountCalls(const TraceNode& node) {
  std::size_t calls = 0;
  for (const auto& child : node.children) calls += 1 + CountCalls(*child);
  return calls;
}

std::string FormatCall(const TraceNode& node, const Style& style) {
  std::string text = std::format(
      "{}({})",
      style.Paint(Tone::kStrong, node.name),
      JoinArguments(node.args)
  );
  if (node.result) text += std::format(" = {}", *node.result);
  return text;
}

void WriteChildren(
    std::ostream& out,
    const TraceNode& node,
    const std::string& indent,
    std::size_t depth,
    const Style& style
) {
  const auto& glyphs = style.glyphs();
  for (std::size_t index = 0; index < node.children.size(); ++index) {
    const TraceNode& child = *node.children[index];
    const bool last = index + 1 == node.children.size();
    std::println(
        out,
        "{}{}{}",
        indent,
        style.Paint(Tone::kMuted, last ? glyphs.tree_last : glyphs.tree_branch),
        FormatCall(child, style)
    );
    const auto child_indent =
        indent + style.Paint(Tone::kMuted, last ? "   " : glyphs.tree_pipe);
    if (depth < kMaxDepth) {
      WriteChildren(out, child, child_indent, depth + 1, style);
    } else if (const auto hidden = CountCalls(child); hidden > 0) {
      std::println(
          out,
          "{}{}",
          child_indent,
          style.Paint(
              Tone::kMuted,
              std::format("{} {} deeper calls hidden", glyphs.ellipsis, hidden)
          )
      );
    }
  }
}

void WriteCounts(std::ostream& out, const Trace& trace, const Style& style) {
  std::vector<std::pair<std::string_view, std::uint64_t>> counts(
      trace.counts().begin(),
      trace.counts().end()
  );
  std::ranges::sort(counts, [](const auto& left, const auto& right) {
    return left.second != right.second ? left.second > right.second
                                       : left.first < right.first;
  });
  const auto total = trace.TotalCalls();
  const auto width = std::max<std::size_t>(std::to_string(total).size(), 5);
  std::println(
      out,
      "\n  {}  {}",
      style.Paint(Tone::kMuted, terminal::PadLeft("calls", width)),
      style.Paint(Tone::kMuted, "function")
  );
  for (const auto& [name, count] : counts) {
    std::println(
        out,
        "  {}  {}",
        terminal::PadLeft(std::to_string(count), width),
        name
    );
  }
  std::println(
      out,
      "  {}  {}",
      style.Paint(
          Tone::kStrong,
          terminal::PadLeft(std::to_string(total), width)
      ),
      style.Paint(Tone::kStrong, "total")
  );
}

}  // namespace

std::string JoinArguments(std::span<const std::uint64_t> arguments) {
  std::string text;
  for (const auto argument : arguments) {
    if (!text.empty()) text += ", ";
    text += std::to_string(argument);
  }
  return text;
}

void WriteTrace(std::ostream& out, const Trace& trace, const Style& style) {
  switch (trace.mode()) {
    case Trace::Mode::kOff:
      return;
    case Trace::Mode::kCounts:
      WriteCounts(out, trace, style);
      return;
    case Trace::Mode::kTree:
      for (const auto& root : trace.roots()) {
        WriteChildren(out, *root, "", 1, style);
      }
      return;
  }
}

}  // namespace prf
