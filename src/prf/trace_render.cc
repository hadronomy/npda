#include "prf/trace_render.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <ostream>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "prf/trace.h"
#include "terminal/ansi.h"

namespace prf {

std::string JoinArguments(std::span<const std::uint64_t> arguments,
                          std::string_view separator) {
  return std::format("{}", ansi::Join(arguments, separator));
}

namespace {

void PrintNode(std::ostream& os, const TraceNode& node,
               const std::string& indent, bool last, std::size_t depth);
constexpr std::size_t kMaxDepth = 20;

std::size_t CountDescendants(const TraceNode& node) {
  std::size_t n = 0;
  for (const auto& c : node.children) n += 1 + CountDescendants(*c);
  return n;
}

// Roots print bare so the first line pastes as an expression.
void PrintRoot(std::ostream& os, const TraceNode& node) {
  const std::string arr = ansi::UnicodeEnabled() ? "→" : "->";
  os << ansi::Format(
            ansi::Fg(ansi::TerminalColor::kCyan) | ansi::Emphasis::kBold, "{}",
            node.name)
     << "(" << JoinArguments(node.args) << ")";
  if (node.result)
    os << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen), " {} {}", arr,
                       *node.result);
  os << "\n";
  for (std::size_t i = 0; i < node.children.size(); ++i)
    PrintNode(os, *node.children[i], "", i + 1 == node.children.size(), 1);
}

void PrintNode(std::ostream& os, const TraceNode& node,
               const std::string& indent, bool last, std::size_t depth) {
  const bool uni = ansi::UnicodeEnabled();
  const std::string arr = uni ? "→" : "->";
  const std::string tick =
      last ? (uni ? "└─ " : "`-- ") : (uni ? "├─ " : "+-- ");
  const std::string vert = uni ? "│  " : "|  ";
  ansi::TextStyle guide;
  guide.em = ansi::Emphasis::kFaint;
  os << indent << ansi::Format(guide, "{}", tick)
     << ansi::Format(
            ansi::Fg(ansi::TerminalColor::kCyan) | ansi::Emphasis::kBold, "{}",
            node.name)
     << "(" << JoinArguments(node.args) << ")";
  if (node.result)
    os << ansi::Format(ansi::Fg(ansi::TerminalColor::kGreen), " {} {}", arr,
                       *node.result);
  if (depth >= 4) os << ansi::Format(guide, " (depth {})", depth);
  os << "\n";

  if (depth >= kMaxDepth) {
    const std::size_t hidden = CountDescendants(node);
    if (hidden > 0) {
      os << indent << (last ? "   " : vert) << (uni ? "…" : "...") << " ("
         << hidden << " hidden below)\n";
    }
    return;
  }
  const std::string child_indent = indent + (last ? "   " : vert);
  for (std::size_t i = 0; i < node.children.size(); ++i) {
    PrintNode(os, *node.children[i], child_indent,
              i + 1 == node.children.size(), depth + 1);
  }
}

}  // namespace

void RenderTrace(const Trace& trace, std::ostream& os) {
  // Full mode prints the tree only. The PASS line above already states
  // the answer, so a single root folds into its children.
  if (trace.mode() == Trace::Mode::kFull) {
    if (trace.roots().size() == 1) {
      const TraceNode& root = *trace.roots().front();
      for (std::size_t i = 0; i < root.children.size(); ++i)
        PrintNode(os, *root.children[i], "", i + 1 == root.children.size(), 1);
      return;
    }
    for (std::size_t i = 0; i < trace.roots().size(); ++i)
      PrintRoot(os, *trace.roots()[i]);
    return;
  }
  // Print a summary of function call counts
  if (!trace.counts().empty()) {
    os << "\nSummary:\n";
    // Emit in descending count, then lexicographic by name for stability
    std::vector<std::pair<std::string, std::uint64_t>> items(
        trace.counts().begin(), trace.counts().end());
    std::sort(items.begin(), items.end(), [](const auto& a, const auto& b) {
      if (a.second != b.second) return a.second > b.second;
      return a.first < b.first;
    });
    std::uint64_t total = 0;
    for (const auto& [name, cnt] : items) {
      os << "  " << name << ": " << cnt << "\n";
      total += cnt;
    }
    os << "  Total: " << total << "\n";
  }
}

}  // namespace prf
