#include "npda/trace.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "automata/symbol.h"
#include "npda/machine.h"
#include "terminal/ansi.h"
#include "terminal/palette.h"
#include "terminal/ui.h"

namespace npda {

namespace {

[[nodiscard]] ansi::Rgb GenerateNonAcceptingStateColor(std::size_t state_hash) {
  // Use golden ratio to create visually distinct colors
  // Map hash to a limited set of harmonious colors
  constexpr std::size_t color_palette_size = 8;
  std::size_t color_index = state_hash % color_palette_size;

  // Use golden ratio to generate harmonious hues, but avoid green range
  // Green is roughly 1/6 to 2/6 (0.166 to 0.333) of the hue wheel
  // We'll map to colors in the blue-purple-orange-red range instead
  constexpr double golden_ratio_conjugate = 0.618033988749895;
  double raw_hue = std::fmod(color_index * golden_ratio_conjugate, 1.0);

  // Shift hues to avoid green range (0.166 to 0.333)
  // Map [0, 0.166] -> [0, 0.166] (reds/oranges)
  // Map [0.166, 0.333] -> [0.333, 0.5] (blues)
  // Map [0.333, 1.0] -> [0.5, 1.0] (purples/reds)
  double base_hue;
  if (raw_hue < 0.166) {
    base_hue = raw_hue;  // Keep reds/oranges
  } else if (raw_hue < 0.333) {
    base_hue = raw_hue + 0.167;  // Shift greens to blues
  } else {
    base_hue = raw_hue;  // Keep purples/reds
  }

  // Create more vibrant colors for better visual distinction
  double saturation = 0.8;  // Increased saturation for more vibrant colors
  double lightness = 0.75;  // Slightly reduced lightness for better contrast

  // Convert HSL to RGB (simplified HSL to RGB conversion)
  double c = (1.0 - std::abs(2.0 * lightness - 1.0)) * saturation;
  double x = c * (1.0 - std::abs(std::fmod(base_hue * 6.0, 2.0) - 1.0));
  double m = lightness - c / 2.0;

  double r, g, b;
  if (base_hue < 1.0 / 6.0) {
    r = c;
    g = x;
    b = 0;
  } else if (base_hue < 2.0 / 6.0) {
    r = x;
    g = c;
    b = 0;
  } else if (base_hue < 3.0 / 6.0) {
    r = 0;
    g = c;
    b = x;
  } else if (base_hue < 4.0 / 6.0) {
    r = 0;
    g = x;
    b = c;
  } else if (base_hue < 5.0 / 6.0) {
    r = x;
    g = 0;
    b = c;
  } else {
    r = c;
    g = 0;
    b = x;
  }

  // Scale to 0-255 range with pastel effect
  std::uint8_t red = static_cast<std::uint8_t>((r + m) * 255.0 * 0.9 + 25);
  std::uint8_t green = static_cast<std::uint8_t>((g + m) * 255.0 * 0.9 + 25);
  std::uint8_t blue = static_cast<std::uint8_t>((b + m) * 255.0 * 0.9 + 25);

  return ansi::Rgb{red, green, blue};
}

[[nodiscard]] std::size_t CodePointCount(std::string_view s) {
  std::size_t n = 0;
  for (unsigned char c : s)
    if ((c & 0xC0) != 0x80) ++n;
  return n;
}

// Cut to N code points. Never splits UTF-8.
[[nodiscard]] std::string PrefixCodePoints(std::string_view s, std::size_t n) {
  std::size_t i = 0;
  std::size_t count = 0;
  while (i < s.size() && count < n) {
    unsigned char c = static_cast<unsigned char>(s[i]);
    std::size_t len = 1;
    if ((c & 0xE0) == 0xC0)
      len = 2;
    else if ((c & 0xF0) == 0xE0)
      len = 3;
    else if ((c & 0xF8) == 0xF0)
      len = 4;
    i += len;
    ++count;
  }
  return std::string(s.substr(0, std::min(i, s.size())));
}

// Visible width: code points outside escape sequences.
[[nodiscard]] std::size_t VisibleWidth(std::string_view s) {
  std::size_t w = 0;
  for (std::size_t i = 0; i < s.size();) {
    if (s[i] == '\x1b' && i + 1 < s.size() && s[i + 1] == '[') {
      std::size_t j = i + 2;
      while (j < s.size() && s[j] != 'm') ++j;
      i = (j < s.size()) ? j + 1 : s.size();
      continue;
    }
    unsigned char c = static_cast<unsigned char>(s[i]);
    std::size_t len = 1;
    if ((c & 0xE0) == 0xC0)
      len = 2;
    else if ((c & 0xF0) == 0xE0)
      len = 3;
    else if ((c & 0xF8) == 0xF0)
      len = 4;
    i += len;
    ++w;
  }
  return w;
}

// Text wrapping utility
// Wraps on visible width. Escapes ride along and never split.
[[nodiscard]] std::string WrapText(const std::string& text,
                                   std::size_t width = 35) {
  // Greedy word pack on visible width. Words keep their escapes.
  std::string result;
  std::size_t vis = 0;
  std::size_t i = 0;
  const auto take_word = [&](std::size_t from) {
    std::size_t j = from;
    while (j < text.size() && text[j] != ' ' && text[j] != '\n') {
      if (text[j] == '\x1b' && j + 1 < text.size() && text[j + 1] == '[') {
        std::size_t k = j + 2;
        while (k < text.size() && text[k] != 'm') ++k;
        j = (k < text.size()) ? k + 1 : text.size();
      } else {
        ++j;
      }
    }
    return text.substr(from, j - from);
  };
  while (i < text.size()) {
    if (text[i] == '\n') {
      result += '\n';
      vis = 0;
      ++i;
      continue;
    }
    if (text[i] == ' ') {
      ++i;
      continue;
    }
    const std::string word = take_word(i);
    i += word.size();
    const std::size_t w = VisibleWidth(word);
    if (vis > 0 && vis + 1 + w > width) {
      result += '\n';
      vis = 0;
    } else if (vis > 0) {
      result += ' ';
      ++vis;
    }
    result += word;
    vis += w;
  }
  return result;
}

// Rule explanation with colorization
[[nodiscard]] std::string ExplainRule(const Transition& r) {
  std::string explanation;

  // Build colored explanation parts
  std::string header = ansi::Format(ansi::Fg(terminal::colors::kSectionHeading),
                                    "Explanation: Transition: ");
  explanation += header;

  // From state (colored as command name)
  std::string from_state_colored = ansi::Format(
      ansi::Fg(terminal::colors::kCommandName), "'{}'", r.from_state);
  explanation += std::format("From state {} ", from_state_colored);

  // Symbol condition
  if (r.input.has_value()) {
    std::string input_colored = ansi::Format(
        ansi::Fg(terminal::colors::kWarning), "'{}'", r.input.value());
    explanation += std::format("when reading {} ", input_colored);
  } else {
    const std::string eps_word = ansi::UnicodeEnabled() ? "ε" : "eps";
    explanation +=
        ansi::Format(ansi::Fg(terminal::colors::kInfo),
                     "without consuming input ({}-transition) ", eps_word);
  }

  // Stack condition
  if (r.stack_top.has_value()) {
    std::string stack_colored = ansi::Format(
        ansi::Fg(terminal::colors::kWarning), "'{}'", r.stack_top.value());
    explanation += std::format("with {} on top of stack ", stack_colored);
  } else {
    explanation += ansi::Format(ansi::Fg(terminal::colors::kInfo),
                                "regardless of stack contents ");
  }

  // Action and destination (arrow colored as example, state as command)
  std::string to_state_colored = ansi::Format(
      ansi::Fg(terminal::colors::kCommandName), "'{}'", r.to_state);
  std::string arrow_colored =
      ansi::Format(ansi::Fg(terminal::colors::kExample), "{}", ui::Arrow());
  explanation += arrow_colored;
  explanation += std::format(" move to state {} ", to_state_colored);

  // Stack operation with colorized symbols
  if (r.stack_top.has_value() && !r.push.empty()) {
    std::string stack_top_colored = ansi::Format(
        ansi::Fg(terminal::colors::kWarning), "'{}'", r.stack_top.value());
    explanation += std::format(" and replace {} with ", stack_top_colored);
    for (std::size_t i = 0; i < r.push.size(); ++i) {
      std::string push_sym_colored =
          ansi::Format(ansi::Fg(terminal::colors::kWarning), "'{}'", r.push[i]);
      explanation += push_sym_colored;
      if (i + 1 < r.push.size()) explanation += ", ";
    }
  } else if (r.stack_top.has_value()) {
    std::string stack_top_colored = ansi::Format(
        ansi::Fg(terminal::colors::kWarning), "'{}'", r.stack_top.value());
    explanation += std::format(" and pop {} from stack", stack_top_colored);
  } else if (!r.push.empty()) {
    explanation += " and push ";
    if (r.push.size() == 1) {
      std::string push_sym_colored =
          ansi::Format(ansi::Fg(terminal::colors::kWarning), "'{}'", r.push[0]);
      explanation += std::format("{} onto stack", push_sym_colored);
    } else {
      explanation += "'";
      for (std::size_t i = 0; i < r.push.size(); ++i) {
        std::string push_sym_colored = ansi::Format(
            ansi::Fg(terminal::colors::kWarning), "'{}'", r.push[i]);
        explanation += push_sym_colored;
        if (i + 1 < r.push.size()) explanation += ", ";
      }
      explanation += "' onto stack";
    }
  } else {
    explanation += ansi::Format(ansi::Fg(terminal::colors::kInfo),
                                " without changing stack");
  }

  explanation += ".";
  return WrapText(explanation, 80);
}

// C++23 NPDA (nondeterministic pushdown automaton), header-only.
// - Epsilon transitions (input == nullopt)
// - Accept by final state, empty stack, or both (after full input)
// - BFS/DFS exploration
// - Loop-avoidance with visited configurations
// - Optional witness reconstruction (indices of rules taken)

struct RenderContext {
  bool track_witness;
  TraceOptions trace;
};

class Renderer {
 public:
  explicit Renderer(const Definition& definition) : definition_(definition) {}

  void EmitTraceStep(const SearchNode& node, std::span<const Symbol> input,
                     std::size_t step_num,
                     const std::optional<Transition>& rule,
                     const RenderContext& opt, bool is_backtrack_point = false,
                     bool is_exploration = false) const;
  void ReplayTracePath(std::span<const SearchNode> nodes, std::size_t final_idx,
                       std::span<const Symbol> input,
                       const RenderContext& opt) const;
  void ShowRejectionTrace(std::span<const SearchNode> nodes,
                          std::size_t furthest_node_index,
                          std::span<const Symbol> input,
                          const RenderContext& opt,
                          std::size_t expansions) const;
  void ShowExplorationTree(
      std::span<const SearchNode> nodes,
      std::span<const std::size_t> explored_nodes,
      std::span<const Symbol> input, const RenderContext& opt,
      const std::vector<std::size_t>& accepting_path) const;

 private:
  const Definition& definition_;
};

void Renderer::EmitTraceStep(const SearchNode& node,
                             std::span<const Symbol> input,
                             std::size_t step_num,
                             const std::optional<Transition>& rule,
                             const RenderContext& opt, bool is_backtrack_point,
                             [[maybe_unused]] bool is_exploration) const {
  if (!opt.trace.enabled) return;

  auto sink = opt.trace.sink ? opt.trace.sink
                             : [](std::string_view s) { std::print("{}", s); };

  std::string output;

  // Styles honor the caller flag; ansi::Format gates the rest on TTY.
  const auto st = [&](ansi::TextStyle s) {
    return opt.trace.colors ? s : ansi::TextStyle{};
  };
  ansi::TextStyle faint;
  faint.em = ansi::Emphasis::kFaint;
  ansi::TextStyle bold;
  bold.em = ansi::Emphasis::kBold;

  // Step card header. Title holds number plus transition only.
  std::string title = std::format("Step {}", step_num);
  if (rule.has_value()) {
    const auto& r0 = rule.value();
    title += std::format(" · {} {} {}", std::format("{}", r0.from_state),
                         ui::Arrow(), std::format("{}", r0.to_state));
  }
  if (is_backtrack_point && opt.trace.show_backtracking)
    title += " · backtrack";
  output += "\n" + ui::Rule(title) + "\n";

  // Current state. Bold always; green only when accepting.
  output += ansi::Format(st(faint), "state: ");
  if (std::ranges::contains(definition_.accepting_states, node.state)) {
    output += ansi::Format(
        st(ansi::Fg(terminal::colors::kSuccess) | ansi::Emphasis::kBold),
        "{}\n", std::format("{}", node.state));
  } else {
    output += ansi::Format(st(bold), "{}\n", std::format("{}", node.state));
  }

  // Symbol window around the head. Full tapes flood; windows do not.
  // Cells cap at 6 code points so the caret math below holds.
  constexpr std::size_t k_window = 8;
  const std::string k_ellipsis = ansi::UnicodeEnabled() ? "… " : "... ";
  output += ansi::Format(st(faint), "input: ");
  std::size_t head_col = 0;
  std::size_t col = 0;
  const std::size_t ilo =
      (node.input_position > k_window) ? node.input_position - k_window : 0;
  const std::size_t ihi =
      std::min(input.size(), node.input_position + k_window + 1);
  if (ilo > 0) {
    output += k_ellipsis;
    col += CodePointCount(k_ellipsis);
  }
  for (std::size_t i = ilo; i < ihi; ++i) {
    const std::string cell = PrefixCodePoints(std::format("{}", input[i]), 6);
    if (i == node.input_position) {
      // Caret centers under the symbol, not the bracket.
      head_col = col + 1 + CodePointCount(cell) / 2;
      output += ansi::Format(
          st(ansi::Fg(terminal::colors::kWarning) | ansi::Emphasis::kBold),
          "[{}]", cell);
      col += CodePointCount(cell) + 2;
    } else {
      output += std::format(" {} ", cell);
      col += CodePointCount(cell) + 2;
    }
  }
  if (node.input_position >= input.size()) {
    head_col = col + 2;
    output += ansi::Format(
        st(ansi::Fg(terminal::colors::kSuccess) | ansi::Emphasis::kBold),
        " [END]");
    col += 6;
  } else if (ihi < input.size()) {
    output += k_ellipsis;
  }
  output += "\n";
  // Caret under the head bracket. Brackets survive pipes; the caret anchors.
  output += std::string(7 + head_col, ' ') + "^\n";

  // Stack visualization. Shows the top cells only, plus a count.
  constexpr std::size_t k_stack = 9;
  output += ansi::Format(st(faint), "stack: ");
  if (node.stack.empty()) {
    output += "(empty)\n";
  } else {
    const std::size_t shown = std::min(node.stack.size(), k_stack);
    auto cell3 = [](std::string s) {
      // Center in a 3-wide field. Single symbols keep the original face.
      s = PrefixCodePoints(s, 3);
      const std::size_t w = CodePointCount(s);
      const std::size_t left = (3 - std::min(w, std::size_t{3})) / 2;
      std::string out(left, ' ');
      out += s;
      while (CodePointCount(out) < 3) out += ' ';
      return out;
    };
    // Show stack top on the right (conventional). Linear row keeps raw
    // symbols; the box below centers them in 3-wide cells.
    for (std::size_t i = 0; i < shown; ++i) {
      std::size_t stack_idx = node.stack.size() - 1 - i;  // top first
      const std::string cell =
          PrefixCodePoints(std::format("{}", node.stack[stack_idx]), 6);
      if (i == 0) {
        output += ansi::Format(
            st(ansi::Fg(terminal::colors::kWarning) | ansi::Emphasis::kBold),
            "[{}]", cell);
      } else {
        output += std::format(" {} ", cell);
      }
    }
    if (shown < node.stack.size()) {
      output += std::format("(+{} more, depth {})", node.stack.size() - shown,
                            node.stack.size());
    }
    output += "\n";

    // Visual stack representation. Opt-in; the linear row is the default.
    if (!opt.trace.compact && opt.trace.box) {
      const bool uni = ansi::UnicodeEnabled();
      const std::string h3 = uni ? "───" : "---";
      const std::string tl = uni ? "┌" : "+";
      const std::string tj = uni ? "┬" : "+";
      const std::string tr = uni ? "┐" : "+";
      const std::string bl = uni ? "└" : "+";
      const std::string bj = uni ? "┴" : "+";
      const std::string br = uni ? "┘" : "+";
      const std::string vv = uni ? "│" : "|";
      output += "       ";
      for (std::size_t i = 0; i < shown; ++i) {
        if (shown == 1) {
          output += tl + h3 + tr;
          break;
        }
        if (i == 0) {
          output += tl + h3 + tj;
          continue;
        }
        if (i != shown - 1) {
          output += h3 + tj;
          continue;
        }
        output += h3 + tr;
      }
      output += "\n       ";
      // Cells pin at 3 wide with shared bars: │AAA│BB │. No extra spaces.
      for (std::size_t i = 0; i < shown; ++i) {
        std::size_t stack_idx = node.stack.size() - 1 - i;
        const std::string cell =
            cell3(std::format("{}", node.stack[stack_idx]));
        output += vv;
        if (i == 0) {
          output += ansi::Format(
              st(ansi::Fg(terminal::colors::kWarning) | ansi::Emphasis::kBold),
              "{}", cell);
        } else {
          output += cell;
        }
      }
      output += vv;
      output += "\n       ";
      for (std::size_t i = 0; i < shown; ++i) {
        if (shown == 1) {
          output += bl + h3 + br;
          break;
        }
        if (i == 0) {
          output += bl + h3 + bj;
          continue;
        }
        if (i != shown - 1) {
          output += h3 + bj;
          continue;
        }
        output += h3 + br;
      }
      output += "\n";
    }
  }

  // Rule as one canonical tuple. Bold covers the arrow plus target.
  if (rule.has_value()) {
    const auto& r = rule.value();
    const std::string arr = ui::Arrow();
    const std::string eps = ansi::UnicodeEnabled() ? "ε" : "eps";
    output += ansi::Format(st(faint), "rule: ");
    output += std::format(
        "({}, {}, {}) ", std::format("{}", r.from_state),
        r.input.has_value() ? std::format("{}", r.input.value()) : eps,
        r.stack_top.has_value() ? std::format("{}", r.stack_top.value()) : "-");

    std::string push;
    if (r.push.empty()) {
      push = "push()";
    } else if (r.push.size() == 1) {
      push = "push " + std::format("{}", r.push.front());
    } else {
      push = "push(";
      for (std::size_t i = 0; i < r.push.size(); ++i) {
        push += std::format("{}", r.push[i]);
        if (i + 1 < r.push.size()) push += ", ";
      }
      push += ")";
    }
    output += ansi::Format(st(bold), "{} ({}, {})", arr,
                           std::format("{}", r.to_state), push);
    output += "\n";

    // Add natural language explanation if enabled
    if (opt.trace.explanations) {
      std::string explanation = npda::ExplainRule(r);
      output += ansi::Format(st(faint), "{}\n", explanation);
    }
  }

  sink(output);
}

void Renderer::ReplayTracePath(std::span<const SearchNode> nodes,
                               std::size_t final_idx,
                               std::span<const Symbol> input,
                               const RenderContext& opt) const {
  if (!opt.trace.enabled || !opt.track_witness) return;

  // Reconstruct node and rule paths from root to final
  std::vector<std::size_t> node_path_rev;
  std::vector<std::size_t> rule_path_rev;  // rules applied (except root)

  std::size_t cur = final_idx;
  while (true) {
    node_path_rev.push_back(cur);
    if (!nodes[cur].predecessor) break;
    rule_path_rev.push_back(nodes[cur].predecessor->transition_index);
    cur = nodes[cur].predecessor->node_index;
  }

  std::vector<std::size_t> node_path(node_path_rev.rbegin(),
                                     node_path_rev.rend());
  std::vector<std::size_t> transition_path(rule_path_rev.rbegin(),
                                           rule_path_rev.rend());

  auto sink = opt.trace.sink ? opt.trace.sink
                             : [](std::string_view s) { std::print("{}", s); };

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kBannerText),
                      "\n{} Accepting path found! Replaying {} steps...\n",
                      terminal::symbols::kInfo, transition_path.size()));
  } else {
    sink(std::format("\nAccepting path found! Replaying {} steps...\n",
                     transition_path.size()));
  }

  // Live search already showed every path node with its rule.
  // Print steps only when live emission stayed off.
  if (!opt.trace.show_full_trace) {
    // Slice long paths: head, hidden count, tail. The node after a gap
    // prints without a rule since its parent stays hidden.
    std::vector<std::size_t> show;
    if (node_path.size() > std::max<std::size_t>(2, opt.trace.step_limit) + 1 &&
        std::max<std::size_t>(2, opt.trace.step_limit) > 1) {
      const std::size_t head =
          std::max<std::size_t>(2, opt.trace.step_limit) / 2;
      const std::size_t tail =
          std::max<std::size_t>(2, opt.trace.step_limit) - head;
      for (std::size_t i = 0; i < head; ++i) show.push_back(i);
      for (std::size_t i = node_path.size() - tail; i < node_path.size(); ++i)
        show.push_back(i);
    } else {
      for (std::size_t i = 0; i < node_path.size(); ++i) show.push_back(i);
    }

    std::size_t step_num = 0;
    for (std::size_t k = 0; k < show.size(); ++k) {
      const std::size_t i = show[k];
      if (k > 0 && i != show[k - 1] + 1) {
        for (std::size_t j = show[k - 1] + 1; j < i; j += 50) {
          const auto& lm = nodes[node_path[j]];
          sink(ui::Rule(std::format("… step {} · {} · pos {} · depth {}", j,
                                    std::format("{}", lm.state),
                                    lm.input_position, lm.stack.size())) +
               "\n");
        }
        sink(std::format("… ({} steps hidden, raise --trace-limit to expand)\n",
                         i - show[k - 1] - 1));
      }
      std::optional<Transition> rr = std::nullopt;
      if (i > 0 && k > 0 && i == show[k - 1] + 1)
        rr = definition_.transitions[transition_path[i - 1]];
      const std::size_t node_idx = node_path[i];
      const bool is_backtrack =
          (k > 0 && nodes[node_idx].input_position <
                        nodes[node_path[show[k - 1]]].input_position);
      EmitTraceStep(nodes[node_idx], input, step_num++, rr, opt, is_backtrack);
    }
  }

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kSuccess),
                      "\n{} Input accepted!\n", terminal::symbols::kSuccess));
  } else {
    sink("\nInput accepted!\n");
  }
}

void Renderer::ShowRejectionTrace(std::span<const SearchNode> nodes,
                                  std::size_t furthest_node_index,
                                  std::span<const Symbol> input,
                                  const RenderContext& opt,
                                  std::size_t expansions) const {
  if (!opt.trace.enabled || !opt.track_witness) return;

  auto sink = opt.trace.sink ? opt.trace.sink
                             : [](std::string_view s) { std::print("{}", s); };

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kBannerText),
                      "\n{} Input rejected! Showing furthest path explored ({} "
                      "expansions)...\n",
                      terminal::symbols::kError, expansions));
  } else {
    sink(std::format(
        "\nInput rejected! Showing furthest path explored ({} expansions)...\n",
        expansions));
  }

  // Reconstruct the path to the best node we found
  std::vector<std::size_t> node_path_rev;
  std::vector<std::size_t> rule_path_rev;

  std::size_t cur = furthest_node_index;
  while (true) {
    node_path_rev.push_back(cur);
    if (!nodes[cur].predecessor) break;
    rule_path_rev.push_back(nodes[cur].predecessor->transition_index);
    cur = nodes[cur].predecessor->node_index;
  }

  std::vector<std::size_t> node_path(node_path_rev.rbegin(),
                                     node_path_rev.rend());
  std::vector<std::size_t> transition_path(rule_path_rev.rbegin(),
                                           rule_path_rev.rend());

  // Show how far we got in the input
  const auto& best_node = nodes[furthest_node_index];
  std::size_t remaining_input = input.size() - best_node.input_position;

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kInfo),
                      "Furthest position: {} / {} ({} characters remaining)\n",
                      best_node.input_position, input.size(), remaining_input));
  } else {
    sink(std::format("Furthest position: {} / {} ({} characters remaining)\n",
                     best_node.input_position, input.size(), remaining_input));
  }

  // Emit the path with the same slicing as the accept replay.
  // The stuck node is the path end, so no extra final emission.
  std::vector<std::size_t> show;
  if (node_path.size() > std::max<std::size_t>(2, opt.trace.step_limit) + 1 &&
      std::max<std::size_t>(2, opt.trace.step_limit) > 1) {
    const std::size_t head = std::max<std::size_t>(2, opt.trace.step_limit) / 2;
    const std::size_t tail =
        std::max<std::size_t>(2, opt.trace.step_limit) - head;
    for (std::size_t i = 0; i < head; ++i) show.push_back(i);
    for (std::size_t i = node_path.size() - tail; i < node_path.size(); ++i)
      show.push_back(i);
  } else {
    for (std::size_t i = 0; i < node_path.size(); ++i) show.push_back(i);
  }

  std::size_t step_num = 0;
  for (std::size_t k = 0; k < show.size(); ++k) {
    const std::size_t i = show[k];
    if (k > 0 && i != show[k - 1] + 1) {
      for (std::size_t j = show[k - 1] + 1; j < i; j += 50) {
        const auto& lm = nodes[node_path[j]];
        sink(ui::Rule(std::format("… step {} · {} · pos {} · depth {}", j,
                                  std::format("{}", lm.state),
                                  lm.input_position, lm.stack.size())) +
             "\n");
      }
      sink(std::format("… ({} steps hidden, raise --trace-limit to expand)\n",
                       i - show[k - 1] - 1));
    }
    std::optional<Transition> rr = std::nullopt;
    if (i > 0 && k > 0 && i == show[k - 1] + 1)
      rr = definition_.transitions[transition_path[i - 1]];
    const std::size_t node_idx = node_path[i];
    const bool is_backtrack =
        (k > 0 && nodes[node_idx].input_position <
                      nodes[node_path[show[k - 1]]].input_position);
    EmitTraceStep(nodes[node_idx], input, step_num++, rr, opt, is_backtrack);
  }

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kError),
                      "\n{} Input rejected at this point!\n",
                      terminal::symbols::kError));
  } else {
    sink("\nInput rejected at this point!\n");
  }
}

void Renderer::ShowExplorationTree(
    std::span<const SearchNode> nodes,
    std::span<const std::size_t> explored_nodes,
    [[maybe_unused]] std::span<const Symbol> input, const RenderContext& opt,
    const std::vector<std::size_t>& accepting_path) const {
  // Convert accepting_path to a set for fast lookup
  std::unordered_set<std::size_t> accepting_nodes(accepting_path.begin(),
                                                  accepting_path.end());

  if (!opt.trace.enabled || explored_nodes.empty()) return;

  auto sink = opt.trace.sink ? opt.trace.sink
                             : [](std::string_view s) { std::print("{}", s); };

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kBannerText),
                      "\n{} Exploration Tree Structure:\n",
                      terminal::symbols::kInfo));
  } else {
    sink("\nExploration Tree Structure:\n");
  }

  // Build parent-child relationships
  std::unordered_map<std::size_t, std::vector<std::size_t>> children;
  for (std::size_t node_idx : explored_nodes) {
    if (nodes[node_idx].predecessor) {
      children[nodes[node_idx].predecessor->node_index].push_back(node_idx);
    }
  }

  // Tree chrome follows the unicode probe. Same tree, two alphabets.
  const bool tuni = ansi::UnicodeEnabled();
  const std::string k_last = tuni ? "└──" : "`--";
  const std::string k_mid = tuni ? "├──" : "+--";
  const std::string k_vert = tuni ? "│   " : "|   ";
  std::size_t printed = 0;

  // Recursive function to print tree with enhanced visualization
  std::function<void(std::size_t, std::string, bool)> print_tree;
  print_tree = [&](std::size_t node_idx, std::string prefix, bool is_last) {
    if (printed >= std::max<std::size_t>(2, opt.trace.step_limit)) return;
    ++printed;
    const auto& node = nodes[node_idx];

    // Get current input symbol (if any) - use lambda for empty
    std::string input_sym = "λ";
    if (node.input_position < input.size()) {
      input_sym = std::format("{}", input[node.input_position]);
    }

    // Get stack representation - show full stack content
    std::string stack_repr;
    if (node.stack.empty()) {
      stack_repr = "[]";
    } else {
      // Show stack from bottom to top (left to right), capped with a count.
      constexpr std::size_t k_tree_stack = 8;
      std::string stack_content;
      const std::size_t tshown = std::min(node.stack.size(), k_tree_stack);
      for (std::size_t i = 0; i < tshown; ++i) {
        if (i > 0) stack_content += ",";
        stack_content += std::format("{}", node.stack[i]);
      }
      if (tshown < node.stack.size())
        stack_content +=
            std::format(",(+{} more, depth {})", node.stack.size() - tshown,
                        node.stack.size());
      stack_repr = std::format("[{}]", stack_content);
    }

    // Colorize based on state using type-safe approach
    ansi::Rgb state_color;

    // Use a hash-based approach that works with any state type
    // First check if it's an accepting state (highest priority)
    if (std::ranges::contains(definition_.accepting_states, node.state)) {
      // Accepting states get a special warm color
      state_color = terminal::colors::kSuccess;  // Green for accepting states
    } else {
      // For non-accepting states, use our specialized color generator.
      // Hash the table bytes, not the ID, so colors stay stable.
      std::size_t state_hash = std::hash<std::string>{}(
          std::string(automata::SymbolName(node.state)));
      state_color = GenerateNonAcceptingStateColor(state_hash);
    }

    // Colorize input symbol
    ansi::Rgb input_color =
        (input_sym == "λ")
            ? terminal::colors::kBannerText       // Mauve for lambda
            : terminal::colors::kSectionHeading;  // Yellow for others

    // Colorize stack
    ansi::Rgb stack_color =
        terminal::colors::kOptionName;  // Sky blue for stack

    // Check if this node is in the accepting path
    bool is_in_accepting_path =
        accepting_nodes.find(node_idx) != accepting_nodes.end();
    std::string accepting_marker =
        is_in_accepting_path
            ? " " + ansi::Format(ansi::Fg(terminal::colors::kSuccess), "{}",
                                 terminal::symbols::kSuccess)
            : "";

    // Show enhanced node info with colors
    std::string node_info;
    if (opt.trace.colors) {
      node_info = std::format(
          "[{}] pos:{} {} state:{} {} ({}){}", node_idx, node.input_position,
          ansi::Format(ansi::Fg(input_color), "{}", input_sym),
          ansi::Format(ansi::Fg(state_color), "{}", node.state),
          ansi::Format(ansi::Fg(stack_color), "{}", stack_repr),
          node.stack.size(), accepting_marker);
    } else {
      node_info = std::format(
          "[{}] pos:{} input:{} state:{} {} ({}){}", node_idx,
          node.input_position, input_sym, std::format("{}", node.state),
          stack_repr, node.stack.size(),
          is_in_accepting_path
              ? std::string(" ") + std::string(terminal::symbols::kSuccess)
              : "");
    }

    if (opt.trace.colors) {
      // Colorize tree connectors using config colors
      ansi::Rgb connector_color =
          terminal::colors::kProgress;  // Subtle gray from config
      std::string connector = is_last ? k_last : k_mid;
      sink(std::format("{}{} {}\n", prefix,
                       ansi::Format(ansi::Fg(connector_color), "{}", connector),
                       node_info));
    } else {
      sink(std::format("{}{} {}\n", prefix, is_last ? k_last : k_mid,
                       node_info));
    }

    // Print children with colorized tree connectors
    if (children.find(node_idx) != children.end()) {
      const auto& child_nodes = children[node_idx];
      for (std::size_t i = 0; i < child_nodes.size(); ++i) {
        bool last_child = (i == child_nodes.size() - 1);
        std::string child_prefix;
        if (opt.trace.colors) {
          // Colorize the tree connectors in the prefix
          std::string vertical_connector =
              is_last ? "    "
                      : ansi::Format(ansi::Fg(terminal::colors::kProgress),
                                     "{}", k_vert);
          child_prefix = prefix + vertical_connector;
        } else {
          child_prefix = prefix + (is_last ? "    " : k_vert);
        }
        print_tree(child_nodes[i], child_prefix, last_child);
      }
    }
  };

  // Find root nodes (nodes with no parent or parent not in explored_nodes)
  std::vector<std::size_t> roots;
  for (std::size_t node_idx : explored_nodes) {
    if (!nodes[node_idx].predecessor ||
        std::find(explored_nodes.begin(), explored_nodes.end(),
                  nodes[node_idx].predecessor->node_index) ==
            explored_nodes.end()) {
      roots.push_back(node_idx);
    }
  }

  // Print all trees
  for (std::size_t i = 0; i < roots.size(); ++i) {
    print_tree(roots[i], "", true);
  }
  if (printed < explored_nodes.size()) {
    sink(std::format("… ({} hidden below, raise --trace-limit to expand)\n",
                     explored_nodes.size() - printed));
  }
}

}  // namespace

void RenderTrace(const Definition& definition, const ExecutionEvent& event,
                 const TraceOptions& options) {
  if (!options.enabled) return;
  const RenderContext opt{event.track_witness, options};
  const Renderer renderer(definition);
  const auto sink = options.sink ? options.sink : [](std::string_view text) {
    std::print("{}", text);
  };
  const auto& node = event.nodes[event.node_index];
  switch (event.kind) {
    case EventKind::kStep: {
      if (!options.show_full_trace) break;
      const auto step = event.explored_nodes.size() - 1;
      if (step < options.step_limit) {
        std::optional<Transition> transition;
        if (node.predecessor)
          transition =
              definition.transitions[node.predecessor->transition_index];
        renderer.EmitTraceStep(node, event.input, step, transition, opt, false,
                               true);
      } else if (step == options.step_limit) {
        sink(std::format(
            "… (live trace stops at {} steps, raise --trace-limit to expand)\n",
            options.step_limit));
      }
      break;
    }
    case EventKind::kDeadEnd:
      if (options.show_full_trace) {
        const auto text = std::format(
            "\n{}Exploration: dead-end at position {} (state {}), no "
            "applicable transitions\n",
            options.colors ? std::string(terminal::symbols::kInfo) + " " : "",
            node.input_position, node.state);
        sink(options.colors
                 ? ansi::Format(ansi::Fg(terminal::colors::kInfo), "{}", text)
                 : text);
      }
      break;
    case EventKind::kAccepted:
      if (!event.track_witness) break;
      renderer.ReplayTracePath(event.nodes, event.node_index, event.input, opt);
      if (options.show_backtracking && !event.deadend_nodes.empty()) {
        const auto text = std::format(
            "\n{}Exploration summary: {} nodes explored, {} dead-ends found\n",
            options.colors ? std::string(terminal::symbols::kInfo) + " " : "",
            event.explored_nodes.size(), event.deadend_nodes.size());
        sink(options.colors
                 ? ansi::Format(ansi::Fg(terminal::colors::kInfo), "{}", text)
                 : text);
      }
      if (options.tree)
        renderer.ShowExplorationTree(event.nodes, event.explored_nodes,
                                     event.input, opt, {});
      break;
    case EventKind::kRejected:
      if (event.nodes.size() > 1)
        renderer.ShowRejectionTrace(event.nodes, event.node_index, event.input,
                                    opt, event.expansions);
      if (event.complete && options.show_full_trace)
        renderer.ShowExplorationTree(event.nodes, event.explored_nodes,
                                     event.input, opt, {});
      break;
  }
}

}  // namespace npda
