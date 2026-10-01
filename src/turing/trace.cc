#include "turing/trace.h"

#include <algorithm>
#include <cstddef>
#include <format>
#include <functional>
#include <optional>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "terminal/ansi.h"
#include "terminal/palette.h"
#include "terminal/ui.h"
#include "turing/machine.h"
#include "turing/transition_text.h"

namespace turing {

namespace {

struct RenderContext {
  TraceOptions trace;
  bool track_witness;
  bool show_config;
};

class Renderer {
 public:
  explicit Renderer(const Definition& definition) : definition_(definition) {}

  void EmitTraceStep(const TapeConfiguration& node, std::size_t step_num,
                     const std::optional<Transition>& rule,
                     const RenderContext& opt) const;
  void ShowConfiguration(const RenderContext& opt) const;
  void ReplayTracePath(std::span<const TapeConfiguration> nodes,
                       std::size_t final_idx, const RenderContext& opt) const;

 private:
  const Definition& definition_;

  static std::function<void(std::string_view)> SinkOf(
      const RenderContext& opt) {
    return opt.trace.sink ? opt.trace.sink : [](std::string_view text) {
      std::print("{}", text);
    };
  }
};

void Renderer::EmitTraceStep(const TapeConfiguration& node,
                             std::size_t step_num,
                             const std::optional<Transition>& rule,
                             const RenderContext& opt) const {
  if (!opt.trace.enabled) return;

  auto sink = SinkOf(opt);
  std::string out;

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
  out += "\n" + ui::Rule(title) + "\n";

  // Current state. Bold always; green only when accepting.
  out += ansi::Format(st(faint), "state: ");
  if (std::ranges::contains(definition_.accepting_states, node.state)) {
    out += ansi::Format(
        st(ansi::Fg(terminal::colors::kSuccess) | ansi::Emphasis::kBold),
        "{}\n", std::format("{}", node.state));
  } else {
    out += ansi::Format(st(bold), "{}\n", std::format("{}", node.state));
  }

  constexpr std::size_t k_window = 8;
  const std::string k_ellipsis = ansi::UnicodeEnabled() ? "… " : "... ";
  // Code-point width. Marker math uses this, never byte length.
  const auto cpwidth = [](std::string_view s) {
    std::size_t n = 0;
    for (unsigned char c : s)
      if ((c & 0xC0) != 0x80) ++n;
    return n;
  };
  const auto cpcut = [](std::string_view s, std::size_t n) {
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
  };

  for (std::size_t tape_idx = 0; tape_idx < definition_.config.num_tapes;
       ++tape_idx) {
    const std::string tag = std::format("tape{}: ", tape_idx + 1);
    out += ansi::Format(st(faint), "{}", tag);

    const auto& tape = node.tapes[tape_idx].cells;
    const std::size_t head_pos = node.tapes[tape_idx].head_position;
    // Logical cells: the tape plus blanks out to the head. The marker
    // always lands on a printed cell, never in a gap.
    const std::size_t logical = std::max(tape.size(), head_pos + 1);
    const std::size_t head_cell = head_pos;
    const std::size_t lo = (head_cell > k_window) ? head_cell - k_window : 0;
    const std::size_t hi = std::min(logical, head_cell + k_window + 1);

    const auto cell_text = [&](std::size_t i) {
      if (i < tape.size()) return cpcut(std::format("{}", tape[i]), 6);
      return cpcut(std::format("{}", definition_.blank_symbol), 6);
    };

    std::size_t head_col = 0;
    std::size_t col = 0;
    if (lo > 0) {
      out += k_ellipsis;
      col += cpwidth(k_ellipsis);
    }
    for (std::size_t i = lo; i < hi; ++i) {
      const std::string piece = (i == head_cell) ? "[" + cell_text(i) + "]"
                                                 : " " + cell_text(i) + " ";
      if (i == head_cell) head_col = col + 1 + cpwidth(cell_text(i)) / 2;
      const bool is_head = (i == head_cell);
      const bool is_blank = (i >= tape.size());
      if (is_head) {
        const auto fg =
            is_blank ? terminal::colors::kSuccess : terminal::colors::kWarning;
        out +=
            ansi::Format(st(ansi::Fg(fg) | ansi::Emphasis::kBold), "{}", piece);
      } else if (is_blank) {
        out += ansi::Format(st(faint), "{}", piece);
      } else {
        out += piece;
      }
      col += cpwidth(piece);
    }
    if (hi < logical) out += k_ellipsis;

    out += "\n";
    out += std::string(tag.size(), ' ');
    out += std::string(head_col, ' ');
    out += "^\n";
  }

  if (rule.has_value()) {
    const auto& r = rule.value();
    const std::string arr = ui::Arrow();
    out += ansi::Format(st(faint), "rule: ");
    out += std::format("({}, {}) ", std::format("{}", r.from_state),
                       JoinReads(r.tapes));
    out += ansi::Format(st(bold), "{} ({}, {}, {})", arr,
                        std::format("{}", r.to_state), JoinWrites(r.tapes),
                        JoinMoves(r.tapes));
    out += "\n";

    if (opt.trace.explanations) {
      out += ansi::Format(st(faint),
                          "In state {}, read ({}), write ({}), move heads "
                          "({}), and go to state {}\n",
                          std::format("{}", r.from_state), JoinReads(r.tapes),
                          JoinWrites(r.tapes), JoinMoves(r.tapes),
                          std::format("{}", r.to_state));
    }
  }

  sink(out);
}

void Renderer::ShowConfiguration(const RenderContext& opt) const {
  if (!opt.show_config) return;

  auto sink = SinkOf(opt);

  const std::string kv = std::format(
      "tapes: {}, direction: {}, mode: {}, stay: {}, blank: {}",
      definition_.config.num_tapes,
      definition_.config.tape_direction == TapeDirection::kBidirectional
          ? "bidirectional"
          : "right-only",
      definition_.config.operation_mode == OperationMode::kSimultaneous
          ? "simultaneous"
          : "independent",
      definition_.config.allow_stay ? "yes" : "no",
      std::format("{}", definition_.blank_symbol));
  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kInfo), "{}", kv));
    sink("\n");
    return;
  }
  sink(kv + "\n");
}

void Renderer::ReplayTracePath(std::span<const TapeConfiguration> nodes,
                               std::size_t final_idx,
                               const RenderContext& opt) const {
  if (!opt.trace.enabled || !opt.track_witness) return;

  std::vector<std::size_t> node_path_rev;
  std::vector<std::size_t> rule_path_rev;

  for (std::size_t cur = final_idx;;) {
    node_path_rev.push_back(cur);
    if (!nodes[cur].predecessor) break;
    rule_path_rev.push_back(nodes[cur].predecessor->transition_index);
    cur = nodes[cur].predecessor->node_index;
  }

  std::vector<std::size_t> node_path(node_path_rev.rbegin(),
                                     node_path_rev.rend());
  std::vector<std::size_t> transition_path(rule_path_rev.rbegin(),
                                           rule_path_rev.rend());

  auto sink = SinkOf(opt);

  if (opt.trace.colors) {
    sink(ansi::Format(
        ansi::Fg(terminal::colors::kBannerText),
        "\n{} Accepting configuration found! Replaying {} steps...\n",
        terminal::symbols::kInfo, transition_path.size()));
  } else {
    sink(std::format("\nAccepting configuration found! Replaying {} steps...\n",
                     transition_path.size()));
  }

  if (!opt.trace.show_full_trace) {
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
          sink(ui::Rule(
                   std::format("… step {} · {}", j,
                               std::format("{}", nodes[node_path[j]].state))) +
               "\n");
        }
        sink(std::format("… ({} steps hidden, raise --trace-limit to expand)\n",
                         i - show[k - 1] - 1));
      }
      std::optional<Transition> rule_opt = std::nullopt;
      if (i > 0 && k > 0 && i == show[k - 1] + 1)
        rule_opt = definition_.transitions[transition_path[i - 1]];
      EmitTraceStep(nodes[node_path[i]], step_num++, rule_opt, opt);
    }
  }

  if (opt.trace.colors) {
    sink(ansi::Format(ansi::Fg(terminal::colors::kSuccess),
                      "\n{} Input accepted!\n", terminal::symbols::kSuccess));
  } else {
    sink("\nInput accepted!\n");
  }
}

}  // namespace

void ShowConfiguration(const Definition& definition,
                       const TraceOptions& options) {
  Renderer(definition).ShowConfiguration(RenderContext{options, true, true});
}

void RenderTrace(const Definition& definition, const ExecutionEvent& event,
                 const TraceOptions& options) {
  if (!options.enabled) return;
  const Renderer renderer(definition);
  const RenderContext context{options, event.track_witness, false};
  const auto& node = event.nodes[event.node_index];
  switch (event.kind) {
    case EventKind::kStep:
      if (options.show_full_trace || !event.track_witness) {
        std::optional<Transition> transition;
        if (event.track_witness && node.predecessor)
          transition =
              definition.transitions[node.predecessor->transition_index];
        renderer.EmitTraceStep(node, event.steps, transition, context);
      }
      break;
    case EventKind::kAccepted:
      if (event.track_witness)
        renderer.ReplayTracePath(event.nodes, event.node_index, context);
      break;
    case EventKind::kNoTransition: {
      std::vector<Symbol> symbols;
      for (std::size_t i = 0; i < node.tapes.size(); ++i) {
        const auto& tape = node.tapes[i].cells;
        const auto position = node.tapes[i].head_position;
        symbols.push_back(position < tape.size() ? tape[position]
                                                 : definition.blank_symbol);
      }
      const auto sink =
          options.sink ? options.sink
                       : [](std::string_view text) { std::print("{}", text); };
      if (options.colors) {
        sink(ansi::Format(
            ansi::Fg(terminal::colors::kError),
            "\n{} No transition available for state '{}' and symbols ({})\n",
            terminal::symbols::kError, node.state, ansi::Join(symbols, ",")));
      } else {
        sink(std::format(
            "\nNo transition available for state '{}' and symbols ({})\n",
            node.state, ansi::Join(symbols, ",")));
      }
      break;
    }
  }
}

}  // namespace turing
