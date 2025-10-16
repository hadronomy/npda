#pragma once

#include <algorithm>
#include <concepts>
#include <cstdlib>
#include <deque>
#include <expected>
#include <filesystem>
#include <fstream>
#include <functional>
#include <mutex>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/color.h>
#include <fmt/format.h>
#include <fmt/ranges.h>

#include "config.h"
#include "turing/concepts.h"
#include "turing/node.h"
#include "turing/rule.h"

namespace turing {

// Machine configuration
struct TMConfig {
  std::size_t num_tapes = 1;
  TapeDirection tape_direction = TapeDirection::Bidirectional;
  OperationMode operation_mode = OperationMode::Independent;
  bool allow_stay = true;
};

struct Error {
  std::string message;
};

struct RunOptions {
  std::size_t max_steps = 1000000;
  bool track_witness = true;

  bool trace = false;
  std::function<void(std::string_view)> trace_sink = {};

  bool trace_colors = true;
  bool trace_compact = false;
  bool trace_explanations = false;

  bool show_config = true;
};

struct RunResult {
  bool accepted = false;
  std::size_t steps = 0;                              // total steps executed
  std::optional<std::vector<std::size_t>> witness;    // indices into rules_
  std::vector<std::vector<std::string>> final_tapes;  // final tape contents
  std::vector<std::size_t> final_head_positions;      // final head positions
  TMConfig config;                                    // TM configuration used
};

// Graphviz export options
struct GraphvizOptions {
  std::string rankdir = "LR";          // "LR", "TB", "RL", "BT"
  std::string fontname = "Helvetica";  // Font for nodes/edges
  bool color_accepting = true;         // Accepting states styled
  bool show_start_edge = true;         // Start arrow to start state
  bool show_legend = false;            // Include legend subgraph
  bool compact_labels = true;          // Single-line transition labels
};

template <Hashable State, Hashable TapeSym>
class TuringMachine {
 public:
  using rule_type = Rule<State, TapeSym>;  // always multi
  using MultiKey = std::pair<State, std::vector<TapeSym>>;

  struct MultiKeyHash {
    std::size_t operator()(const MultiKey& k) const noexcept {
      std::size_t h = std::hash<State>{}(k.first);
      constexpr std::size_t seed = 0x9e3779b97f4a7c15ULL;
      for (const auto& sym : k.second) {
        std::size_t v = std::hash<TapeSym>{}(sym);
        h ^= v + seed + (h << 6) + (h >> 2);
      }
      return h;
    }
  };

  class Builder {
   public:
    Builder& config(const TMConfig& cfg) {
      config_ = cfg;
      return *this;
    }
    Builder& num_tapes(std::size_t n) {
      config_.num_tapes = n;
      return *this;
    }
    Builder& tape_direction(TapeDirection dir) {
      config_.tape_direction = dir;
      return *this;
    }
    Builder& operation_mode(OperationMode mode) {
      config_.operation_mode = mode;
      return *this;
    }
    Builder& allow_stay(bool allow) {
      config_.allow_stay = allow;
      return *this;
    }
    Builder& start(State s) {
      start_ = s;
      return *this;
    }

    Builder& accepting(std::initializer_list<State> states) {
      accepting_.insert(accepting_.end(), states.begin(), states.end());
      return *this;
    }

    template <std::input_iterator It>
    requires std::same_as<std::iter_value_t<It>, State> Builder& accepting(It first, It last) {
      accepting_.insert(accepting_.end(), first, last);
      return *this;
    }

    template <std::ranges::input_range R>
    requires std::same_as<std::ranges::range_value_t<R>, State> Builder& accepting(R&& r) {
      for (auto&& s : r)
        accepting_.push_back(s);
      return *this;
    }

    Builder& blank(TapeSym sym) {
      blank_ = sym;
      return *this;
    }

    Builder& rule(const rule_type& r) {
      rules_.push_back(r);
      return *this;
    }

    [[nodiscard]] std::expected<TuringMachine, Error> build() & { return std::move(*this).build(); }

    [[nodiscard]] std::expected<TuringMachine, Error> build() && {
      if (!start_.has_value()) {
        return std::unexpected(Error{"start state not set in TuringMachine::Builder"});
      }
      if (!blank_.has_value()) {
        return std::unexpected(Error{"blank symbol not set in TuringMachine::Builder"});
      }

      TuringMachine tm;
      tm.config_ = config_;
      tm.start_ = *start_;
      tm.accepting_ = std::move(accepting_);
      tm.accepting_set_.reserve(tm.accepting_.size());
      for (const auto& s : tm.accepting_)
        tm.accepting_set_.insert(s);
      tm.blank_ = *blank_;
      tm.rules_ = std::move(rules_);

      tm.build_indices();
      if (tm.indices_error_)
        return std::unexpected(*tm.indices_error_);
      return tm;
    }

   private:
    TMConfig config_{};
    std::optional<State> start_{};
    std::vector<State> accepting_{};
    std::optional<TapeSym> blank_{};
    std::vector<rule_type> rules_{};
  };

  TuringMachine() : indices_once_(std::make_unique<std::once_flag>()) {}
  TuringMachine(TuringMachine&&) = default;
  TuringMachine& operator=(TuringMachine&&) = default;
  TuringMachine(const TuringMachine&) = delete;
  TuringMachine& operator=(const TuringMachine&) = delete;

  template <std::ranges::input_range R>
  requires std::same_as<std::ranges::range_value_t<R>, TapeSym> [[nodiscard]] std::
    expected<RunResult, Error>
    run(R&& rng, const RunOptions& opt = {}) const {
    std::vector<TapeSym> input;
    for (auto&& x : rng)
      input.push_back(x);

    build_indices();
    if (indices_error_)
      return std::unexpected(*indices_error_);

    if (opt.show_config)
      show_configuration(opt);

    return run_multi_tape(input, opt);
  }

  [[nodiscard]] const TMConfig& config() const noexcept { return config_; }

  std::expected<RunResult, Error> build_result(
    const MultiTapeNode<State, TapeSym>& acc_node,
    const std::vector<MultiTapeNode<State, TapeSym>>& nodes,
    std::size_t idx,
    std::size_t steps,
    const RunOptions& opt
  ) const;

  void emit_trace_step(
    const MultiTapeNode<State, TapeSym>& node,
    std::size_t step_num,
    const std::optional<rule_type>& rule = std::nullopt,
    const RunOptions& opt = {}
  ) const;

  void show_configuration(const RunOptions& opt) const;

  void replay_trace_path(
    const std::vector<MultiTapeNode<State, TapeSym>>& nodes,
    std::size_t final_idx,
    const RunOptions& opt
  ) const;

  // Graphviz export API
  [[nodiscard]] std::string to_graphviz_dot(const GraphvizOptions& opt = {}) const;

  [[nodiscard]] std::expected<void, Error>
    write_graphviz_dot(const std::filesystem::path& path, const GraphvizOptions& opt = {}) const;

  [[nodiscard]] std::expected<void, Error> export_graphviz_image(
    const std::filesystem::path& output_path,
    std::string_view format = "png",
    const GraphvizOptions& opt = {},
    std::string_view dot_exe = "dot"
  ) const;

 private:
  using NodeType = MultiTapeNode<State, TapeSym>;

  TMConfig config_{};
  State start_{};
  std::vector<State> accepting_{};
  mutable std::unordered_set<State> accepting_set_{};
  TapeSym blank_{};
  std::vector<rule_type> rules_{};

  mutable std::unordered_map<MultiKey, std::size_t, MultiKeyHash> multi_transitions_;
  mutable bool indices_built_ = false;
  mutable std::unique_ptr<std::once_flag> indices_once_;
  mutable std::optional<Error> indices_error_{};

  static constexpr std::size_t npos = static_cast<std::size_t>(-1);

  [[nodiscard]] static std::string join_symbols(const std::vector<TapeSym>& symbols) {
    return fmt::format("{}", fmt::join(symbols, ","));
  }

  [[nodiscard]] static std::string join_directions(const std::vector<Direction>& directions) {
    std::vector<char> chars;
    chars.reserve(directions.size());
    for (auto d : directions)
      chars.push_back(static_cast<char>(d));
    return fmt::format("{}", fmt::join(chars, ","));
  }

  // Escape string for Graphviz quoted labels/IDs
  [[nodiscard]] static std::string dot_escape(std::string_view s) {
    std::string out;
    out.reserve(s.size() + 8);
    for (char c : s) {
      if (c == '"' || c == '\\')
        out.push_back('\\');
      if (c == '\n') {
        out += "\\n";
        continue;
      }
      out.push_back(c);
    }
    return out;
  }

  [[nodiscard]] std::expected<RunResult, Error>
    run_multi_tape(const std::vector<TapeSym>& input, const RunOptions& opt) const;

  [[nodiscard]] static bool contains(const std::vector<State>& v, const State& s) {
    return std::ranges::find(v, s) != v.end();
  }

  void apply_multi_tape_transition(MultiTapeNode<State, TapeSym>& node, const rule_type& rule)
    const {
    const std::size_t num_tapes = config_.num_tapes;

    if (config_.operation_mode == OperationMode::Simultaneous) {
      for (std::size_t i = 0; i < num_tapes; ++i) {
        auto& tape = node.tapes[i];
        auto& pos = node.head_positions[i];
        if (pos >= tape.size())
          tape.resize(pos + 1, blank_);
        tape[pos] = rule.write[i];
      }
      for (std::size_t i = 0; i < num_tapes; ++i) {
        move_multi_head(node, i, rule.move[i]);
      }
      node.s = rule.to;
      return;
    }

    for (std::size_t i = 0; i < num_tapes; ++i) {
      auto& tape = node.tapes[i];
      auto& pos = node.head_positions[i];
      if (pos >= tape.size())
        tape.resize(pos + 1, blank_);
      tape[pos] = rule.write[i];
      move_multi_head(node, i, rule.move[i]);
    }
    node.s = rule.to;
  }

  void move_multi_head(MultiTapeNode<State, TapeSym>& node, std::size_t tape_idx, Direction dir)
    const {
    auto& pos = node.head_positions[tape_idx];

    if (dir == Direction::Right || (!config_.allow_stay && dir == Direction::Stay)) {
      ++pos;
      return;
    }
    if (dir == Direction::Stay)
      return;

    if (config_.tape_direction == TapeDirection::RightOnly)
      return;

    if (pos == 0) {
      node.tapes[tape_idx].insert(node.tapes[tape_idx].begin(), blank_);
      return;
    }
    --pos;
  }

  void build_indices() const {
    std::call_once(*indices_once_, [this]() {
      multi_transitions_.clear();
      indices_error_.reset();

      if (accepting_set_.empty() && !accepting_.empty()) {
        accepting_set_.reserve(accepting_.size());
        for (const auto& s : accepting_)
          accepting_set_.insert(s);
      }

      const bool left_disallowed = (config_.tape_direction == TapeDirection::RightOnly);
      const bool stay_disallowed = !config_.allow_stay;
      const auto num_tapes = config_.num_tapes;

      auto set_error = [&](std::string msg) {
        indices_error_ = Error{std::move(msg)};
      };

      for (std::size_t i = 0; i < rules_.size(); ++i) {
        const auto& r = rules_[i];

        const bool arity_ok =
          r.read.size() == num_tapes && r.write.size() == num_tapes && r.move.size() == num_tapes;
        if (!arity_ok) {
          set_error(fmt::format(
            "rule #{} arity mismatch: expected {} entries in read/write/move", i, num_tapes
          ));
          return;
        }

        if (stay_disallowed) {
          if (std::ranges::any_of(r.move, [](auto d) { return d == Direction::Stay; })) {
            set_error(fmt::format("rule #{} uses Stay while allow_stay=false", i));
            return;
          }
        }
        if (left_disallowed) {
          if (std::ranges::any_of(r.move, [](auto d) { return d == Direction::Left; })) {
            set_error(fmt::format("rule #{} uses Left while TapeDirection is Right-only", i));
            return;
          }
        }

        MultiKey key = std::make_pair(r.from, r.read);
        auto [it, ok] = multi_transitions_.emplace(std::move(key), i);
        if (!ok) {
          set_error(fmt::format(
            "duplicate multi-tape transition for state '{}' and symbols ({})",
            fmt::format("{}", r.from),
            join_symbols(r.read)
          ));
          return;
        }
      }

      indices_built_ = true;
    });
  }

  [[nodiscard]] bool is_accepting(const State& s) const {
    if (!accepting_set_.empty())
      return accepting_set_.find(s) != accepting_set_.end();
    return contains(accepting_, s);
  }

  [[nodiscard]] NodeType make_root(const std::vector<TapeSym>& input) const {
    NodeType node;
    node.s = start_;
    node.tapes.resize(config_.num_tapes);
    node.head_positions.resize(config_.num_tapes, 0);

    node.tapes[0] = input;
    for (std::size_t i = 1; i < config_.num_tapes; ++i)
      node.tapes[i] = {};

    node.parent = npos;
    node.rule_idx = std::nullopt;
    return node;
  }

  [[nodiscard]] std::vector<TapeSym> current_symbols_of(const NodeType& n) const {
    std::vector<TapeSym> symbols;
    symbols.reserve(config_.num_tapes);
    for (std::size_t i = 0; i < config_.num_tapes; ++i) {
      if (n.head_positions[i] < n.tapes[i].size()) {
        symbols.push_back(n.tapes[i][n.head_positions[i]]);
      } else {
        symbols.push_back(blank_);
      }
    }
    return symbols;
  }

  void apply_rule_indexed(NodeType& node, std::size_t rule_idx) const {
    apply_multi_tape_transition(node, rules_[rule_idx]);
  }

  [[nodiscard]] std::optional<std::size_t>
    find_transition_index(const State& s, const std::vector<TapeSym>& symbols) const {
    auto it = multi_transitions_.find(std::make_pair(s, symbols));
    if (it == multi_transitions_.end())
      return std::nullopt;
    return it->second;
  }

  [[nodiscard]] static std::function<void(std::string_view)> sink_of(const RunOptions& opt) {
    if (opt.trace_sink)
      return opt.trace_sink;
    return [](std::string_view s) {
      fmt::print("{}", s);
    };
  }

  void no_transition_trace(
    const NodeType& node,
    const std::vector<TapeSym>& symbols,
    const RunOptions& opt
  ) const {
    if (!opt.trace)
      return;
    auto sink = sink_of(opt);
    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(npda::config::colors::error),
        "\n{} No transition available for state '{}' and symbols ({})\n",
        npda::config::symbols::error,
        fmt::format("{}", node.s),
        join_symbols(symbols)
      ));
      return;
    }
    sink(fmt::format(
      "\nNo transition available for state '{}' and symbols ({})\n",
      fmt::format("{}", node.s),
      join_symbols(symbols)
    ));
  }
};

// ===== Visualization =====

template <Hashable State, Hashable TapeSym>
void TuringMachine<State, TapeSym>::emit_trace_step(
  const MultiTapeNode<State, TapeSym>& node,
  std::size_t step_num,
  const std::optional<rule_type>& rule,
  const RunOptions& opt
) const {
  if (!opt.trace)
    return;

  auto sink = sink_of(opt);
  std::string out;

  if (opt.trace_colors) {
    out +=
      fmt::format(fmt::fg(npda::config::colors::section_heading), "\n=== Step {} ===\n", step_num);
  } else {
    out += fmt::format("\n=== Step {} ===\n", step_num);
  }

  if (opt.trace_colors) {
    out += fmt::format(fmt::fg(npda::config::colors::info), "State: ");
    out += fmt::format(fmt::fg(npda::config::colors::success), "{}\n", fmt::format("{}", node.s));
  } else {
    out += fmt::format("State: {}\n", fmt::format("{}", node.s));
  }

  for (std::size_t tape_idx = 0; tape_idx < config_.num_tapes; ++tape_idx) {
    out += fmt::format("Tape {}: ", tape_idx + 1);

    const auto& tape = node.tapes[tape_idx];
    const std::size_t head_pos = node.head_positions[tape_idx];

    for (std::size_t i = 0; i < tape.size(); ++i) {
      if (i == head_pos) {
        if (opt.trace_colors) {
          out +=
            fmt::format(fmt::fg(npda::config::colors::warning), "[{}]", fmt::format("{}", tape[i]));
        } else {
          out += fmt::format("[{}]", fmt::format("{}", tape[i]));
        }
      } else {
        out += fmt::format(" {} ", fmt::format("{}", tape[i]));
      }
    }

    if (head_pos >= tape.size()) {
      if (opt.trace_colors) {
        out +=
          fmt::format(fmt::fg(npda::config::colors::success), " [{}]", fmt::format("{}", blank_));
      } else {
        out += fmt::format(" [{}]", fmt::format("{}", blank_));
      }
    }

    out += "\n       ";

    const std::size_t spaces = (head_pos < tape.size()) ? head_pos : tape.size();
    for (std::size_t i = 0; i < spaces; ++i)
      out += "    ";

    if (head_pos >= tape.size()) {
      for (std::size_t i = tape.size(); i < head_pos; ++i)
        out += "    ";
    }
    out += " ^  \n";
  }

  if (rule.has_value()) {
    if (opt.trace_colors) {
      out += fmt::format(fmt::fg(npda::config::colors::info), "Rule: ");
    } else {
      out += "Rule: ";
    }

    const auto& r = rule.value();
    const std::string rule_str = fmt::format(
      "({}, {}) → ({}, {}, {})",
      fmt::format("{}", r.from),
      join_symbols(r.read),
      fmt::format("{}", r.to),
      join_symbols(r.write),
      join_directions(r.move)
    );

    if (opt.trace_colors) {
      out += fmt::format(fmt::fg(npda::config::colors::example), "{}\n", rule_str);
    } else {
      out += fmt::format("{}\n", rule_str);
    }

    if (opt.trace_explanations) {
      const std::string explanation = fmt::format(
        "In state {}, read ({}), write ({}), move heads ({}), and go to "
        "state {}",
        fmt::format("{}", r.from),
        join_symbols(r.read),
        join_symbols(r.write),
        join_directions(r.move),
        fmt::format("{}", r.to)
      );

      if (opt.trace_colors) {
        out += fmt::format(fmt::fg(npda::config::colors::info), "{}\n", explanation);
      } else {
        out += fmt::format("{}\n", explanation);
      }
    }
  }

  sink(out);
}

template <Hashable State, Hashable TapeSym>
void TuringMachine<State, TapeSym>::show_configuration(const RunOptions& opt) const {
  if (!opt.show_config)
    return;

  auto sink = sink_of(opt);

  if (opt.trace_colors) {
    sink(fmt::format(
      fmt::fg(npda::config::colors::banner_text),
      "\n{} Turing Machine Configuration:\n",
      npda::config::symbols::info
    ));
  } else {
    sink("\nTuring Machine Configuration:\n");
  }

  auto print_config = [&](std::string_view key, std::string_view value) {
    if (opt.trace_colors) {
      sink(fmt::format(fmt::fg(npda::config::colors::info), "  {}: ", key));
      sink(fmt::format(fmt::fg(npda::config::colors::success), "{}\n", value));
      return;
    }
    sink(fmt::format("  {}: {}\n", key, value));
  };

  print_config("Number of Tapes", std::to_string(config_.num_tapes));
  print_config(
    "Tape Direction",
    config_.tape_direction == TapeDirection::Bidirectional ? "Bidirectional" : "Right-only"
  );
  print_config(
    "Operation Mode",
    config_.operation_mode == OperationMode::Simultaneous ? "Simultaneous" : "Independent"
  );
  print_config("Allow Stay Movement", config_.allow_stay ? "Yes" : "No");
  print_config("Blank Symbol", fmt::format("{}", blank_));
}

template <Hashable State, Hashable TapeSym>
std::expected<RunResult, Error> TuringMachine<State, TapeSym>::build_result(
  const MultiTapeNode<State, TapeSym>& acc_node,
  const std::vector<MultiTapeNode<State, TapeSym>>& nodes,
  std::size_t idx,
  std::size_t steps,
  const RunOptions& opt
) const {
  auto build_final = [&]() {
    std::vector<std::vector<std::string>> final_tapes;
    std::vector<std::size_t> final_head_positions;

    final_tapes.reserve(config_.num_tapes);
    final_head_positions.reserve(config_.num_tapes);

    for (std::size_t i = 0; i < config_.num_tapes; ++i) {
      std::vector<std::string> tape_str;
      tape_str.reserve(acc_node.tapes[i].size());
      for (const auto& sym : acc_node.tapes[i])
        tape_str.push_back(fmt::format("{}", sym));
      final_tapes.push_back(std::move(tape_str));
      final_head_positions.push_back(acc_node.head_positions[i]);
    }

    return std::make_pair(std::move(final_tapes), std::move(final_head_positions));
  };

  if (!opt.track_witness) {
    auto [final_tapes, final_heads] = build_final();
    return RunResult{true, steps, std::nullopt, final_tapes, final_heads, config_};
  }

  std::vector<std::size_t> path;
  for (std::size_t cur = idx; nodes[cur].parent != npos; cur = nodes[cur].parent) {
    if (!nodes[cur].rule_idx.has_value())
      break;
    path.push_back(*nodes[cur].rule_idx);
  }
  std::ranges::reverse(path);

  if (opt.trace)
    replay_trace_path(nodes, idx, opt);

  auto [final_tapes, final_heads] = build_final();
  return RunResult{true, steps, std::move(path), final_tapes, final_heads, config_};
}

template <Hashable State, Hashable TapeSym>
void TuringMachine<State, TapeSym>::replay_trace_path(
  const std::vector<MultiTapeNode<State, TapeSym>>& nodes,
  std::size_t final_idx,
  const RunOptions& opt
) const {
  if (!opt.trace || !opt.track_witness)
    return;

  std::vector<std::size_t> node_path_rev;
  std::vector<std::size_t> rule_path_rev;

  for (std::size_t cur = final_idx;;) {
    node_path_rev.push_back(cur);
    if (nodes[cur].parent == npos)
      break;
    if (nodes[cur].rule_idx.has_value())
      rule_path_rev.push_back(*nodes[cur].rule_idx);
    cur = nodes[cur].parent;
  }

  std::vector<std::size_t> node_path(node_path_rev.rbegin(), node_path_rev.rend());
  std::vector<std::size_t> rule_path(rule_path_rev.rbegin(), rule_path_rev.rend());

  auto sink = sink_of(opt);

  if (opt.trace_colors) {
    sink(fmt::format(
      fmt::fg(npda::config::colors::banner_text),
      "\n{} Accepting configuration found! Replaying {} steps...\n",
      npda::config::symbols::info,
      rule_path.size()
    ));
  } else {
    sink(fmt::format("\nAccepting configuration found! Replaying {} steps...\n", rule_path.size()));
  }

  std::size_t step_num = 0;
  if (!node_path.empty())
    emit_trace_step(nodes[node_path[0]], step_num, std::nullopt, opt);

  for (std::size_t i = 1; i < node_path.size(); ++i) {
    ++step_num;
    const std::size_t node_idx = node_path[i];
    const std::size_t rule_idx = rule_path[i - 1];

    std::optional<rule_type> rule_opt = rules_[rule_idx];
    emit_trace_step(nodes[node_idx], step_num, rule_opt, opt);
  }

  if (opt.trace_colors) {
    sink(fmt::format(
      fmt::fg(npda::config::colors::success),
      "\n{} Input accepted!\n",
      npda::config::symbols::success
    ));
  } else {
    sink("\nInput accepted!\n");
  }
}

template <Hashable State, Hashable TapeSym>
std::expected<RunResult, Error> TuringMachine<State, TapeSym>::run_multi_tape(
  const std::vector<TapeSym>& input,
  const RunOptions& opt
) const {
  auto finalize_reject = [&](std::size_t steps) -> std::expected<RunResult, Error> {
    return RunResult{false, steps, std::nullopt, {}, {}, config_};
  };

  if (!opt.track_witness) {
    NodeType current = make_root(input);
    std::size_t steps = 0;

    for (;;) {
      if (opt.trace)
        emit_trace_step(current, steps, std::nullopt, opt);
      if (is_accepting(current.s)) {
        std::vector<std::vector<std::string>> final_tapes;
        final_tapes.reserve(config_.num_tapes);
        std::vector<std::size_t> final_heads;
        final_heads.reserve(config_.num_tapes);

        for (std::size_t i = 0; i < config_.num_tapes; ++i) {
          std::vector<std::string> tape_str;
          tape_str.reserve(current.tapes[i].size());
          for (const auto& sym : current.tapes[i])
            tape_str.push_back(fmt::format("{}", sym));
          final_tapes.push_back(std::move(tape_str));
          final_heads.push_back(current.head_positions[i]);
        }

        return RunResult{
          true, steps, std::nullopt, std::move(final_tapes), std::move(final_heads), config_
        };
      }

      if (steps >= opt.max_steps)
        return std::unexpected(Error{"max_steps reached"});

      auto symbols = current_symbols_of(current);
      auto idx_opt = find_transition_index(current.s, symbols);
      if (!idx_opt) {
        no_transition_trace(current, symbols, opt);
        break;
      }

      apply_rule_indexed(current, *idx_opt);
      ++steps;
    }

    return finalize_reject(steps);
  }

  std::vector<NodeType> nodes;
  nodes.reserve(1024);
  nodes.push_back(make_root(input));

  std::deque<std::size_t> work;
  work.push_back(0);

  std::size_t steps = 0;

  while (!work.empty()) {
    const std::size_t idx = work.front();
    work.pop_front();
    const NodeType& current = nodes[idx];

    if (opt.trace)
      emit_trace_step(current, steps, std::nullopt, opt);
    if (is_accepting(current.s))
      return build_result(current, nodes, idx, steps, opt);
    if (steps >= opt.max_steps)
      return std::unexpected(Error{"max_steps reached"});

    auto symbols = current_symbols_of(current);
    auto idx_opt = find_transition_index(current.s, symbols);
    if (!idx_opt) {
      no_transition_trace(current, symbols, opt);
      break;
    }

    NodeType next = current;
    apply_rule_indexed(next, *idx_opt);
    ++steps;

    next.parent = idx;
    next.rule_idx = *idx_opt;

    nodes.push_back(std::move(next));
    work.push_back(nodes.size() - 1);
  }

  return finalize_reject(steps);
}

// ---------- Graphviz export ----------

template <Hashable State, Hashable TapeSym>
std::string TuringMachine<State, TapeSym>::to_graphviz_dot(const GraphvizOptions& opt) const {
  std::unordered_set<std::string> accepting_str;
  accepting_str.reserve(accepting_.size());
  for (const auto& s : accepting_) {
    accepting_str.insert(fmt::format("{}", s));
  }

  auto state_id = [](const State& s) {
    return fmt::format("{}", s);
  };

  auto label_for_rule = [&](const rule_type& r) {
    const auto reads = join_symbols(r.read);
    const auto writes = join_symbols(r.write);
    const auto moves = join_directions(r.move);

    if (opt.compact_labels) {
      return fmt::format(
        "[r:({}) | w:({}) | m:({})]", dot_escape(reads), dot_escape(writes), dot_escape(moves)
      );
    }
    return fmt::format(
      "read: ({})\\nwrite: ({})\\nmove: ({})",
      dot_escape(reads),
      dot_escape(writes),
      dot_escape(moves)
    );
  };

  std::string dot;
  dot += "digraph TM {\n";
  dot += fmt::format("  rankdir={};\n", opt.rankdir);
  dot += fmt::format("  node [fontname=\"{}\"];\n", dot_escape(opt.fontname));
  dot += fmt::format("  edge [fontname=\"{}\"];\n", dot_escape(opt.fontname));
  dot += "  labelloc=\"t\";\n";
  dot += "  labeljust=\"l\";\n";
  dot += fmt::format(
    "  label=\"Turing Machine\\nTapes: {} | Tape dir: {} | Mode: {} | "
    "AllowStay: {} | Blank: {}\";\n",
    config_.num_tapes,
    (config_.tape_direction == TapeDirection::Bidirectional ? "Bidirectional" : "Right-only"),
    (config_.operation_mode == OperationMode::Simultaneous ? "Simultaneous" : "Independent"),
    (config_.allow_stay ? "true" : "false"),
    dot_escape(fmt::format("{}", blank_))
  );

  if (opt.show_start_edge) {
    dot += "  __start [shape=point, width=0.15, label=\"\"];\n";
    dot += fmt::format("  __start -> \"{}\";\n", dot_escape(state_id(start_)));
  }

  std::unordered_set<std::string> all_states;
  all_states.insert(state_id(start_));
  for (const auto& s : accepting_)
    all_states.insert(state_id(s));
  for (const auto& r : rules_) {
    all_states.insert(state_id(r.from));
    all_states.insert(state_id(r.to));
  }

  for (const auto& s_str : all_states) {
    const bool is_accept = accepting_str.find(s_str) != accepting_str.end();
    if (is_accept && opt.color_accepting) {
      dot += fmt::format(
        "  \"{}\" [shape=doublecircle, color=\"#2e7d32\", "
        "fontcolor=\"#2e7d32\"];\n",
        dot_escape(s_str)
      );
    } else if (is_accept) {
      dot += fmt::format("  \"{}\" [shape=doublecircle];\n", dot_escape(s_str));
    } else {
      dot += fmt::format("  \"{}\" [shape=circle];\n", dot_escape(s_str));
    }
  }

  for (const auto& r : rules_) {
    const auto from_str = dot_escape(state_id(r.from));
    const auto to_str = dot_escape(state_id(r.to));
    const auto elabel = dot_escape(label_for_rule(r));
    dot += fmt::format("  \"{}\" -> \"{}\" [label=\"{}\"];\n", from_str, to_str, elabel);
  }

  if (opt.show_legend) {
    dot += "  subgraph cluster_legend {\n";
    dot += "    label = \"Legend\";\n";
    dot += "    style = dashed;\n";
    dot += "    legend_accept [label=\"accepting\", shape=doublecircle];\n";
    dot += "    legend_state [label=\"state\", shape=circle];\n";
    dot +=
      "    legend_t [label=\"transition: [r:(... ) | w:(... ) | m:(... "
      ")]\", shape=box];\n";
    dot += "    legend_state -> legend_state [style=invis];\n";
    dot += "  }\n";
  }

  dot += "}\n";
  return dot;
}

template <Hashable State, Hashable TapeSym>
std::expected<void, Error> TuringMachine<State, TapeSym>::write_graphviz_dot(
  const std::filesystem::path& path,
  const GraphvizOptions& opt
) const {
  std::error_code ec;
  if (path.has_parent_path()) {
    std::filesystem::create_directories(path.parent_path(), ec);
    if (ec) {
      return std::unexpected(
        Error{fmt::format("failed to create directories '{}': {}", path.string(), ec.message())}
      );
    }
  }
  std::ofstream ofs(path, std::ios::binary);
  if (!ofs) {
    return std::unexpected(Error{fmt::format("failed to open output '{}'", path.string())});
  }
  ofs << to_graphviz_dot(opt);
  if (!ofs) {
    return std::unexpected(Error{fmt::format("failed to write DOT to '{}'", path.string())});
  }
  return {};
}

template <Hashable State, Hashable TapeSym>
std::expected<void, Error> TuringMachine<State, TapeSym>::export_graphviz_image(
  const std::filesystem::path& output_path,
  std::string_view format,
  const GraphvizOptions& opt,
  std::string_view dot_exe
) const {
  std::error_code ec;
  const auto tmp_dir = std::filesystem::temp_directory_path(ec);
  if (ec) {
    return std::unexpected(Error{fmt::format("failed to get temp directory: {}", ec.message())});
  }
  const auto tmp_dot = tmp_dir / fmt::format("tm_{}.dot", std::hash<const void*>{}(this));

  if (auto w = write_graphviz_dot(tmp_dot, opt); !w) {
    return std::unexpected(w.error());
  }

  const auto cmd = fmt::format(
    "\"{}\" -T{} \"{}\" -o \"{}\"",
    std::string(dot_exe),
    std::string(format),
    tmp_dot.string(),
    output_path.string()
  );
  const int rc = std::system(cmd.c_str());
  if (rc != 0) {
    return std::unexpected(Error{fmt::format("graphviz 'dot' failed (exit code {}): {}", rc, cmd)});
  }
  return {};
}

}  // namespace turing