// C++23 NPDA (nondeterministic pushdown automaton), header-only.
// - Epsilon transitions (input == nullopt)
// - Accept by final state, empty stack, or both (after full input)
// - BFS/DFS exploration
// - Loop-avoidance with visited configurations
// - Optional witness reconstruction (indices of rules taken)

#pragma once

#include <fmt/color.h>
#include <fmt/format.h>
#include <algorithm>
#include <concepts>
#include <deque>
#include <expected>
#include <functional>
#include <optional>
#include <ranges>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>
#include "config.h"

namespace npda {

enum class AcceptBy { FinalState, EmptyStack, Both };

struct Error {
  std::string message;
};

struct RunOptions {
  // If true, BFS (returns a shortest-transition witness). If false, DFS.
  bool bfs = true;

  // Hard limit on expanded transitions to prevent blow-ups/infinite loops.
  std::size_t max_expansions = 1000000;

  // Track and return a witness (sequence of rule indices). Costs memory.
  bool track_witness = true;

  // Pretty, colored per-step trace diagrams printed during run.
  bool trace = false;
  // Optional sink; if not set and trace=true, prints via fmt::print.
  std::function<void(std::string_view)> trace_sink = {};

  // Trace formatting options
  bool trace_colors = true;
  bool trace_compact = false;
};

struct RunResult {
  bool accepted = false;
  std::size_t expansions = 0;                       // expanded transitions (search work)
  std::optional<std::vector<std::size_t>> witness;  // indices into rules_
};

template <typename T>
concept Hashable = requires(T t) {
  { std::hash<T>{}(t) } -> std::convertible_to<std::size_t>;
};

template <Hashable State, Hashable Input, Hashable StackSym>
struct Rule {
  State from{};
  std::optional<Input> input{};         // std::nullopt = epsilon
  std::optional<StackSym> stack_top{};  // if set, must match top and pop
  State to{};
  std::vector<StackSym> push{};  // left-to-right; last becomes new top
};

template <Hashable State, Hashable Input, Hashable StackSym>
class NPDA {
 public:
  using rule_type = Rule<State, Input, StackSym>;

  class Builder {
   public:
    Builder& start(State s) {
      start_ = s;
      return *this;
    }

    Builder& accepting(std::initializer_list<State> states) {
      accepting_.insert(accepting_.end(), states.begin(), states.end());
      return *this;
    }

    Builder& accept_by(AcceptBy policy) {
      policy_ = policy;
      return *this;
    }

    Builder& stack_bottom(StackSym sym) {
      bottom_ = sym;
      return *this;
    }

    Builder& rule(const rule_type& r) {
      rules_.push_back(r);
      return *this;
    }

    [[nodiscard]] std::expected<NPDA, Error> build() & { return std::move(*this).build(); }

    [[nodiscard]] std::expected<NPDA, Error> build() && {
      if (!start_.has_value()) {
        return std::unexpected(Error{"start state not set in NPDA::Builder"});
      }
      if (!bottom_.has_value()) {
        return std::unexpected(Error{"stack bottom not set in NPDA::Builder"});
      }

      NPDA m;
      m.start_ = *start_;
      m.accepting_ = std::move(accepting_);
      m.policy_ = policy_;
      m.bottom_ = *bottom_;
      m.rules_ = std::move(rules_);
      return m;
    }

   private:
    std::optional<State> start_{};
    std::vector<State> accepting_{};
    AcceptBy policy_{AcceptBy::FinalState};
    std::optional<StackSym> bottom_{};
    std::vector<rule_type> rules_{};
  };

  NPDA() = default;

  // Run on any input range; acceptance is checked after all input is consumed.
  template <std::ranges::input_range R>
  requires
    std::same_as<std::ranges::range_value_t<R>, Input> [[nodiscard]] std::expected<RunResult, Error>
    run(R&& rng, const RunOptions& opt = {}) const {
    std::vector<Input> input;
    for (auto&& x : rng)
      input.push_back(x);

    // Node in search graph
    struct Node {
      State s{};
      std::size_t pos = 0;  // index into input
      std::vector<StackSym> stack{};
      std::size_t parent = npos;
      std::optional<std::size_t> rule_idx{};
    };

    // Key for visited set
    struct Key {
      State s{};
      std::size_t pos = 0;
      std::vector<StackSym> stack{};
      bool operator==(const Key& o) const { return s == o.s && pos == o.pos && stack == o.stack; }
    };

    struct KeyHash {
      std::size_t operator()(const Key& k) const {
        std::size_t h = std::hash<State>{}(k.s);
        h = combine(h, std::hash<std::size_t>{}(k.pos));
        h = combine(h, vec_hash(k.stack));
        return h;
      }

      static std::size_t combine(std::size_t a, std::size_t b) {
        // 64-bit mix (splitmix64-ish)
        std::size_t x = a ^ (b + 0x9e3779b97f4a7c15ULL + (a << 6) + (a >> 2));
        return x;
      }

      static std::size_t vec_hash(const std::vector<StackSym>& v) {
        std::size_t h = 0xcbf29ce484222325ULL;  // FNV offset
        for (const auto& e : v) {
          std::size_t eh = std::hash<StackSym>{}(e);
          h ^= eh;
          h *= 0x100000001b3ULL;  // FNV prime
        }
        return h;
      }
    };

    std::vector<Node> nodes;
    nodes.reserve(1024);

    auto make_root = [&] {
      Node n;
      n.s = start_;
      n.pos = 0;
      n.stack.clear();
      n.stack.push_back(bottom_);
      n.parent = npos;
      n.rule_idx = std::nullopt;
      return n;
    };

    std::deque<std::size_t> work;
    nodes.push_back(make_root());
    work.push_back(0);

    std::unordered_set<Key, KeyHash> visited;
    visited.reserve(4096);
    visited.insert(Key{nodes[0].s, nodes[0].pos, nodes[0].stack});

    std::size_t expansions = 0;

    auto accept = [&](const Node& n) -> bool {
      const bool at_end = (n.pos == input.size());
      const bool by_state = contains(accepting_, n.s);
      const bool by_stack = (n.stack.size() == 1 && n.stack.back() == bottom_);
      switch (policy_) {
        case AcceptBy::FinalState:
          return at_end && by_state;
        case AcceptBy::EmptyStack:
          return at_end && by_stack;
        case AcceptBy::Both:
          return at_end && by_state && by_stack;
      }
      return false;
    };

    auto push_node = [&](Node&& n, std::size_t parent_idx, std::size_t rule_idx) -> void {
      Key k{n.s, n.pos, n.stack};
      if (visited.insert(std::move(k)).second) {
        n.parent = parent_idx;
        n.rule_idx = rule_idx;
        nodes.push_back(std::move(n));
        if (opt.bfs) {
          work.push_back(nodes.size() - 1);
        } else {
          work.push_back(nodes.size() - 1);  // LIFO achieved by popping from back
        }
      }
    };

    while (!work.empty()) {
      std::size_t idx = opt.bfs ? work.front() : work.back();
      if (opt.bfs)
        work.pop_front();
      else
        work.pop_back();
      const Node cur = nodes[idx];  // copy for isolation

      if (accept(cur)) {
        return build_result(cur, nodes, idx, expansions, opt, input);
      }

      if (expansions >= opt.max_expansions) {
        return std::unexpected(Error{"max_expansions reached"});
      }

      // Generate epsilon transitions
      for (std::size_t ri = 0; ri < rules_.size(); ++ri) {
        const auto& r = rules_[ri];
        if (r.from != cur.s)
          continue;
        if (r.input.has_value())
          continue;  // only epsilons here
        if (!stack_matches(cur.stack, r.stack_top))
          continue;

        Node nxt = cur;
        apply_stack(nxt.stack, r);
        nxt.s = r.to;
        ++expansions;
        push_node(std::move(nxt), idx, ri);
        if (expansions >= opt.max_expansions)
          return std::unexpected(Error{"max_expansions reached"});
      }

      // Generate consuming transitions (if input left)
      if (cur.pos < input.size()) {
        const Input sym = input[cur.pos];
        for (std::size_t ri = 0; ri < rules_.size(); ++ri) {
          const auto& r = rules_[ri];
          if (r.from != cur.s)
            continue;
          if (!r.input.has_value() || r.input.value() != sym)
            continue;
          if (!stack_matches(cur.stack, r.stack_top))
            continue;

          Node nxt = cur;
          apply_stack(nxt.stack, r);
          nxt.s = r.to;
          nxt.pos = cur.pos + 1;
          ++expansions;
          push_node(std::move(nxt), idx, ri);
          if (expansions >= opt.max_expansions)
            return std::unexpected(Error{"max_expansions reached"});
        }
      }
    }

    return RunResult{false, expansions, std::nullopt};
  }
  // Trace visualization methods
  template <typename NodeT>
  void emit_trace_step(
    const NodeT& node,
    const std::vector<Input>& input,
    std::size_t step_num,
    const std::optional<rule_type>& rule = std::nullopt,
    const RunOptions& opt = {}
  ) const {
    if (!opt.trace)
      return;

    auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
      fmt::print("{}", s);
    };

    std::string output;

    // Header with step number
    if (opt.trace_colors) {
      output +=
        fmt::format(fmt::fg(config::colors::section_heading), "\n=== Step {} ===\n", step_num);
    } else {
      output += fmt::format("\n=== Step {} ===\n", step_num);
    }

    // Current state
    if (opt.trace_colors) {
      output += fmt::format(fmt::fg(config::colors::info), "State: ");
      output += fmt::format(fmt::fg(config::colors::success), "{}\n", fmt::format("{}", node.s));
    } else {
      output += fmt::format("State: {}\n", fmt::format("{}", node.s));
    }

    // Input tape with pointer
    output += "Input: ";
    for (std::size_t i = 0; i < input.size(); ++i) {
      if (i == node.pos) {
        if (opt.trace_colors) {
          output +=
            fmt::format(fmt::fg(config::colors::warning), "[{}]", fmt::format("{}", input[i]));
        } else {
          output += fmt::format("[{}]", fmt::format("{}", input[i]));
        }
      } else {
        output += fmt::format(" {} ", fmt::format("{}", input[i]));
      }
    }
    if (node.pos >= input.size()) {
      if (opt.trace_colors) {
        output += fmt::format(fmt::fg(config::colors::success), " [END]");
      } else {
        output += " [END]";
      }
    }
    output += "\n";

    // Stack visualization
    output += "Stack: ";
    if (node.stack.empty()) {
      output += "(empty)\n";
    } else {
      // Show stack top on the right (conventional)
      for (std::size_t i = 0; i < node.stack.size(); ++i) {
        std::size_t stack_idx = node.stack.size() - 1 - i;  // Reverse to show top first
        if (i == 0) {
          if (opt.trace_colors) {
            output += fmt::format(
              fmt::fg(config::colors::warning), "[{}]", fmt::format("{}", node.stack[stack_idx])
            );
          } else {
            output += fmt::format("[{}]", fmt::format("{}", node.stack[stack_idx]));
          }
        } else {
          output += fmt::format(" {} ", fmt::format("{}", node.stack[stack_idx]));
        }
      }
      output += "\n";

      // Visual stack representation
      if (!opt.trace_compact) {
        output += "       ";
        for (std::size_t i = 0; i < node.stack.size(); ++i) {
          if (node.stack.size() == 1) {
            output += "┌───┐";
            break;
          }
          if (i == 0) {
            output += "┌───┬";
            continue;
          }
          if (i != node.stack.size() - 1) {
            output += "───┬";
            continue;
          }
          output += "───┐";
        }
        output += "\n       ";
        for (std::size_t i = 0; i < node.stack.size(); ++i) {
          std::size_t stack_idx = node.stack.size() - 1 - i;
          if (i == 0) {
            if (opt.trace_colors) {
              output += fmt::format(
                fmt::fg(config::colors::warning), "│ {} │", fmt::format("{}", node.stack[stack_idx])
              );
            } else {
              output += fmt::format("│ {} │", fmt::format("{}", node.stack[stack_idx]));
            }
          } else {
            output += fmt::format(" {} │", fmt::format("{}", node.stack[stack_idx]));
          }
        }
        output += "\n       ";
        for (std::size_t i = 0; i < node.stack.size(); ++i) {
          if (node.stack.size() == 1) {
            output += "└───┘";
            break;
          }
          if (i == 0) {
            output += "└───┴";
            continue;
          }
          if (i != node.stack.size() - 1) {
            output += "───┴";
            continue;
          }
          output += "───┘";
        }
        output += "\n";
      }
    }

    // Rule information
    if (rule.has_value()) {
      const auto& r = rule.value();
      if (opt.trace_colors) {
        output += fmt::format(fmt::fg(config::colors::info), "Rule: ");
      } else {
        output += "Rule: ";
      }

      // Format rule nicely
      std::string rule_str = fmt::format("{} → ", fmt::format("{}", r.from));

      // Input symbol
      if (r.input.has_value()) {
        rule_str += fmt::format("{}, ", fmt::format("{}", r.input.value()));
      } else {
        rule_str += "ε, ";
      }

      // Stack operation
      if (r.stack_top.has_value()) {
        rule_str += fmt::format("pop({}) → ", fmt::format("{}", r.stack_top.value()));
      } else {
        rule_str += "nop → ";
      }

      rule_str += fmt::format("{}, ", fmt::format("{}", r.to));

      // Push symbols
      if (r.push.empty()) {
        rule_str += "push()";
      } else {
        rule_str += "push(";
        for (std::size_t i = 0; i < r.push.size(); ++i) {
          rule_str += fmt::format("{}", fmt::format("{}", r.push[i]));
          if (i + 1 < r.push.size())
            rule_str += ", ";
        }
        rule_str += ")";
      }

      if (opt.trace_colors) {
        output += fmt::format(fmt::fg(config::colors::example), "{}\n", rule_str);
      } else {
        output += fmt::format("{}\n", rule_str);
      }
    }

    sink(output);
  }

  template <typename NodeT>
  std::expected<RunResult, Error> build_result(
    [[maybe_unused]] const NodeT& acc_node,
    const std::vector<NodeT>& nodes,
    std::size_t idx,
    std::size_t expansions,
    const RunOptions& opt,
    const std::vector<Input>& input
  ) const {
    if (!opt.track_witness) {
      return RunResult{true, expansions, std::nullopt};
    }
    std::vector<std::size_t> path;
    std::size_t cur = idx;
    while (nodes[cur].parent != npos) {
      if (nodes[cur].rule_idx.has_value()) {
        path.push_back(*nodes[cur].rule_idx);
      } else {
        break;
      }
      cur = nodes[cur].parent;
    }
    std::reverse(path.begin(), path.end());

    // Replay the trace if tracing is enabled
    if (opt.trace) {
      replay_trace_path(nodes, idx, input, opt);
    }

    return RunResult{true, expansions, std::move(path)};
  }

  template <typename NodeT>
  void replay_trace_path(
    const std::vector<NodeT>& nodes,
    std::size_t final_idx,
    const std::vector<Input>& input,
    const RunOptions& opt
  ) const {
    if (!opt.trace || !opt.track_witness)
      return;

    // Reconstruct node and rule paths from root to final
    std::vector<std::size_t> node_path_rev;
    std::vector<std::size_t> rule_path_rev;  // rules applied to reach each node (except root)

    std::size_t cur = final_idx;
    while (true) {
      node_path_rev.push_back(cur);
      if (nodes[cur].parent == npos)
        break;
      if (nodes[cur].rule_idx.has_value())
        rule_path_rev.push_back(*nodes[cur].rule_idx);
      cur = nodes[cur].parent;
    }

    std::vector<std::size_t> node_path(node_path_rev.rbegin(), node_path_rev.rend());
    std::vector<std::size_t> rule_path(rule_path_rev.rbegin(), rule_path_rev.rend());

    auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
      fmt::print("{}", s);
    };

    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(config::colors::banner_text),
        "\n🎯 Accepting path found! Replaying {} steps...\n",
        rule_path.size()
      ));
    } else {
      sink(fmt::format("\nAccepting path found! Replaying {} steps...\n", rule_path.size()));
    }

    // Emit each step showing the state after applying the rule
    std::size_t step_num = 0;
    for (std::size_t i = 1; i < node_path.size(); ++i) {
      ++step_num;
      const std::size_t node_idx = node_path[i];
      const std::size_t rule_idx = rule_path[i - 1];
      emit_trace_step(nodes[node_idx], input, step_num, rules_[rule_idx], opt);
    }

    // Show final accepting state (if not already shown)
    if (node_path.empty() || node_path.back() != final_idx) {
      emit_trace_step(nodes[final_idx], input, step_num + 1, std::nullopt, opt);
      ++step_num;
    }

    if (opt.trace_colors) {
      sink(fmt::format(fmt::fg(config::colors::success), "\n✅ Input accepted!\n"));
    } else {
      sink("\nInput accepted!\n");
    }
  }

  State start_{};
  std::vector<State> accepting_{};
  AcceptBy policy_{AcceptBy::FinalState};
  StackSym bottom_{};
  std::vector<rule_type> rules_{};

  // Constants and helper functions
  static constexpr std::size_t npos = static_cast<std::size_t>(-1);

  static bool contains(const std::vector<State>& v, const State& s) {
    return std::find(v.begin(), v.end(), s) != v.end();
  }

  static bool
    stack_matches(const std::vector<StackSym>& st, const std::optional<StackSym>& need_top) {
    if (!need_top.has_value())
      return true;
    if (st.empty())
      return false;
    return st.back() == need_top.value();
  }

  static void apply_stack(std::vector<StackSym>& st, const rule_type& r) {
    if (r.stack_top.has_value()) {
      // pop top (precondition already checked)
      st.pop_back();
    }
    // push in reverse so that r.push.front() ends up deeper
    for (auto it = r.push.rbegin(); it != r.push.rend(); ++it) {
      st.push_back(*it);
    }
  }
};

}  // namespace npda