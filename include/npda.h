// C++23 NPDA (nondeterministic pushdown automaton), header-only.
// - Epsilon transitions (input == nullopt)
// - Accept by final state, empty stack, or both (after full input)
// - BFS/DFS exploration
// - Loop-avoidance with visited configurations
// - Optional witness reconstruction (indices of rules taken)

#pragma once

#include <algorithm>
#include <concepts>
#include <deque>
#include <expected>
#include <functional>
#include <map>
#include <optional>
#include <ranges>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <fmt/color.h>
#include <fmt/format.h>

#include "config.h"
#include "npda/concepts.h"
#include "npda/key.h"
#include "npda/key_hash.h"
#include "npda/node.h"
#include "npda/rule.h"
#include "npda/utilities.h"

namespace npda {

enum class AcceptBy { FinalState, EmptyStack, Both, Any };

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
  bool trace_explanations = false;

  // Backtracking visualization options
  bool show_backtracking = true;  // Enable backtracking detection and visualization
  bool show_full_trace = true;    // Show complete execution trace including backtracks
};

struct RunResult {
  bool accepted = false;
  std::size_t expansions = 0;                       // expanded transitions (search work)
  std::optional<std::vector<std::size_t>> witness;  // indices into rules_
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

    // Build transition indices on first run
    build_indices();

    // Use the external Node, Key, and KeyHash types
    using NodeType = Node<State, StackSym>;
    using KeyType = Key<State, StackSym>;
    using KeyHashType = KeyHash<State, StackSym>;

    std::vector<NodeType> nodes;
    nodes.reserve(1024);

    auto make_root = [&] {
      NodeType n;
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

    std::unordered_set<KeyType, KeyHashType> visited;
    visited.reserve(4096);
    visited.insert(KeyType{nodes[0].s, nodes[0].pos, nodes[0].stack});

    std::size_t expansions = 0;

    // Exploration tracking: record all nodes and their exploration status
    std::vector<std::size_t> explored_nodes;  // Node indices that were explored
    std::vector<std::size_t> deadend_nodes;   // Node indices that are dead-ends
    bool exploration_detected = false;

    auto push_node = [&](NodeType&& n, std::size_t parent_idx, std::size_t rule_idx) -> void {
      KeyType k{n.s, n.pos, n.stack};
      if (visited.insert(std::move(k)).second) {
        n.parent = parent_idx;
        n.rule_idx = rule_idx;
        nodes.push_back(std::move(n));
        work.push_back(nodes.size() - 1);
      }
    };

    std::size_t best_trace_idx = 0;       // Track the most advanced node for trace display
    std::size_t max_pos = 0;              // Track the furthest input position reached
    std::size_t exploration_counter = 0;  // Counter for exploration steps

    while (!work.empty()) {
      std::size_t idx = opt.bfs ? work.front() : work.back();
      if (opt.bfs)
        work.pop_front();
      else
        work.pop_back();
      const NodeType cur = nodes[idx];  // copy for isolation

      // Track that we're exploring this node and show full detailed trace
      if (opt.trace && opt.show_full_trace) {
        explored_nodes.push_back(idx);

        // Show full detailed trace for this exploration step
        // Find the rule that led to this node (if any)
        std::optional<rule_type> exploration_rule = std::nullopt;
        if (cur.rule_idx.has_value() && *cur.rule_idx < rules_.size()) {
          exploration_rule = rules_[*cur.rule_idx];
        }

        emit_trace_step(cur, input, exploration_counter++, exploration_rule, opt, false, true);
      }

      if (is_accepting(cur, input)) {
        return build_result(
          cur,
          nodes,
          idx,
          expansions,
          opt,
          input,
          exploration_detected,
          explored_nodes,
          deadend_nodes
        );
      }

      // Track the most advanced node for potential trace display
      if (cur.pos > max_pos) {
        max_pos = cur.pos;
        best_trace_idx = idx;
      }

      if (expansions >= opt.max_expansions) {
        // If tracing is enabled and we have explored some paths, show the best trace we found
        if (opt.trace && nodes.size() > 1) {
          show_rejection_trace(nodes, best_trace_idx, input, opt, expansions);
        }
        return std::unexpected(Error{"max_expansions reached"});
      }

      // Expand children and detect dead-ends for DFS backtracking annotation
      std::size_t before = nodes.size();

      // Generate epsilon transitions using indices
      auto epsilon_it = epsilon_by_from_.find(cur.s);
      if (epsilon_it != epsilon_by_from_.end()) {
        for (std::size_t ri : epsilon_it->second) {
          const auto& r = rules_[ri];
          if (!stack_matches(cur.stack, r.stack_top))
            continue;

          NodeType nxt = cur;
          apply_stack(nxt.stack, r);
          nxt.s = r.to;
          ++expansions;
          push_node(std::move(nxt), idx, ri);
          if (expansions >= opt.max_expansions)
            return std::unexpected(Error{"max_expansions reached"});
        }
      }

      // Generate consuming transitions (if input left) using indices
      if (cur.pos < input.size()) {
        const Input sym = input[cur.pos];
        auto consume_it = consume_by_from_input_.find({cur.s, sym});
        if (consume_it != consume_by_from_input_.end()) {
          for (std::size_t ri : consume_it->second) {
            const auto& r = rules_[ri];
            if (!stack_matches(cur.stack, r.stack_top))
              continue;

            NodeType nxt = cur;
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

      // If no children were added, mark exploration dead-end
      if (!opt.bfs && nodes.size() == before) {
        exploration_detected = true;
        deadend_nodes.push_back(idx);
        if (opt.trace && opt.show_full_trace) {
          auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
            fmt::print("{}", s);
          };
          if (opt.trace_colors) {
            sink(fmt::format(
              fmt::fg(config::colors::info),
              "\n{} Exploration: dead-end at position {} (state {}), no applicable transitions\n",
              config::symbols::info,
              cur.pos,
              fmt::format("{}", cur.s)
            ));
          } else {
            sink(fmt::format(
              "\nExploration: dead-end at position {} (state {}), no applicable transitions\n",
              cur.pos,
              fmt::format("{}", cur.s)
            ));
          }
        }
      }
    }

    // If tracing is enabled and we explored some paths, show the best trace we found
    if (opt.trace && nodes.size() > 1) {
      show_rejection_trace(nodes, best_trace_idx, input, opt, expansions);
    }

    // Show exploration tree if enabled
    if (opt.trace && opt.show_full_trace && !explored_nodes.empty()) {
      show_exploration_tree(nodes, explored_nodes, input, opt);
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
    const RunOptions& opt = {},
    bool is_backtrack_point = false,
    bool is_exploration = false
  ) const {
    if (!opt.trace)
      return;

    auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
      fmt::print("{}", s);
    };

    std::string output;

    // Header with step number and exploration/backtracking indicator
    if (opt.trace_colors) {
      if (is_exploration) {
        output +=
          fmt::format(fmt::fg(config::colors::info), "\n=== Exploration Step {} ===\n", step_num);
      } else if (is_backtrack_point && opt.show_backtracking) {
        output += fmt::format(
          fmt::fg(config::colors::warning),
          "\n=== Step {} {}(BACKTRACK) ===\n",
          step_num,
          config::symbols::warning
        );
      } else {
        output +=
          fmt::format(fmt::fg(config::colors::section_heading), "\n=== Step {} ===\n", step_num);
      }
    } else {
      if (is_exploration) {
        output += fmt::format("\n=== Exploration Step {} ===\n", step_num);
      } else if (is_backtrack_point && opt.show_backtracking) {
        output += fmt::format("\n=== Step {} (BACKTRACK) ===\n", step_num);
      } else {
        output += fmt::format("\n=== Step {} ===\n", step_num);
      }
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

      // Add natural language explanation if enabled
      if (opt.trace_explanations) {
        std::string explanation = npda::explain_rule(r);
        if (opt.trace_colors) {
          output += fmt::format(fmt::fg(config::colors::info), "{}\n", explanation);
        } else {
          output += fmt::format("{}\n", explanation);
        }
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
    const std::vector<Input>& input,
    bool exploration_detected = false,
    const std::vector<std::size_t>& explored_nodes = {},
    const std::vector<std::size_t>& deadend_nodes = {}
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

      // Show exploration summary if enabled
      if (opt.show_backtracking && exploration_detected) {
        auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
          fmt::print("{}", s);
        };

        if (opt.trace_colors) {
          sink(fmt::format(
            fmt::fg(config::colors::info),
            "\n{} Exploration summary: {} nodes explored, {} dead-ends found\n",
            config::symbols::info,
            explored_nodes.size(),
            deadend_nodes.size()
          ));
        } else {
          sink(fmt::format(
            "\nExploration summary: {} nodes explored, {} dead-ends found\n",
            explored_nodes.size(),
            deadend_nodes.size()
          ));
        }
      }
    }

    // Show exploration tree if enabled
    if (opt.trace && opt.show_full_trace && !explored_nodes.empty()) {
      show_exploration_tree(nodes, explored_nodes, input, opt);
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
        "\n{} Accepting path found! Replaying {} steps...\n",
        config::symbols::info,
        rule_path.size()
      ));
    } else {
      sink(fmt::format("\nAccepting path found! Replaying {} steps...\n", rule_path.size()));
    }

    // Emit each step showing the state after applying the rule
    std::size_t step_num = 0;

    // First, show the initial state (Step 0) with no rule applied
    if (!node_path.empty()) {
      emit_trace_step(nodes[node_path[0]], input, step_num, std::nullopt, opt, false);
    }

    // Then show the transitions starting from Step 1
    for (std::size_t i = 1; i < node_path.size(); ++i) {
      ++step_num;
      const std::size_t node_idx = node_path[i];
      const std::size_t rule_idx = rule_path[i - 1];

      // Check if this is a backtrack point by comparing positions
      bool is_backtrack = (i > 1 && nodes[node_idx].pos < nodes[node_path[i - 1]].pos);
      emit_trace_step(nodes[node_idx], input, step_num, rules_[rule_idx], opt, is_backtrack);
    }

    // Show final accepting state (if not already shown)
    if (node_path.empty() || node_path.back() != final_idx) {
      emit_trace_step(nodes[final_idx], input, step_num + 1, std::nullopt, opt, false);
      ++step_num;
    }

    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(config::colors::success), "\n{} Input accepted!\n", config::symbols::success
      ));
    } else {
      sink("\nInput accepted!\n");
    }
  }

  template <typename NodeT>
  void show_rejection_trace(
    const std::vector<NodeT>& nodes,
    std::size_t best_idx,
    const std::vector<Input>& input,
    const RunOptions& opt,
    std::size_t expansions
  ) const {
    if (!opt.trace || !opt.track_witness)
      return;

    auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
      fmt::print("{}", s);
    };

    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(config::colors::banner_text),
        "\n{} Input rejected! Showing furthest path explored ({} expansions)...\n",
        config::symbols::error,
        expansions
      ));
    } else {
      sink(fmt::format(
        "\nInput rejected! Showing furthest path explored ({} expansions)...\n", expansions
      ));
    }

    // Reconstruct the path to the best node we found
    std::vector<std::size_t> node_path_rev;
    std::vector<std::size_t> rule_path_rev;

    std::size_t cur = best_idx;
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

    // Show how far we got in the input
    const auto& best_node = nodes[best_idx];
    std::size_t remaining_input = input.size() - best_node.pos;

    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(config::colors::info),
        "Furthest position: {} / {} ({} characters remaining)\n",
        best_node.pos,
        input.size(),
        remaining_input
      ));
    } else {
      sink(fmt::format(
        "Furthest position: {} / {} ({} characters remaining)\n",
        best_node.pos,
        input.size(),
        remaining_input
      ));
    }

    // Emit each step showing the state after applying the rule
    std::size_t step_num = 0;

    // First, show the initial state (Step 0) with no rule applied
    if (!node_path.empty()) {
      emit_trace_step(nodes[node_path[0]], input, step_num, std::nullopt, opt, false);
    }

    // Then show the transitions starting from Step 1
    for (std::size_t i = 1; i < node_path.size(); ++i) {
      ++step_num;
      const std::size_t node_idx = node_path[i];
      const std::size_t rule_idx = rule_path[i - 1];

      // Check if this is a backtrack point by comparing positions
      bool is_backtrack = (i > 1 && nodes[node_idx].pos < nodes[node_path[i - 1]].pos);
      emit_trace_step(nodes[node_idx], input, step_num, rules_[rule_idx], opt, is_backtrack);
    }

    // Show the final state where we got stuck
    if (!node_path.empty()) {
      emit_trace_step(nodes[best_idx], input, step_num + 1, std::nullopt, opt, false);
    }

    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(config::colors::error),
        "\n{} Input rejected at this point!\n",
        config::symbols::error
      ));
    } else {
      sink("\nInput rejected at this point!\n");
    }
  }

  // Show exploration tree structure
  template <typename NodeT>
  void show_exploration_tree(
    const std::vector<NodeT>& nodes,
    const std::vector<std::size_t>& explored_nodes,
    [[maybe_unused]] const std::vector<Input>& input,
    const RunOptions& opt
  ) const {
    if (!opt.trace || explored_nodes.empty())
      return;

    auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
      fmt::print("{}", s);
    };

    if (opt.trace_colors) {
      sink(fmt::format(
        fmt::fg(config::colors::banner_text),
        "\n{} Exploration Tree Structure:\n",
        config::symbols::info
      ));
    } else {
      sink("\nExploration Tree Structure:\n");
    }

    // Build parent-child relationships
    std::unordered_map<std::size_t, std::vector<std::size_t>> children;
    for (std::size_t node_idx : explored_nodes) {
      if (nodes[node_idx].parent != npos) {
        children[nodes[node_idx].parent].push_back(node_idx);
      }
    }

    // Recursive function to print tree
    std::function<void(std::size_t, std::string, bool)> print_tree;
    print_tree = [&](std::size_t node_idx, std::string prefix, bool is_last) {
      const auto& node = nodes[node_idx];

      // Show node info
      std::string node_info =
        fmt::format("[{}] pos:{} state:{} ", node_idx, node.pos, fmt::format("{}", node.s));

      if (opt.trace_colors) {
        sink(fmt::format(
          "{}{} {}\n",
          prefix,
          is_last ? "└── " : "├── ",
          fmt::format(fmt::fg(config::colors::info), "{}", node_info)
        ));
      } else {
        sink(fmt::format("{}{} {}\n", prefix, is_last ? "└── " : "├── ", node_info));
      }

      // Print children
      if (children.find(node_idx) != children.end()) {
        const auto& child_nodes = children[node_idx];
        for (std::size_t i = 0; i < child_nodes.size(); ++i) {
          bool last_child = (i == child_nodes.size() - 1);
          std::string child_prefix = prefix + (is_last ? "    " : "│   ");
          print_tree(child_nodes[i], child_prefix, last_child);
        }
      }
    };

    // Find root nodes (nodes with no parent or parent not in explored_nodes)
    std::vector<std::size_t> roots;
    for (std::size_t node_idx : explored_nodes) {
      if (nodes[node_idx].parent == npos ||
          std::find(explored_nodes.begin(), explored_nodes.end(), nodes[node_idx].parent) ==
            explored_nodes.end()) {
        roots.push_back(node_idx);
      }
    }

    // Print all trees
    for (std::size_t i = 0; i < roots.size(); ++i) {
      print_tree(roots[i], "", true);
    }
  }

  State start_{};
  std::vector<State> accepting_{};
  AcceptBy policy_{AcceptBy::FinalState};
  StackSym bottom_{};
  std::vector<rule_type> rules_{};

  // Transition indices for efficient rule lookup
  mutable std::unordered_map<State, std::vector<std::size_t>> epsilon_by_from_;
  mutable std::map<std::pair<State, Input>, std::vector<std::size_t>> consume_by_from_input_;
  mutable bool indices_built_ = false;

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

  // Build transition indices for efficient rule lookup
  void build_indices() const {
    if (indices_built_)
      return;

    for (std::size_t i = 0; i < rules_.size(); ++i) {
      const auto& r = rules_[i];
      if (!r.input.has_value()) {
        // Epsilon transition
        epsilon_by_from_[r.from].push_back(i);
      } else {
        // Consuming transition - use a simple hash approach
        auto key = std::make_pair(r.from, r.input.value());
        consume_by_from_input_[key].push_back(i);
      }
    }
    indices_built_ = true;
  }

  // Check if a node satisfies the acceptance policy
  template <typename NodeT>
  [[nodiscard]] bool is_accepting(const NodeT& n, std::span<const Input> input) const {
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
      case AcceptBy::Any:
        return at_end && (by_state || by_stack);
    }
    return false;
  }
};

}  // namespace npda