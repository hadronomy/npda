// C++23 Modern Turing Machine Implementation
// - Deterministic execution with configurable variants
// - Single and multi-tape support
// - Configurable movement (L/R vs L/R/S)
// - Configurable tape direction (bidirectional vs right-only)
// - Configurable operation modes (simultaneous vs independent)
// - Modern C++23 features: std::expected, ranges, concepts
// - Clean architecture with separation of concerns
// - Comprehensive trace visualization

#pragma once

#include <algorithm>
#include <concepts>
#include <deque>
#include <expected>
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

// Configuration structure for TM variants
struct TMConfig {
  std::size_t num_tapes = 1;
  TapeDirection tape_direction = TapeDirection::Bidirectional;
  OperationMode operation_mode = OperationMode::Independent;
  bool allow_stay = true;  // Allow 'S' movement or only L/R
};

struct Error {
  std::string message;
};

struct RunOptions {
  // Hard limit on steps to prevent infinite loops
  std::size_t max_steps = 1000000;

  // Track and return a witness (sequence of rule indices)
  bool track_witness = true;

  // Pretty, colored per-step trace diagrams printed during run
  bool trace = false;
  // Optional sink; if not set and trace=true, prints via fmt::print
  std::function<void(std::string_view)> trace_sink = {};

  // Trace formatting options
  bool trace_colors = true;
  bool trace_compact = false;
  bool trace_explanations = false;

  // Show configuration summary at start
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

template <Hashable State, Hashable TapeSym>
class TuringMachine {
 public:
  using rule_type = Rule<State, TapeSym>;

  // Internal key types and hashers for transition tables
  using SingleKey = std::pair<State, TapeSym>;
  using MultiKey = std::pair<State, std::vector<TapeSym>>;

  struct SingleKeyHash {
    std::size_t operator()(SingleKey const& k) const noexcept {
      std::size_t h1 = std::hash<State>{}(k.first);
      std::size_t h2 = std::hash<TapeSym>{}(k.second);
      // simple combine
      return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
    }
  };

  struct MultiKeyHash {
    std::size_t operator()(MultiKey const& k) const noexcept {
      std::size_t h = std::hash<State>{}(k.first);
      std::size_t seed = 0x9e3779b97f4a7c15ULL;
      for (auto const& sym : k.second) {
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
      for (auto&& s : r) {
        accepting_.push_back(s);
      }
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
      // Build faster accepting set internally
      tm.accepting_set_.reserve(tm.accepting_.size());
      for (auto const& s : tm.accepting_)
        tm.accepting_set_.insert(s);
      tm.blank_ = *blank_;
      tm.rules_ = std::move(rules_);

      // Build indices and validate determinism/arity/movement constraints now
      tm.build_indices();
      if (tm.indices_error_) {
        return std::unexpected(*tm.indices_error_);
      }
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

  // Run on any input range
  template <std::ranges::input_range R>
  requires std::same_as<std::ranges::range_value_t<R>, TapeSym> [[nodiscard]] std::
    expected<RunResult, Error>
    run(R&& rng, const RunOptions& opt = {}) const {

    std::vector<TapeSym> input;
    for (auto&& x : rng)
      input.push_back(x);

    // Build transition indices on first run
    build_indices();
    if (indices_error_) {
      return std::unexpected(*indices_error_);
    }

    // Show configuration if requested
    if (opt.show_config) {
      show_configuration(opt);
    }

    if (config_.num_tapes == 1) {
      return run_single_tape(input, opt);
    } else {
      return run_multi_tape(input, opt);
    }
  }

  // Get configuration
  [[nodiscard]] const TMConfig& config() const noexcept { return config_; }

  // Trace visualization methods
  template <typename NodeT>
  void emit_trace_step(
    const NodeT& node,
    std::size_t step_num,
    const std::optional<rule_type>& rule = std::nullopt,
    const RunOptions& opt = {}
  ) const;

  template <typename NodeT>
  std::expected<RunResult, Error> build_result(
    const NodeT& acc_node,
    const std::vector<NodeT>& nodes,
    std::size_t idx,
    std::size_t steps,
    const RunOptions& opt
  ) const;

  template <typename NodeT>
  void replay_trace_path(
    const std::vector<NodeT>& nodes,
    std::size_t final_idx,
    const RunOptions& opt
  ) const;

  void show_configuration(const RunOptions& opt) const;

 private:
  TMConfig config_{};
  State start_{};
  std::vector<State> accepting_{};
  // Faster membership test for accepting states (internal)
  mutable std::unordered_set<State> accepting_set_{};
  TapeSym blank_{};
  std::vector<rule_type> rules_{};

  // Transition maps for efficient lookup
  mutable std::unordered_map<SingleKey, std::size_t, SingleKeyHash> single_transitions_;
  mutable std::unordered_map<MultiKey, std::size_t, MultiKeyHash> multi_transitions_;
  mutable bool indices_built_ = false;
  mutable std::unique_ptr<std::once_flag> indices_once_;
  mutable std::optional<Error> indices_error_{};

  static constexpr std::size_t npos = static_cast<std::size_t>(-1);

  // Helper functions for string joining
  static std::string join_symbols(const std::vector<TapeSym>& symbols) {
    // TapeSym must be fmt-formattable
    return fmt::format("{}", fmt::join(symbols, ","));
  }

  static std::string join_directions(const std::vector<Direction>& directions) {
    std::vector<char> chars;
    chars.reserve(directions.size());
    for (auto d : directions)
      chars.push_back(static_cast<char>(d));
    return fmt::format("{}", fmt::join(chars, ","));
  }

  // Single tape execution
  [[nodiscard]] std::expected<RunResult, Error>
    run_single_tape(const std::vector<TapeSym>& input, const RunOptions& opt) const;

  // Multi-tape execution
  [[nodiscard]] std::expected<RunResult, Error>
    run_multi_tape(const std::vector<TapeSym>& input, const RunOptions& opt) const;

  // Helper methods
  static bool contains(const std::vector<State>& v, const State& s) {
    return std::ranges::find(v, s) != v.end();
  }

  // Get symbol at current head position, extending tape with blanks if needed
  TapeSym get_tape_symbol(const SingleTapeNode<State, TapeSym>& node) const {
    if (node.head_pos < node.tape.size()) {
      return node.tape[node.head_pos];
    }
    return blank_;
  }

  // Apply single tape transition
  void apply_single_tape_transition(
    SingleTapeNode<State, TapeSym>& node,
    const SingleTapeRule<State, TapeSym>& rule
  ) const {
    // For a single tape, both modes are effectively identical:
    // write then move.
    if (node.head_pos >= node.tape.size()) {
      node.tape.resize(node.head_pos + 1, blank_);
    }
    node.tape[node.head_pos] = rule.write;
    move_head(node, rule.move);
    node.s = rule.to;
  }

  // Apply multi-tape transition
  void apply_multi_tape_transition(
    MultiTapeNode<State, TapeSym>& node,
    const MultiTapeRule<State, TapeSym>& rule
  ) const {
    const std::size_t num_tapes = config_.num_tapes;

    if (config_.operation_mode == OperationMode::Simultaneous) {
      // Phase 1: ensure capacity and write ALL heads
      for (std::size_t i = 0; i < num_tapes; ++i) {
        if (node.head_positions[i] >= node.tapes[i].size()) {
          node.tapes[i].resize(node.head_positions[i] + 1, blank_);
        }
        node.tapes[i][node.head_positions[i]] = rule.write[i];
      }
      // Phase 2: move ALL heads
      for (std::size_t i = 0; i < num_tapes; ++i) {
        move_multi_head(node, i, rule.move[i]);
      }
    } else {
      // Independent: apply per tape in index order (write then move per tape)
      for (std::size_t i = 0; i < num_tapes; ++i) {
        if (node.head_positions[i] >= node.tapes[i].size()) {
          node.tapes[i].resize(node.head_positions[i] + 1, blank_);
        }
        node.tapes[i][node.head_positions[i]] = rule.write[i];
        move_multi_head(node, i, rule.move[i]);
      }
    }
    node.s = rule.to;
  }

  // Move single tape head
  void move_head(SingleTapeNode<State, TapeSym>& node, Direction dir) const {
    switch (dir) {
      case Direction::Left:
        if (config_.tape_direction == TapeDirection::Bidirectional) {
          if (node.head_pos == 0) {
            // extend to the left with a blank
            node.tape.insert(node.tape.begin(), blank_);
            // head remains at 0 (now on the new blank)
          } else {
            --node.head_pos;
          }
        }
        // In right-only mode, left movement is disallowed by validation
        break;
      case Direction::Right:
        ++node.head_pos;
        break;
      case Direction::Stay:
        if (config_.allow_stay) {
          // Head position unchanged
        } else {
          // If stay not allowed, treat as right movement
          ++node.head_pos;
        }
        break;
    }
  }

  // Move multi-tape head
  void move_multi_head(MultiTapeNode<State, TapeSym>& node, std::size_t tape_idx, Direction dir)
    const {
    switch (dir) {
      case Direction::Left:
        if (config_.tape_direction == TapeDirection::Bidirectional) {
          auto& pos = node.head_positions[tape_idx];
          if (pos == 0) {
            node.tapes[tape_idx].insert(node.tapes[tape_idx].begin(), blank_);
            // head stays at 0
          } else {
            --pos;
          }
        }
        break;
      case Direction::Right:
        ++node.head_positions[tape_idx];
        break;
      case Direction::Stay:
        if (config_.allow_stay) {
          // Head position unchanged
        } else {
          // If stay not allowed, treat as right movement
          ++node.head_positions[tape_idx];
        }
        break;
    }
  }

  // Build transition indices for efficient rule lookup
  void build_indices() const {
    std::call_once(*indices_once_, [this]() {
      // Clear any existing data
      single_transitions_.clear();
      multi_transitions_.clear();
      indices_error_.reset();

      // ensure accepting_set_ is populated if not built via Builder
      if (accepting_set_.empty() && !accepting_.empty()) {
        accepting_set_.reserve(accepting_.size());
        for (auto const& s : accepting_)
          accepting_set_.insert(s);
      }

      const auto left_disallowed = (config_.tape_direction == TapeDirection::RightOnly);
      const auto stay_disallowed = !config_.allow_stay;
      const auto num_tapes = config_.num_tapes;

      for (std::size_t i = 0; i < rules_.size(); ++i) {
        const auto& r = rules_[i];
        if (r.is_multi_tape) {
          // Arity checks
          if (r.multi.read.size() != num_tapes || r.multi.write.size() != num_tapes ||
              r.multi.move.size() != num_tapes) {
            indices_error_ = Error{fmt::format(
              "multi-tape rule #{} arity mismatch: expected {} entries in read/write/move",
              i,
              num_tapes
            )};
            return;
          }
          // Movement constraints
          if (stay_disallowed) {
            for (auto d : r.multi.move) {
              if (d == Direction::Stay) {
                indices_error_ = Error{fmt::format("rule #{} uses Stay while allow_stay=false", i)};
                return;
              }
            }
          }
          if (left_disallowed) {
            for (auto d : r.multi.move) {
              if (d == Direction::Left) {
                indices_error_ =
                  Error{fmt::format("rule #{} uses Left while TapeDirection is Right-only", i)};
                return;
              }
            }
          }
          // Determinism: duplicate (state, read-vector) is error
          MultiKey key = std::make_pair(r.multi.from, r.multi.read);
          auto [it, ok] = multi_transitions_.emplace(std::move(key), i);
          if (!ok) {
            indices_error_ = Error{fmt::format(
              "duplicate multi-tape transition for state '{}' and symbols ({})",
              fmt::format("{}", r.multi.from),
              join_symbols(r.multi.read)
            )};
            return;
          }
        } else {
          // Movement constraints
          if (stay_disallowed && r.single.move == Direction::Stay) {
            indices_error_ = Error{fmt::format("rule #{} uses Stay while allow_stay=false", i)};
            return;
          }
          if (left_disallowed && r.single.move == Direction::Left) {
            indices_error_ =
              Error{fmt::format("rule #{} uses Left while TapeDirection is Right-only", i)};
            return;
          }
          // Determinism: duplicate (state, read) is error
          SingleKey key = std::make_pair(r.single.from, r.single.read);
          auto [it, ok] = single_transitions_.emplace(std::move(key), i);
          if (!ok) {
            indices_error_ = Error{fmt::format(
              "duplicate single-tape transition for state '{}' and symbol '{}'",
              fmt::format("{}", r.single.from),
              fmt::format("{}", r.single.read)
            )};
            return;
          }
        }
      }
      indices_built_ = true;
    });
  }

  // Check if a node is in an accepting state
  [[nodiscard]] bool is_accepting(const State& s) const {
    if (!accepting_set_.empty())
      return accepting_set_.find(s) != accepting_set_.end();
    return contains(accepting_, s);
  }
};

//
// Out-of-class implementations for trace visualization methods
//

template <Hashable State, Hashable TapeSym>
template <typename NodeT>
void TuringMachine<State, TapeSym>::emit_trace_step(
  const NodeT& node,
  std::size_t step_num,
  const std::optional<rule_type>& rule,
  const RunOptions& opt
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
      fmt::format(fmt::fg(npda::config::colors::section_heading), "\n=== Step {} ===\n", step_num);
  } else {
    output += fmt::format("\n=== Step {} ===\n", step_num);
  }

  // Current state
  if (opt.trace_colors) {
    output += fmt::format(fmt::fg(npda::config::colors::info), "State: ");
    output +=
      fmt::format(fmt::fg(npda::config::colors::success), "{}\n", fmt::format("{}", node.s));
  } else {
    output += fmt::format("State: {}\n", fmt::format("{}", node.s));
  }

  // Tape visualization
  if constexpr (std::is_same_v<NodeT, SingleTapeNode<State, TapeSym>>) {
    // Single tape visualization
    output += "Tape: ";
    for (std::size_t i = 0; i < node.tape.size(); ++i) {
      if (i == node.head_pos) {
        if (opt.trace_colors) {
          output += fmt::format(
            fmt::fg(npda::config::colors::warning), "[{}]", fmt::format("{}", node.tape[i])
          );
        } else {
          output += fmt::format("[{}]", fmt::format("{}", node.tape[i]));
        }
      } else {
        output += fmt::format(" {} ", fmt::format("{}", node.tape[i]));
      }
    }
    if (node.head_pos >= node.tape.size()) {
      if (opt.trace_colors) {
        output +=
          fmt::format(fmt::fg(npda::config::colors::success), " [{}]", fmt::format("{}", blank_));
      } else {
        output += fmt::format(" [{}]", fmt::format("{}", blank_));
      }
    }
    output += "\n";

    // Head position indicator
    output += "      ";
    for (std::size_t i = 0; i < std::min(node.head_pos, node.tape.size()); ++i) {
      output += "    ";
    }
    if (node.head_pos < node.tape.size()) {
      output += " ^  ";
    } else {
      for (std::size_t i = node.tape.size(); i < node.head_pos; ++i) {
        output += "    ";
      }
      output += " ^  ";
    }
    output += "\n";
  } else {
    // Multi-tape visualization
    for (std::size_t tape_idx = 0; tape_idx < config_.num_tapes; ++tape_idx) {
      output += fmt::format("Tape {}: ", tape_idx + 1);
      const auto& tape = node.tapes[tape_idx];
      std::size_t head_pos = node.head_positions[tape_idx];

      for (std::size_t i = 0; i < tape.size(); ++i) {
        if (i == head_pos) {
          if (opt.trace_colors) {
            output += fmt::format(
              fmt::fg(npda::config::colors::warning), "[{}]", fmt::format("{}", tape[i])
            );
          } else {
            output += fmt::format("[{}]", fmt::format("{}", tape[i]));
          }
        } else {
          output += fmt::format(" {} ", fmt::format("{}", tape[i]));
        }
      }
      if (head_pos >= tape.size()) {
        if (opt.trace_colors) {
          output +=
            fmt::format(fmt::fg(npda::config::colors::success), " [{}]", fmt::format("{}", blank_));
        } else {
          output += fmt::format(" [{}]", fmt::format("{}", blank_));
        }
      }
      output += "\n";

      // Head position indicator
      output += "       ";
      for (std::size_t i = 0; i < std::min(head_pos, tape.size()); ++i) {
        output += "    ";
      }
      if (head_pos < tape.size()) {
        output += " ^  ";
      } else {
        for (std::size_t i = tape.size(); i < head_pos; ++i) {
          output += "    ";
        }
        output += " ^  ";
      }
      output += "\n";
    }
  }

  // Rule information
  if (rule.has_value()) {
    if (opt.trace_colors) {
      output += fmt::format(fmt::fg(npda::config::colors::info), "Rule: ");
    } else {
      output += "Rule: ";
    }

    const auto& r = rule.value();
    std::string rule_str;

    if (r.is_multi_tape) {
      // Multi-tape rule format
      rule_str = fmt::format(
        "({}, {}) → ({}, {}, {})",
        fmt::format("{}", r.multi.from),
        join_symbols(r.multi.read),
        fmt::format("{}", r.multi.to),
        join_symbols(r.multi.write),
        join_directions(r.multi.move)
      );
    } else {
      // Single tape rule format
      rule_str = fmt::format(
        "({}, {}) → ({}, {}, {})",
        fmt::format("{}", r.single.from),
        fmt::format("{}", r.single.read),
        fmt::format("{}", r.single.to),
        fmt::format("{}", r.single.write),
        static_cast<char>(r.single.move)
      );
    }

    if (opt.trace_colors) {
      output += fmt::format(fmt::fg(npda::config::colors::example), "{}\n", rule_str);
    } else {
      output += fmt::format("{}\n", rule_str);
    }

    // Add natural language explanation if enabled
    if (opt.trace_explanations) {
      std::string explanation;
      if (r.is_multi_tape) {
        explanation = fmt::format(
          "In state {}, read ({}), write ({}), move heads ({}), and go to state {}",
          fmt::format("{}", r.multi.from),
          join_symbols(r.multi.read),
          join_symbols(r.multi.write),
          join_directions(r.multi.move),
          fmt::format("{}", r.multi.to)
        );
      } else {
        explanation = fmt::format(
          "In state {}, read '{}' from tape, write '{}', move head {}, and go to state {}",
          fmt::format("{}", r.single.from),
          fmt::format("{}", r.single.read),
          fmt::format("{}", r.single.write),
          static_cast<char>(r.single.move),
          fmt::format("{}", r.single.to)
        );
      }

      if (opt.trace_colors) {
        output += fmt::format(fmt::fg(npda::config::colors::info), "{}\n", explanation);
      } else {
        output += fmt::format("{}\n", explanation);
      }
    }
  }

  sink(output);
}

template <Hashable State, Hashable TapeSym>
void TuringMachine<State, TapeSym>::show_configuration(const RunOptions& opt) const {
  if (!opt.show_config)
    return;

  auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
    fmt::print("{}", s);
  };

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
    } else {
      sink(fmt::format("  {}: {}\n", key, value));
    }
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
template <typename NodeT>
std::expected<RunResult, Error> TuringMachine<State, TapeSym>::build_result(
  const NodeT& acc_node,
  const std::vector<NodeT>& nodes,
  std::size_t idx,
  std::size_t steps,
  const RunOptions& opt
) const {
  if (!opt.track_witness) {
    // Prepare final tape and head position
    std::vector<std::vector<std::string>> final_tapes;
    std::vector<std::size_t> final_head_positions;

    if constexpr (std::is_same_v<NodeT, SingleTapeNode<State, TapeSym>>) {
      std::vector<std::string> tape_str;
      for (const auto& sym : acc_node.tape) {
        tape_str.push_back(fmt::format("{}", sym));
      }
      final_tapes.push_back(tape_str);
      final_head_positions.push_back(acc_node.head_pos);
    } else {
      for (std::size_t i = 0; i < config_.num_tapes; ++i) {
        std::vector<std::string> tape_str;
        for (const auto& sym : acc_node.tapes[i]) {
          tape_str.push_back(fmt::format("{}", sym));
        }
        final_tapes.push_back(tape_str);
        final_head_positions.push_back(acc_node.head_positions[i]);
      }
    }

    return RunResult{true, steps, std::nullopt, final_tapes, final_head_positions, config_};
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
  std::ranges::reverse(path);

  // Replay the trace if tracing is enabled
  if (opt.trace) {
    replay_trace_path(nodes, idx, opt);
  }

  // Prepare final tape and head position
  std::vector<std::vector<std::string>> final_tapes;
  std::vector<std::size_t> final_head_positions;

  if constexpr (std::is_same_v<NodeT, SingleTapeNode<State, TapeSym>>) {
    std::vector<std::string> tape_str;
    for (const auto& sym : acc_node.tape) {
      tape_str.push_back(fmt::format("{}", sym));
    }
    final_tapes.push_back(tape_str);
    final_head_positions.push_back(acc_node.head_pos);
  } else {
    for (std::size_t i = 0; i < config_.num_tapes; ++i) {
      std::vector<std::string> tape_str;
      for (const auto& sym : acc_node.tapes[i]) {
        tape_str.push_back(fmt::format("{}", sym));
      }
      final_tapes.push_back(tape_str);
      final_head_positions.push_back(acc_node.head_positions[i]);
    }
  }

  return RunResult{true, steps, std::move(path), final_tapes, final_head_positions, config_};
}

template <Hashable State, Hashable TapeSym>
template <typename NodeT>
void TuringMachine<State, TapeSym>::replay_trace_path(
  const std::vector<NodeT>& nodes,
  std::size_t final_idx,
  const RunOptions& opt
) const {
  if (!opt.trace || !opt.track_witness)
    return;

  // Reconstruct node and rule paths from root to final
  std::vector<std::size_t> node_path_rev;
  std::vector<std::size_t> rule_path_rev;

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
      fmt::fg(npda::config::colors::banner_text),
      "\n{} Accepting configuration found! Replaying {} steps...\n",
      npda::config::symbols::info,
      rule_path.size()
    ));
  } else {
    sink(fmt::format("\nAccepting configuration found! Replaying {} steps...\n", rule_path.size()));
  }

  // Emit each step showing state after applying rule
  std::size_t step_num = 0;

  // First, show the initial state (Step 0) with no rule applied
  if (!node_path.empty()) {
    emit_trace_step(nodes[node_path[0]], step_num, std::nullopt, opt);
  }

  // Then show the transitions starting from Step 1
  for (std::size_t i = 1; i < node_path.size(); ++i) {
    ++step_num;
    const std::size_t node_idx = node_path[i];
    const std::size_t rule_idx = rule_path[i - 1];

    std::optional<rule_type> rule_opt;
    const auto& rule = rules_[rule_idx];
    rule_opt = rule;

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
std::expected<RunResult, Error> TuringMachine<State, TapeSym>::run_single_tape(
  const std::vector<TapeSym>& input,
  const RunOptions& opt
) const {
  using NodeType = SingleTapeNode<State, TapeSym>;

  auto make_root = [&]() {
    NodeType node;
    node.s = start_;
    node.tape = input;
    node.head_pos = 0;
    node.parent = npos;
    node.rule_idx = std::nullopt;
    return node;
  };

  std::size_t steps = 0;

  // Fast path: no witness tracking => no need to store nodes/path
  if (!opt.track_witness) {
    NodeType current = make_root();
    for (;;) {
      if (opt.trace) {
        emit_trace_step(current, steps, std::nullopt, opt);
      }
      if (is_accepting(current.s)) {
        // Build final result directly
        std::vector<std::vector<std::string>> final_tapes;
        final_tapes.reserve(1);
        std::vector<std::string> tape_str;
        tape_str.reserve(current.tape.size());
        for (const auto& sym : current.tape)
          tape_str.push_back(fmt::format("{}", sym));
        final_tapes.push_back(std::move(tape_str));
        return RunResult{
          true, steps, std::nullopt, std::move(final_tapes), {current.head_pos}, config_
        };
      }
      if (steps >= opt.max_steps) {
        return std::unexpected(Error{"max_steps reached"});
      }
      // Transition lookup
      TapeSym current_symbol = get_tape_symbol(current);
      auto key = std::make_pair(current.s, current_symbol);
      auto it = single_transitions_.find(key);
      if (it == single_transitions_.end()) {
        if (opt.trace) {
          auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
            fmt::print("{}", s);
          };
          if (opt.trace_colors) {
            sink(fmt::format(
              fmt::fg(npda::config::colors::error),
              "\n{} No transition available for state '{}' and symbol '{}'\n",
              npda::config::symbols::error,
              fmt::format("{}", current.s),
              fmt::format("{}", current_symbol)
            ));
          } else {
            sink(fmt::format(
              "\nNo transition available for state '{}' and symbol '{}'\n",
              fmt::format("{}", current.s),
              fmt::format("{}", current_symbol)
            ));
          }
        }
        break;
      }
      const auto& rule = rules_[it->second].single;
      apply_single_tape_transition(current, rule);
      ++steps;
    }
    return RunResult{false, steps, std::nullopt, {}, {}, config_};
  }

  // Witness path required: keep nodes and parent links
  std::vector<NodeType> nodes;
  nodes.reserve(1024);
  nodes.push_back(make_root());
  std::deque<std::size_t> work;
  work.push_back(0);

  while (!work.empty()) {
    std::size_t idx = work.front();
    work.pop_front();
    const NodeType& current = nodes[idx];  // refer to stored node

    // Trace current step
    if (opt.trace) {
      emit_trace_step(current, steps, std::nullopt, opt);
    }

    if (is_accepting(current.s)) {
      return build_result(current, nodes, idx, steps, opt);
    }

    if (steps >= opt.max_steps) {
      return std::unexpected(Error{"max_steps reached"});
    }

    // Find applicable transition
    TapeSym current_symbol = get_tape_symbol(current);
    auto key = std::make_pair(current.s, current_symbol);
    auto it = single_transitions_.find(key);

    if (it == single_transitions_.end()) {
      // No transition available - halt and reject
      if (opt.trace) {
        auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
          fmt::print("{}", s);
        };
        if (opt.trace_colors) {
          sink(fmt::format(
            fmt::fg(npda::config::colors::error),
            "\n{} No transition available for state '{}' and symbol '{}'\n",
            npda::config::symbols::error,
            fmt::format("{}", current.s),
            fmt::format("{}", current_symbol)
          ));
        } else {
          sink(fmt::format(
            "\nNo transition available for state '{}' and symbol '{}'\n",
            fmt::format("{}", current.s),
            fmt::format("{}", current_symbol)
          ));
        }
      }
      break;
    }

    // Apply transition (deterministic, so only one)
    const auto& rule = rules_[it->second].single;
    NodeType next = current;  // copy to produce child
    apply_single_tape_transition(next, rule);
    ++steps;
    next.parent = idx;
    next.rule_idx = it->second;
    nodes.push_back(std::move(next));
    work.push_back(nodes.size() - 1);
  }

  return RunResult{false, steps, std::nullopt, {}, {}, config_};
}

template <Hashable State, Hashable TapeSym>
std::expected<RunResult, Error> TuringMachine<State, TapeSym>::run_multi_tape(
  const std::vector<TapeSym>& input,
  const RunOptions& opt
) const {
  using NodeType = MultiTapeNode<State, TapeSym>;

  auto make_root = [&]() {
    NodeType node;
    node.s = start_;
    node.tapes.resize(config_.num_tapes);
    node.head_positions.resize(config_.num_tapes, 0);

    // Input goes on first tape
    node.tapes[0] = input;
    for (std::size_t i = 1; i < config_.num_tapes; ++i) {
      node.tapes[i] = {};  // Other tapes start empty
    }

    node.parent = npos;
    node.rule_idx = std::nullopt;
    return node;
  };

  std::size_t steps = 0;

  // Fast path: no witness tracking
  if (!opt.track_witness) {
    NodeType current = make_root();
    for (;;) {
      if (opt.trace) {
        emit_trace_step(current, steps, std::nullopt, opt);
      }
      if (is_accepting(current.s)) {
        // Build final result directly
        std::vector<std::vector<std::string>> final_tapes;
        final_tapes.reserve(config_.num_tapes);
        std::vector<std::size_t> final_head_positions;
        final_head_positions.reserve(config_.num_tapes);
        for (std::size_t i = 0; i < config_.num_tapes; ++i) {
          std::vector<std::string> tape_str;
          tape_str.reserve(current.tapes[i].size());
          for (const auto& sym : current.tapes[i])
            tape_str.push_back(fmt::format("{}", sym));
          final_tapes.push_back(std::move(tape_str));
          final_head_positions.push_back(current.head_positions[i]);
        }
        return RunResult{
          true,
          steps,
          std::nullopt,
          std::move(final_tapes),
          std::move(final_head_positions),
          config_
        };
      }
      if (steps >= opt.max_steps) {
        return std::unexpected(Error{"max_steps reached"});
      }
      // Find applicable transition
      std::vector<TapeSym> current_symbols;
      current_symbols.reserve(config_.num_tapes);
      for (std::size_t i = 0; i < config_.num_tapes; ++i) {
        if (current.head_positions[i] < current.tapes[i].size()) {
          current_symbols.push_back(current.tapes[i][current.head_positions[i]]);
        } else {
          current_symbols.push_back(blank_);
        }
      }
      auto key = std::make_pair(current.s, current_symbols);
      auto it = multi_transitions_.find(key);
      if (it == multi_transitions_.end()) {
        if (opt.trace) {
          auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
            fmt::print("{}", s);
          };
          if (opt.trace_colors) {
            sink(fmt::format(
              fmt::fg(npda::config::colors::error),
              "\n{} No transition available for state '{}' and symbols ({})\n",
              npda::config::symbols::error,
              fmt::format("{}", current.s),
              join_symbols(current_symbols)
            ));
          } else {
            sink(fmt::format(
              "\nNo transition available for state '{}' and symbols ({})\n",
              fmt::format("{}", current.s),
              join_symbols(current_symbols)
            ));
          }
        }
        break;
      }
      const auto& rule = rules_[it->second].multi;
      apply_multi_tape_transition(current, rule);
      ++steps;
    }
    return RunResult{false, steps, std::nullopt, {}, {}, config_};
  }

  // Witness path required
  std::vector<NodeType> nodes;
  nodes.reserve(1024);
  nodes.push_back(make_root());
  std::deque<std::size_t> work;
  work.push_back(0);

  while (!work.empty()) {
    std::size_t idx = work.front();
    work.pop_front();
    const NodeType& current = nodes[idx];  // refer to stored node

    // Trace current step
    if (opt.trace) {
      emit_trace_step(current, steps, std::nullopt, opt);
    }

    if (is_accepting(current.s)) {
      return build_result(current, nodes, idx, steps, opt);
    }

    if (steps >= opt.max_steps) {
      return std::unexpected(Error{"max_steps reached"});
    }

    // Find applicable transition
    std::vector<TapeSym> current_symbols;
    current_symbols.reserve(config_.num_tapes);
    for (std::size_t i = 0; i < config_.num_tapes; ++i) {
      if (current.head_positions[i] < current.tapes[i].size()) {
        current_symbols.push_back(current.tapes[i][current.head_positions[i]]);
      } else {
        current_symbols.push_back(blank_);
      }
    }

    auto key = std::make_pair(current.s, current_symbols);
    auto it = multi_transitions_.find(key);

    if (it == multi_transitions_.end()) {
      // No transition available - halt and reject
      if (opt.trace) {
        auto sink = opt.trace_sink ? opt.trace_sink : [](std::string_view s) {
          fmt::print("{}", s);
        };
        if (opt.trace_colors) {
          sink(fmt::format(
            fmt::fg(npda::config::colors::error),
            "\n{} No transition available for state '{}' and symbols ({})\n",
            npda::config::symbols::error,
            fmt::format("{}", current.s),
            join_symbols(current_symbols)
          ));
        } else {
          sink(fmt::format(
            "\nNo transition available for state '{}' and symbols ({})\n",
            fmt::format("{}", current.s),
            join_symbols(current_symbols)
          ));
        }
      }
      break;
    }

    // Apply transition (deterministic, so only one)
    const auto& rule = rules_[it->second].multi;
    NodeType next = current;  // copy to produce child
    apply_multi_tape_transition(next, rule);
    ++steps;
    next.parent = idx;
    next.rule_idx = it->second;
    nodes.push_back(std::move(next));
    work.push_back(nodes.size() - 1);
  }

  return RunResult{false, steps, std::nullopt, {}, {}, config_};
}

}  // namespace turing