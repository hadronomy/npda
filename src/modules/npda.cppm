// Own the NPDA engine: automaton, rules, and trace views.
// Import everything below. Change this file to change the API.
export module npda;
import std;
import ansi;
import config;
import lex;

export namespace npda {

template <typename T>
concept Hashable = std::equality_comparable<T> && requires(const T& t) {
  { std::hash<T>{}(t) } -> std::convertible_to<std::size_t>;
};

}  // namespace npda
export namespace npda {

template <typename State, typename StackSym>
struct Key {
  State s{};
  std::size_t pos = 0;
  std::vector<StackSym> stack{};
  bool operator==(const Key& o) const { return s == o.s && pos == o.pos && stack == o.stack; }
};

}  // namespace npda
export namespace npda {

template <typename State, typename StackSym>
struct KeyHash {
  [[nodiscard]] std::size_t operator()(const struct Key<State, StackSym>& k) const {
    std::size_t h = std::hash<State>{}(k.s);
    h = combine(h, std::hash<std::size_t>{}(k.pos));
    h = combine(h, vec_hash(k.stack));
    return h;
  }

  [[nodiscard]] static std::size_t combine(std::size_t a, std::size_t b) {
    // 64-bit mix (splitmix64-ish)
    std::size_t x = a ^ (b + 0x9e3779b97f4a7c15ULL + (a << 6) + (a >> 2));
    return x;
  }

  [[nodiscard]] static std::size_t vec_hash(const std::vector<StackSym>& v) {
    std::size_t h = 0xcbf29ce484222325ULL;  // FNV offset
    for (const auto& e : v) {
      std::size_t eh = std::hash<StackSym>{}(e);
      h ^= eh;
      h *= 0x100000001b3ULL;  // FNV prime
    }
    return h;
  }
};

}  // namespace npda
export namespace npda {

// Hash for state/input pairs in the transition index.
template <typename State, typename Input>
struct PairHash {
  std::size_t operator()(const std::pair<State, Input>& p) const noexcept {
    std::size_t h1 = std::hash<State>{}(p.first);
    std::size_t h2 = std::hash<Input>{}(p.second);
    // boost-ish hash combine
    return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
  }
};

}  // namespace npda
export namespace npda {

template <typename State, typename StackSym>
struct Node {
  State s{};
  std::size_t pos = 0;  // index into input
  std::vector<StackSym> stack{};
  std::size_t parent = static_cast<std::size_t>(-1);
  std::optional<std::size_t> rule_idx{};
};

}  // namespace npda
export namespace npda {

template <Hashable State, Hashable Input, Hashable StackSym>
struct Rule {
  State from{};
  std::optional<Input> input{};         // std::nullopt = epsilon
  std::optional<StackSym> stack_top{};  // if set, must match top and pop
  State to{};
  std::vector<StackSym> push{};  // left-to-right; last becomes new top
};

}  // namespace npda
export namespace npda {

// Generate color for non-accepting states (avoids green hues)
// Takes a hash value as parameter to ensure consistent colors
[[nodiscard]] inline ansi::rgb generate_non_accepting_state_color(std::size_t state_hash) {
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

  return ansi::rgb{red, green, blue};
}

}  // namespace npda
export namespace npda {

// Code-point count. Display math uses this, never byte length.
[[nodiscard]] inline std::size_t cpsize(std::string_view s) {
  std::size_t n = 0;
  for (unsigned char c : s)
    if ((c & 0xC0) != 0x80)
      ++n;
  return n;
}

// Cut to N code points. Never splits UTF-8.
[[nodiscard]] inline std::string cpcut(std::string_view s, std::size_t n) {
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
[[nodiscard]] inline std::size_t visible_width(std::string_view s) {
  std::size_t w = 0;
  for (std::size_t i = 0; i < s.size();) {
    if (s[i] == '\x1b' && i + 1 < s.size() && s[i + 1] == '[') {
      std::size_t j = i + 2;
      while (j < s.size() && s[j] != 'm')
        ++j;
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
[[nodiscard]] inline std::string wrap_text(const std::string& text, std::size_t width = 35) {
  // Greedy word pack on visible width. Words keep their escapes.
  std::string result;
  std::size_t vis = 0;
  std::size_t i = 0;
  const auto take_word = [&](std::size_t from) {
    std::size_t j = from;
    while (j < text.size() && text[j] != ' ' && text[j] != '\n') {
      if (text[j] == '\x1b' && j + 1 < text.size() && text[j + 1] == '[') {
        std::size_t k = j + 2;
        while (k < text.size() && text[k] != 'm')
          ++k;
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
    const std::size_t w = visible_width(word);
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
template <Hashable State, Hashable Input, Hashable StackSym>
[[nodiscard]] std::string explain_rule(const Rule<State, Input, StackSym>& r) {
  std::string explanation;

  // Build colored explanation parts
  std::string header =
    ansi::format(ansi::fg(config::colors::section_heading), "Explanation: Transition: ");
  explanation += header;

  // From state (colored as command name)
  std::string from_state_colored =
    ansi::format(ansi::fg(config::colors::command_name), "'{}'", r.from);
  explanation += std::format("From state {} ", from_state_colored);

  // Input condition
  if (r.input.has_value()) {
    std::string input_colored =
      ansi::format(ansi::fg(config::colors::warning), "'{}'", r.input.value());
    explanation += std::format("when reading {} ", input_colored);
  } else {
    explanation +=
      ansi::format(ansi::fg(config::colors::info), "without consuming input (ε-transition) ");
  }

  // Stack condition
  if (r.stack_top.has_value()) {
    std::string stack_colored =
      ansi::format(ansi::fg(config::colors::warning), "'{}'", r.stack_top.value());
    explanation += std::format("with {} on top of stack ", stack_colored);
  } else {
    explanation += ansi::format(ansi::fg(config::colors::info), "regardless of stack contents ");
  }

  // Action and destination (arrow colored as example, state as command)
  std::string to_state_colored = ansi::format(ansi::fg(config::colors::command_name), "'{}'", r.to);
  std::string arrow_colored = ansi::format(ansi::fg(config::colors::example), "→");
  explanation += arrow_colored;
  explanation += std::format(" move to state {} ", to_state_colored);

  // Stack operation with colorized symbols
  if (r.stack_top.has_value() && !r.push.empty()) {
    std::string stack_top_colored =
      ansi::format(ansi::fg(config::colors::warning), "'{}'", r.stack_top.value());
    explanation += std::format(" and replace {} with ", stack_top_colored);
    for (std::size_t i = 0; i < r.push.size(); ++i) {
      std::string push_sym_colored =
        ansi::format(ansi::fg(config::colors::warning), "'{}'", r.push[i]);
      explanation += push_sym_colored;
      if (i + 1 < r.push.size())
        explanation += ", ";
    }
  } else if (r.stack_top.has_value()) {
    std::string stack_top_colored =
      ansi::format(ansi::fg(config::colors::warning), "'{}'", r.stack_top.value());
    explanation += std::format(" and pop {} from stack", stack_top_colored);
  } else if (!r.push.empty()) {
    explanation += " and push ";
    if (r.push.size() == 1) {
      std::string push_sym_colored =
        ansi::format(ansi::fg(config::colors::warning), "'{}'", r.push[0]);
      explanation += std::format("{} onto stack", push_sym_colored);
    } else {
      explanation += "'";
      for (std::size_t i = 0; i < r.push.size(); ++i) {
        std::string push_sym_colored =
          ansi::format(ansi::fg(config::colors::warning), "'{}'", r.push[i]);
        explanation += push_sym_colored;
        if (i + 1 < r.push.size())
          explanation += ", ";
      }
      explanation += "' onto stack";
    }
  } else {
    explanation += ansi::format(ansi::fg(config::colors::info), " without changing stack");
  }

  explanation += ".";
  return wrap_text(explanation, 80);
}

}  // namespace npda
// C++23 NPDA (nondeterministic pushdown automaton), header-only.
// - Epsilon transitions (input == nullopt)
// - Accept by final state, empty stack, or both (after full input)
// - BFS/DFS exploration
// - Loop-avoidance with visited configurations
// - Optional witness reconstruction (indices of rules taken)





export namespace npda {

enum class AcceptBy { FinalState, EmptyStack, Both, Any };

struct Error {
  std::string message;
};

// Trace output options: what to print and where to send it.
struct TraceOptions {
  // Pretty, colored per-step trace diagrams printed during run.
  bool enabled = false;
  // Optional sink; if not set and tracing is on, prints via std::print.
  std::function<void(std::string_view)> sink = {};

  // Trace formatting options
  bool colors = true;
  bool compact = false;
  bool explanations = false;

  // Backtracking visualization options
  bool show_backtracking = true;  // Enable backtracking detection and visualization
  bool show_full_trace = true;    // Show complete execution trace including backtracks

  // Output cap. Live steps, replay steps, and tree rows each stop here
  // with a hidden count. Bounds trace bytes regardless of max_expansions.
  std::size_t step_limit = 200;
};

struct RunOptions {
  // If true, BFS (returns a shortest-transition witness). If false, DFS.
  bool bfs = true;

  // Hard limit on expanded transitions to prevent blow-ups/infinite loops.
  std::size_t max_expansions = 1000000;

  // Track and return a witness (sequence of rule indices). Costs memory.
  bool track_witness = true;

  TraceOptions trace{};
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
      m.build_indices();
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

  // Copies share nothing mutable: each copy rebuilds indices on first run.
  NPDA(const NPDA& o)
      : start_(o.start_),
        accepting_(o.accepting_),
        policy_(o.policy_),
        bottom_(o.bottom_),
        rules_(o.rules_),
        indices_built_(false),
        indices_once_(std::make_unique<std::once_flag>()) {}
  NPDA& operator=(const NPDA& o) {
    if (this != &o) {
      start_ = o.start_;
      accepting_ = o.accepting_;
      policy_ = o.policy_;
      bottom_ = o.bottom_;
      rules_ = o.rules_;
      epsilon_by_from_.clear();
      consume_by_from_input_.clear();
      indices_built_ = false;
      indices_once_ = std::make_unique<std::once_flag>();
    }
    return *this;
  }

  // Moves transfer the cache; the source resets for a lazy rebuild.
  NPDA(NPDA&& o) noexcept
      : start_(std::move(o.start_)),
        accepting_(std::move(o.accepting_)),
        policy_(o.policy_),
        bottom_(std::move(o.bottom_)),
        rules_(std::move(o.rules_)),
        epsilon_by_from_(std::move(o.epsilon_by_from_)),
        consume_by_from_input_(std::move(o.consume_by_from_input_)),
        indices_built_(o.indices_built_),
        indices_once_(std::make_unique<std::once_flag>()) {
    o.epsilon_by_from_.clear();
    o.consume_by_from_input_.clear();
    o.indices_built_ = false;
    o.indices_once_ = std::make_unique<std::once_flag>();
  }
  NPDA& operator=(NPDA&& o) noexcept {
    if (this != &o) {
      start_ = std::move(o.start_);
      accepting_ = std::move(o.accepting_);
      policy_ = o.policy_;
      bottom_ = std::move(o.bottom_);
      rules_ = std::move(o.rules_);
      epsilon_by_from_ = std::move(o.epsilon_by_from_);
      consume_by_from_input_ = std::move(o.consume_by_from_input_);
      indices_built_ = o.indices_built_;
      indices_once_ = std::make_unique<std::once_flag>();
      o.epsilon_by_from_.clear();
      o.consume_by_from_input_.clear();
      o.indices_built_ = false;
      o.indices_once_ = std::make_unique<std::once_flag>();
    }
    return *this;
  }

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
    // Cap the upfront reserve: max_expansions bounds the search.
    constexpr std::size_t kMaxReserve = 65536;
    nodes.reserve(std::min(opt.max_expansions, kMaxReserve));

    auto make_root = [&] {
      NodeType node;
      node.s = start_;
      node.pos = 0;
      node.stack.clear();
      node.stack.push_back(bottom_);
      node.parent = npos;
      node.rule_idx = std::nullopt;
      return node;
    };

    std::deque<std::size_t> work;
    nodes.push_back(make_root());
    work.push_back(0);

    std::unordered_set<KeyType, KeyHashType> visited;
    visited.reserve(std::min(opt.max_expansions, kMaxReserve));
    visited.insert(KeyType{nodes[0].s, nodes[0].pos, nodes[0].stack});

    std::size_t expansions = 0;

    // Exploration tracking: record all nodes and their exploration status
    std::vector<std::size_t> explored_nodes;  // Node indices that were explored
    std::vector<std::size_t> deadend_nodes;   // Node indices that are dead-ends
    bool exploration_detected = false;

    auto push_node = [&](NodeType&& node, std::size_t parent_idx, std::size_t rule_idx) -> void {
      KeyType k{node.s, node.pos, node.stack};
      if (visited.insert(std::move(k)).second) {
        node.parent = parent_idx;
        node.rule_idx = rule_idx;
        nodes.push_back(std::move(node));
        work.push_back(nodes.size() - 1);
      }
    };

    std::size_t best_trace_idx = 0;       // Track the most advanced node for trace
    std::size_t max_pos = 0;              // Track the furthest input position
    std::size_t exploration_counter = 0;  // Counter for exploration steps
    bool cap_noticed = false;             // Step cap notice prints once

    while (!work.empty()) {
      std::size_t idx = opt.bfs ? work.front() : work.back();
      if (opt.bfs)
        work.pop_front();
      else
        work.pop_back();
      const NodeType& current = nodes[idx];  // ref; re-fetch as nodes[idx] after pushes below

      // Track every dequeued node so counts stay honest when emission caps.
      if (opt.trace.enabled)
        explored_nodes.push_back(idx);

      // Track that we're exploring this node and show full detailed trace
      if (opt.trace.enabled && opt.trace.show_full_trace) {
        if (exploration_counter < opt.trace.step_limit) {
          // Show full detailed trace for this exploration step
          // Find the rule that led to this node (if any)
          std::optional<rule_type> exploration_rule = std::nullopt;
          if (current.rule_idx.has_value() && *current.rule_idx < rules_.size()) {
            exploration_rule = rules_[*current.rule_idx];
          }

          emit_trace_step(current, input, exploration_counter++, exploration_rule, opt, false, true);
        } else if (!cap_noticed) {
          cap_noticed = true;
          auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
            std::print("{}", s);
          };
          sink(std::format(
            "… (live trace stops at {} steps, raise --trace-limit to expand)\n",
            opt.trace.step_limit
          ));
        }
      }

      if (is_accepting(current, input)) {
        return build_result(
          current,
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
      if (current.pos > max_pos) {
        max_pos = current.pos;
        best_trace_idx = idx;
      }

      if (expansions >= opt.max_expansions) {
        // If tracing is enabled and we have explored some paths, show the best
        // trace we found
        if (opt.trace.enabled && nodes.size() > 1) {
          show_rejection_trace(nodes, best_trace_idx, input, opt, expansions);
        }
        return std::unexpected(Error{"max_expansions reached"});
      }

      // Expand children and detect dead-ends for DFS backtracking annotation
      std::size_t before = nodes.size();

      // Generate epsilon transitions using indices
      auto epsilon_it = epsilon_by_from_.find(nodes[idx].s);
      if (epsilon_it != epsilon_by_from_.end()) {
        for (std::size_t ri : epsilon_it->second) {
          const auto& r = rules_[ri];
          if (!stack_matches(nodes[idx].stack, r.stack_top))
            continue;

          NodeType next = nodes[idx];
          apply_stack(next.stack, r);
          next.s = r.to;
          ++expansions;
          push_node(std::move(next), idx, ri);
          if (expansions >= opt.max_expansions)
            return std::unexpected(Error{"max_expansions reached"});
        }
      }

      // Generate consuming transitions (if input left) using indices
      if (current.pos < input.size()) {
        const Input sym = input[current.pos];
        auto consume_it = consume_by_from_input_.find({nodes[idx].s, sym});
        if (consume_it != consume_by_from_input_.end()) {
          for (std::size_t ri : consume_it->second) {
            const auto& r = rules_[ri];
            if (!stack_matches(nodes[idx].stack, r.stack_top))
              continue;

            NodeType nxt = nodes[idx];
            apply_stack(nxt.stack, r);
            nxt.s = r.to;
            nxt.pos = nodes[idx].pos + 1;
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
        if (opt.trace.enabled && opt.trace.show_full_trace) {
          auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
            std::print("{}", s);
          };
          if (opt.trace.colors) {
            sink(ansi::format(ansi::fg(config::colors::info), "\n{} Exploration: dead-end at position {} (state {}), "
              "no applicable transitions\n", config::symbols::info, nodes[idx].pos, std::format("{}", nodes[idx].s)));
          } else {
            sink(std::format(
              "\nExploration: dead-end at position {} (state {}), "
              "no applicable transitions\n",
              nodes[idx].pos,
              std::format("{}", nodes[idx].s)
            ));
          }
        }
      }
    }

    // If tracing is enabled and we explored some paths, show the best trace we
    // found
    if (opt.trace.enabled && nodes.size() > 1) {
      show_rejection_trace(nodes, best_trace_idx, input, opt, expansions);
    }

    // Show exploration tree if enabled
    if (opt.trace.enabled && opt.trace.show_full_trace && !explored_nodes.empty()) {
      show_exploration_tree(nodes, explored_nodes, input, opt, std::vector<std::size_t>{});
    }

    return RunResult{false, expansions, std::nullopt};
  }

  // Trace visualization methods (declarations only; implemented out-of-class)
  template <typename NodeT>
  void emit_trace_step(
    const NodeT& node,
    const std::vector<Input>& input,
    std::size_t step_num,
    const std::optional<rule_type>& rule = std::nullopt,
    const RunOptions& opt = {},
    bool is_backtrack_point = false,
    bool is_exploration = false
  ) const;

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
  ) const;

  template <typename NodeT>
  void replay_trace_path(
    const std::vector<NodeT>& nodes,
    std::size_t final_idx,
    const std::vector<Input>& input,
    const RunOptions& opt
  ) const;

  template <typename NodeT>
  void show_rejection_trace(
    const std::vector<NodeT>& nodes,
    std::size_t best_idx,
    const std::vector<Input>& input,
    const RunOptions& opt,
    std::size_t expansions
  ) const;

  template <typename NodeT>
  void show_exploration_tree(
    const std::vector<NodeT>& nodes,
    const std::vector<std::size_t>& explored_nodes,
    [[maybe_unused]] const std::vector<Input>& input,
    const RunOptions& opt
  ) const;

  template <typename NodeT>
  void show_exploration_tree(
    const std::vector<NodeT>& nodes,
    const std::vector<std::size_t>& explored_nodes,
    [[maybe_unused]] const std::vector<Input>& input,
    const RunOptions& opt,
    const std::vector<std::size_t>& accepting_path
  ) const;

  State start_{};
  std::vector<State> accepting_{};
  AcceptBy policy_{AcceptBy::FinalState};
  StackSym bottom_{};
  std::vector<rule_type> rules_{};

  // Transition indices for efficient rule lookup
  mutable std::unordered_map<State, std::vector<std::size_t>> epsilon_by_from_;
  mutable std::unordered_map<std::pair<State, Input>, std::vector<std::size_t>, PairHash<State, Input>>
    consume_by_from_input_;
  mutable bool indices_built_ = false;
  mutable std::unique_ptr<std::once_flag> indices_once_ =
    std::make_unique<std::once_flag>();

  // Constants and helper functions
  static constexpr std::size_t npos = static_cast<std::size_t>(-1);

  [[nodiscard]] static bool
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
    std::call_once(*indices_once_, [this]() {
      // Clear first: copies and moves arrive with a fresh flag but
      // may carry maps, so rebuilding must stay idempotent.
      epsilon_by_from_.clear();
      consume_by_from_input_.clear();
      // One entry per rule at most, split across both maps.
      epsilon_by_from_.reserve(rules_.size());
      consume_by_from_input_.reserve(rules_.size());
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
    });
  }

  // Check if a node satisfies the acceptance policy
  template <typename NodeT>
  [[nodiscard]] bool is_accepting(const NodeT& n, std::span<const Input> input) const {
    const bool at_end = (n.pos == input.size());
    const bool by_state = std::ranges::contains(accepting_, n.s);
    // const bool by_stack =
    //   (n.stack.size() == 0 || (n.stack.size() == 1 && n.stack.back() == bottom_));
    const bool by_stack = n.stack.size() == 0;
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

//
// Out-of-class implementations for trace visualization methods
//

template <Hashable State, Hashable Input, Hashable StackSym>
template <typename NodeT>
void NPDA<State, Input, StackSym>::emit_trace_step(
  const NodeT& node,
  const std::vector<Input>& input,
  std::size_t step_num,
  const std::optional<rule_type>& rule,
  const RunOptions& opt,
  bool is_backtrack_point,
  bool is_exploration
) const {
  if (!opt.trace.enabled)
    return;

  auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
    std::print("{}", s);
  };

  std::string output;

  // Header with step number and exploration/backtracking indicator
  if (opt.trace.colors) {
    if (is_exploration) {
      output +=
        ansi::format(ansi::fg(config::colors::info), "\n=== Exploration Step {} ===\n", step_num);
    } else if (is_backtrack_point && opt.trace.show_backtracking) {
      output += ansi::format(ansi::fg(config::colors::warning), "\n=== Step {} {}(BACKTRACK) ===\n", step_num, config::symbols::warning);
    } else {
      output +=
        ansi::format(ansi::fg(config::colors::section_heading), "\n=== Step {} ===\n", step_num);
    }
  } else {
    if (is_exploration) {
      output += std::format("\n=== Exploration Step {} ===\n", step_num);
    } else if (is_backtrack_point && opt.trace.show_backtracking) {
      output += std::format("\n=== Step {} (BACKTRACK) ===\n", step_num);
    } else {
      output += std::format("\n=== Step {} ===\n", step_num);
    }
  }

  // Current state
  if (opt.trace.colors) {
    output += ansi::format(ansi::fg(config::colors::info), "State: ");
    output += ansi::format(ansi::fg(config::colors::success), "{}\n", std::format("{}", node.s));
  } else {
    output += std::format("State: {}\n", std::format("{}", node.s));
  }

  // Input window around the head. Full tapes flood; windows do not.
  // Cells cap at 6 code points so fixed-stride math below holds.
  constexpr std::size_t kWindow = 8;
  const std::string kEllipsis = ansi::unicode_enabled() ? "… " : "... ";
  output += "Input: ";
  const std::size_t ilo = (node.pos > kWindow) ? node.pos - kWindow : 0;
  const std::size_t ihi = std::min(input.size(), node.pos + kWindow + 1);
  if (ilo > 0)
    output += kEllipsis;
  for (std::size_t i = ilo; i < ihi; ++i) {
    const std::string cell = cpcut(std::format("{}", input[i]), 6);
    if (i == node.pos) {
      if (opt.trace.colors) {
        output +=
          ansi::format(ansi::fg(config::colors::warning), "[{}]", cell);
      } else {
        output += std::format("[{}]", cell);
      }
    } else {
      output += std::format(" {} ", cell);
    }
  }
  if (node.pos >= input.size()) {
    if (opt.trace.colors) {
      output += ansi::format(ansi::fg(config::colors::success), " [END]");
    } else {
      output += " [END]";
    }
  } else if (ihi < input.size()) {
    output += kEllipsis;
  }
  output += "\n";

  // Stack visualization. Shows the top cells only, plus a count.
  constexpr std::size_t kStack = 9;
  output += "Stack: ";
  if (node.stack.empty()) {
    output += "(empty)\n";
  } else {
    const std::size_t shown = std::min(node.stack.size(), kStack);
    auto cell3 = [](std::string s) {
      s = cpcut(s, 3);
      while (cpsize(s) < 3)
        s += ' ';
      return s;
    };
    // Show stack top on the right (conventional)
    for (std::size_t i = 0; i < shown; ++i) {
      std::size_t stack_idx = node.stack.size() - 1 - i;  // top first
      const std::string cell = cell3(std::format("{}", node.stack[stack_idx]));
      if (i == 0) {
        if (opt.trace.colors) {
          output += ansi::format(ansi::fg(config::colors::warning), "[{}]", cell);
        } else {
          output += std::format("[{}]", cell);
        }
      } else {
        output += std::format(" {} ", cell);
      }
    }
    if (shown < node.stack.size())
      output += std::format("(+{} more)", node.stack.size() - shown);
    output += "\n";

    // Visual stack representation
    if (!opt.trace.compact) {
      const bool uni = ansi::unicode_enabled();
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
      for (std::size_t i = 0; i < shown; ++i) {
        std::size_t stack_idx = node.stack.size() - 1 - i;
        const std::string cell = cell3(std::format("{}", node.stack[stack_idx]));
        if (i == 0) {
          if (opt.trace.colors) {
            output += ansi::format(ansi::fg(config::colors::warning), "{} {} {}", vv, cell, vv);
          } else {
            output += std::format("{} {} {}", vv, cell, vv);
          }
        } else {
          output += std::format(" {} {}", cell, vv);
        }
      }
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

  // Rule information
  if (rule.has_value()) {
    const auto& r = rule.value();
    if (opt.trace.colors) {
      output += ansi::format(ansi::fg(config::colors::info), "Rule: ");
    } else {
      output += "Rule: ";
    }

    // Format rule nicely
    std::string rule_str = std::format("{} → ", std::format("{}", r.from));

    // Input symbol
    if (r.input.has_value()) {
      rule_str += std::format("{}, ", std::format("{}", r.input.value()));
    } else {
      rule_str += "ε, ";
    }

    // Stack operation
    if (r.stack_top.has_value()) {
      rule_str += std::format("pop({}) → ", std::format("{}", r.stack_top.value()));
    } else {
      rule_str += "nop → ";
    }

    rule_str += std::format("{}, ", std::format("{}", r.to));

    // Push symbols
    if (r.push.empty()) {
      rule_str += "push()";
    } else {
      rule_str += "push(";
      for (std::size_t i = 0; i < r.push.size(); ++i) {
        rule_str += std::format("{}", std::format("{}", r.push[i]));
        if (i + 1 < r.push.size())
          rule_str += ", ";
      }
      rule_str += ")";
    }

    if (opt.trace.colors) {
      output += ansi::format(ansi::fg(config::colors::example), "{}\n", rule_str);
    } else {
      output += std::format("{}\n", rule_str);
    }

    // Add natural language explanation if enabled
    if (opt.trace.explanations) {
      std::string explanation = npda::explain_rule(r);
      if (opt.trace.colors) {
        output += ansi::format(ansi::fg(config::colors::info), "{}\n", explanation);
      } else {
        output += std::format("{}\n", explanation);
      }
    }
  }

  sink(output);
}

template <Hashable State, Hashable Input, Hashable StackSym>
template <typename NodeT>
std::expected<RunResult, Error> NPDA<State, Input, StackSym>::build_result(
  [[maybe_unused]] const NodeT& acc_node,
  const std::vector<NodeT>& nodes,
  std::size_t idx,
  std::size_t expansions,
  const RunOptions& opt,
  const std::vector<Input>& input,
  bool exploration_detected,
  const std::vector<std::size_t>& explored_nodes,
  const std::vector<std::size_t>& deadend_nodes
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
  if (opt.trace.enabled) {
    replay_trace_path(nodes, idx, input, opt);

    // Show exploration summary if enabled
    if (opt.trace.show_backtracking && exploration_detected) {
      auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
        std::print("{}", s);
      };

      if (opt.trace.colors) {
        sink(ansi::format(ansi::fg(config::colors::info), "\n{} Exploration summary: {} nodes explored, {} dead-ends found\n", config::symbols::info, explored_nodes.size(), deadend_nodes.size()));
      } else {
        sink(std::format(
          "\nExploration summary: {} nodes explored, {} dead-ends found\n",
          explored_nodes.size(),
          deadend_nodes.size()
        ));
      }
    }
  }

  // Show exploration tree if enabled
  if (opt.trace.enabled && opt.trace.show_full_trace && !explored_nodes.empty()) {
    show_exploration_tree(nodes, explored_nodes, input, opt, std::vector<std::size_t>{});
  }

  return RunResult{true, expansions, std::move(path)};
}

template <Hashable State, Hashable Input, Hashable StackSym>
template <typename NodeT>
void NPDA<State, Input, StackSym>::replay_trace_path(
  const std::vector<NodeT>& nodes,
  std::size_t final_idx,
  const std::vector<Input>& input,
  const RunOptions& opt
) const {
  if (!opt.trace.enabled || !opt.track_witness)
    return;

  // Reconstruct node and rule paths from root to final
  std::vector<std::size_t> node_path_rev;
  std::vector<std::size_t> rule_path_rev;  // rules applied (except root)

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

  auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
    std::print("{}", s);
  };

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::banner_text), "\n{} Accepting path found! Replaying {} steps...\n", config::symbols::info, rule_path.size()));
  } else {
    sink(std::format("\nAccepting path found! Replaying {} steps...\n", rule_path.size()));
  }

  // Live search already showed every path node with its rule.
  // Print steps only when live emission stayed off.
  if (!opt.trace.show_full_trace) {
    // Slice long paths: head, hidden count, tail. The node after a gap
    // prints without a rule since its parent stays hidden.
    std::vector<std::size_t> show;
    if (node_path.size() > opt.trace.step_limit + 1 && opt.trace.step_limit > 1) {
      const std::size_t head = opt.trace.step_limit / 2;
      const std::size_t tail = opt.trace.step_limit - head;
      for (std::size_t i = 0; i < head; ++i)
        show.push_back(i);
      for (std::size_t i = node_path.size() - tail; i < node_path.size(); ++i)
        show.push_back(i);
    } else {
      for (std::size_t i = 0; i < node_path.size(); ++i)
        show.push_back(i);
    }

    std::size_t step_num = 0;
    for (std::size_t k = 0; k < show.size(); ++k) {
      const std::size_t i = show[k];
      if (k > 0 && i != show[k - 1] + 1) {
        sink(std::format(
          "… ({} steps hidden, raise --trace-limit to expand)\n", i - show[k - 1] - 1
        ));
      }
      std::optional<rule_type> rr = std::nullopt;
      if (i > 0 && k > 0 && i == show[k - 1] + 1)
        rr = rules_[rule_path[i - 1]];
      const std::size_t node_idx = node_path[i];
      const bool is_backtrack = (k > 0 && nodes[node_idx].pos < nodes[node_path[show[k - 1]]].pos);
      emit_trace_step(nodes[node_idx], input, step_num++, rr, opt, is_backtrack);
    }
  }

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::success), "\n{} Input accepted!\n", config::symbols::success));
  } else {
    sink("\nInput accepted!\n");
  }
}

template <Hashable State, Hashable Input, Hashable StackSym>
template <typename NodeT>
void NPDA<State, Input, StackSym>::show_rejection_trace(
  const std::vector<NodeT>& nodes,
  std::size_t best_idx,
  const std::vector<Input>& input,
  const RunOptions& opt,
  std::size_t expansions
) const {
  if (!opt.trace.enabled || !opt.track_witness)
    return;

  auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
    std::print("{}", s);
  };

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::banner_text), "\n{} Input rejected! Showing furthest path explored ({} "
      "expansions)...\n", config::symbols::error, expansions));
  } else {
    sink(std::format(
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

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::info), "Furthest position: {} / {} ({} characters remaining)\n", best_node.pos, input.size(), remaining_input));
  } else {
    sink(std::format(
      "Furthest position: {} / {} ({} characters remaining)\n",
      best_node.pos,
      input.size(),
      remaining_input
    ));
  }

  // Emit the path with the same slicing as the accept replay.
  // The stuck node is the path end, so no extra final emission.
  std::vector<std::size_t> show;
  if (node_path.size() > opt.trace.step_limit + 1 && opt.trace.step_limit > 1) {
    const std::size_t head = opt.trace.step_limit / 2;
    const std::size_t tail = opt.trace.step_limit - head;
    for (std::size_t i = 0; i < head; ++i)
      show.push_back(i);
    for (std::size_t i = node_path.size() - tail; i < node_path.size(); ++i)
      show.push_back(i);
  } else {
    for (std::size_t i = 0; i < node_path.size(); ++i)
      show.push_back(i);
  }

  std::size_t step_num = 0;
  for (std::size_t k = 0; k < show.size(); ++k) {
    const std::size_t i = show[k];
    if (k > 0 && i != show[k - 1] + 1) {
      sink(std::format(
        "… ({} steps hidden, raise --trace-limit to expand)\n", i - show[k - 1] - 1
      ));
    }
    std::optional<rule_type> rr = std::nullopt;
    if (i > 0 && k > 0 && i == show[k - 1] + 1)
      rr = rules_[rule_path[i - 1]];
    const std::size_t node_idx = node_path[i];
    const bool is_backtrack = (k > 0 && nodes[node_idx].pos < nodes[node_path[show[k - 1]]].pos);
    emit_trace_step(nodes[node_idx], input, step_num++, rr, opt, is_backtrack);
  }

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::error), "\n{} Input rejected at this point!\n", config::symbols::error));
  } else {
    sink("\nInput rejected at this point!\n");
  }
}

template <Hashable State, Hashable Input, Hashable StackSym>
template <typename NodeT>
void NPDA<State, Input, StackSym>::show_exploration_tree(
  const std::vector<NodeT>& nodes,
  const std::vector<std::size_t>& explored_nodes,
  [[maybe_unused]] const std::vector<Input>& input,
  const RunOptions& opt
) const {
  if (!opt.trace.enabled || explored_nodes.empty())
    return;

  auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
    std::print("{}", s);
  };

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::banner_text), "\n{} Exploration Tree Structure:\n", config::symbols::info));
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

  // Tree chrome follows the unicode probe. Same tree, two alphabets.
  const bool tuni = ansi::unicode_enabled();
  const std::string kLast = tuni ? "└── " : "`-- ";
  const std::string kMid = tuni ? "├── " : "+-- ";
  const std::string kVert = tuni ? "│   " : "|   ";
  std::size_t printed = 0;

  // Recursive function to print tree with enhanced visualization
  std::function<void(std::size_t, std::string, bool)> print_tree;
  print_tree = [&](std::size_t node_idx, std::string prefix, bool is_last) {
    if (printed >= opt.trace.step_limit)
      return;
    ++printed;
    const auto& node = nodes[node_idx];

    // Get current input symbol (if any) - use lambda for empty
    std::string input_sym = "λ";
    if (node.pos < input.size()) {
      input_sym = std::format("{}", input[node.pos]);
    }

    // Get stack representation - show full stack content
    std::string stack_repr;
    if (node.stack.empty()) {
      stack_repr = "[]";
    } else {
      // Show stack from bottom to top (left to right), capped with a count.
      constexpr std::size_t kTreeStack = 8;
      std::string stack_content;
      const std::size_t tshown = std::min(node.stack.size(), kTreeStack);
      for (std::size_t i = 0; i < tshown; ++i) {
        if (i > 0)
          stack_content += ",";
        stack_content += std::format("{}", node.stack[i]);
      }
      if (tshown < node.stack.size())
        stack_content += std::format(",(+{} more)", node.stack.size() - tshown);
      stack_repr = std::format("[{}]", stack_content);
    }

    // Colorize based on state using type-safe approach
    ansi::rgb state_color;

    // Use a hash-based approach that works with any state type
    // First check if it's an accepting state (highest priority)
    if (std::ranges::contains(accepting_, node.s)) {
      // Accepting states get a special warm color
      state_color = config::colors::success;  // Green for accepting states
    } else {
      // For non-accepting states, use our specialized color generator.
      // Hash the table bytes, not the ID, so colors stay stable.
      std::size_t state_hash = std::hash<std::string>{}(std::string(lex::name(node.s)));
      state_color = generate_non_accepting_state_color(state_hash);
    }

    // Colorize input symbol
    ansi::rgb input_color = (input_sym == "λ")
                           ? config::colors::banner_text       // Mauve for lambda
                           : config::colors::section_heading;  // Yellow for others

    // Colorize stack
    ansi::rgb stack_color = config::colors::option_name;  // Sky blue for stack

    // Show enhanced node info with colors
    std::string node_info;
    if (opt.trace.colors) {
      node_info = std::format(
        "[{}] pos:{} {} state:{} {} ({})",
        node_idx,
        node.pos,
        ansi::format(ansi::fg(input_color), "{}", input_sym),
        ansi::format(ansi::fg(state_color), "{}", node.s),
        ansi::format(ansi::fg(stack_color), "{}", stack_repr),
        node.stack.size()
      );
    } else {
      node_info = std::format(
        "[{}] pos:{} input:{} state:{} {} ({})",
        node_idx,
        node.pos,
        input_sym,
        std::format("{}", node.s),
        stack_repr,
        node.stack.size()
      );
    }

    if (opt.trace.colors) {
      // Colorize tree connectors using config colors
      ansi::rgb connector_color = config::colors::progress;  // Subtle gray from config
      std::string connector = is_last ? kLast : kMid;
      sink(std::format(
        "{}{} {}\n", prefix, ansi::format(ansi::fg(connector_color), "{}", connector), node_info
      ));
    } else {
      sink(std::format("{}{} {}\n", prefix, is_last ? kLast : kMid, node_info));
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
            is_last ? "    " : ansi::format(ansi::fg(config::colors::progress), "{}", kVert);
          child_prefix = prefix + vertical_connector;
        } else {
          child_prefix = prefix + (is_last ? "    " : kVert);
        }
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
  if (printed < explored_nodes.size()) {
    sink(std::format(
      "… ({} more nodes hidden, raise --trace-limit to expand)\n",
      explored_nodes.size() - printed
    ));
  }
}

template <Hashable State, Hashable Input, Hashable StackSym>
template <typename NodeT>
void NPDA<State, Input, StackSym>::show_exploration_tree(
  const std::vector<NodeT>& nodes,
  const std::vector<std::size_t>& explored_nodes,
  [[maybe_unused]] const std::vector<Input>& input,
  const RunOptions& opt,
  const std::vector<std::size_t>& accepting_path
) const {
  // Convert accepting_path to a set for fast lookup
  std::unordered_set<std::size_t> accepting_nodes(accepting_path.begin(), accepting_path.end());

  if (!opt.trace.enabled || explored_nodes.empty())
    return;

  auto sink = opt.trace.sink ? opt.trace.sink : [](std::string_view s) {
    std::print("{}", s);
  };

  if (opt.trace.colors) {
    sink(ansi::format(ansi::fg(config::colors::banner_text), "\n{} Exploration Tree Structure:\n", config::symbols::info));
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

  // Tree chrome follows the unicode probe. Same tree, two alphabets.
  const bool tuni = ansi::unicode_enabled();
  const std::string kLast = tuni ? "└── " : "`-- ";
  const std::string kMid = tuni ? "├── " : "+-- ";
  const std::string kVert = tuni ? "│   " : "|   ";
  std::size_t printed = 0;

  // Recursive function to print tree with enhanced visualization
  std::function<void(std::size_t, std::string, bool)> print_tree;
  print_tree = [&](std::size_t node_idx, std::string prefix, bool is_last) {
    if (printed >= opt.trace.step_limit)
      return;
    ++printed;
    const auto& node = nodes[node_idx];

    // Get current input symbol (if any) - use lambda for empty
    std::string input_sym = "λ";
    if (node.pos < input.size()) {
      input_sym = std::format("{}", input[node.pos]);
    }

    // Get stack representation - show full stack content
    std::string stack_repr;
    if (node.stack.empty()) {
      stack_repr = "[]";
    } else {
      // Show stack from bottom to top (left to right), capped with a count.
      constexpr std::size_t kTreeStack = 8;
      std::string stack_content;
      const std::size_t tshown = std::min(node.stack.size(), kTreeStack);
      for (std::size_t i = 0; i < tshown; ++i) {
        if (i > 0)
          stack_content += ",";
        stack_content += std::format("{}", node.stack[i]);
      }
      if (tshown < node.stack.size())
        stack_content += std::format(",(+{} more)", node.stack.size() - tshown);
      stack_repr = std::format("[{}]", stack_content);
    }

    // Colorize based on state using type-safe approach
    ansi::rgb state_color;

    // Use a hash-based approach that works with any state type
    // First check if it's an accepting state (highest priority)
    if (std::ranges::contains(accepting_, node.s)) {
      // Accepting states get a special warm color
      state_color = config::colors::success;  // Green for accepting states
    } else {
      // For non-accepting states, use our specialized color generator.
      // Hash the table bytes, not the ID, so colors stay stable.
      std::size_t state_hash = std::hash<std::string>{}(std::string(lex::name(node.s)));
      state_color = generate_non_accepting_state_color(state_hash);
    }

    // Colorize input symbol
    ansi::rgb input_color = (input_sym == "λ")
                           ? config::colors::banner_text       // Mauve for lambda
                           : config::colors::section_heading;  // Yellow for others

    // Colorize stack
    ansi::rgb stack_color = config::colors::option_name;  // Sky blue for stack

    // Check if this node is in the accepting path
    bool is_in_accepting_path = accepting_nodes.find(node_idx) != accepting_nodes.end();
    std::string accepting_marker =
      is_in_accepting_path
        ? ansi::format(ansi::fg(config::colors::success), "{}", config::symbols::success)
        : "";

    // Show enhanced node info with colors
    std::string node_info;
    if (opt.trace.colors) {
      node_info = std::format(
        "[{}] pos:{} {} state:{} {} ({}) {}",
        node_idx,
        node.pos,
        ansi::format(ansi::fg(input_color), "{}", input_sym),
        ansi::format(ansi::fg(state_color), "{}", node.s),
        ansi::format(ansi::fg(stack_color), "{}", stack_repr),
        node.stack.size(),
        accepting_marker
      );
    } else {
      node_info = std::format(
        "[{}] pos:{} input:{} state:{} {} ({}) {}",
        node_idx,
        node.pos,
        input_sym,
        std::format("{}", node.s),
        stack_repr,
        node.stack.size(),
        is_in_accepting_path ? config::symbols::success : ""
      );
    }

    if (opt.trace.colors) {
      // Colorize tree connectors using config colors
      ansi::rgb connector_color = config::colors::progress;  // Subtle gray from config
      std::string connector = is_last ? kLast : kMid;
      sink(std::format(
        "{}{} {}\n", prefix, ansi::format(ansi::fg(connector_color), "{}", connector), node_info
      ));
    } else {
      sink(std::format("{}{} {}\n", prefix, is_last ? kLast : kMid, node_info));
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
            is_last ? "    " : ansi::format(ansi::fg(config::colors::progress), "{}", kVert);
          child_prefix = prefix + vertical_connector;
        } else {
          child_prefix = prefix + (is_last ? "    " : kVert);
        }
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
  if (printed < explored_nodes.size()) {
    sink(std::format(
      "… ({} more nodes hidden, raise --trace-limit to expand)\n",
      explored_nodes.size() - printed
    ));
  }
}

}  // namespace npda
