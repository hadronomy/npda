#include <algorithm>
#include <cstdint>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace prf {

// -------------------- Utilities --------------------

inline std::string join_u64(const std::vector<uint64_t>& xs, std::string_view sep = ", ") {
  std::ostringstream oss;
  for (size_t i = 0; i < xs.size(); ++i) {
    if (i)
      oss << sep;
    oss << xs[i];
  }
  return oss.str();
}

// -------------------- Trace --------------------

class Trace {
 public:
  struct Node {
    std::string name;
    std::vector<uint64_t> args;
    std::optional<uint64_t> result;
    size_t depth{};
    std::vector<std::unique_ptr<Node>> children;
  };

  class Scope {
   public:
    Scope(Trace& t, Node* n) : trace_(&t), node_(n) {}
    Scope(const Scope&) = delete;
    Scope& operator=(const Scope&) = delete;
    Scope(Scope&& o) noexcept : trace_(o.trace_), node_(o.node_) {
      o.trace_ = nullptr;
      o.node_ = nullptr;
    }
    Scope& operator=(Scope&& o) noexcept {
      if (this != &o) {
        leave_if_needed();
        trace_ = o.trace_;
        node_ = o.node_;
        o.trace_ = nullptr;
        o.node_ = nullptr;
      }
      return *this;
    }
    ~Scope() { leave_if_needed(); }

    void set_result(uint64_t r) {
      if (!node_)
        return;
      node_->result = r;
    }

   private:
    void leave_if_needed() {
      if (trace_ && node_) {
        trace_->leave_();
        trace_ = nullptr;
        node_ = nullptr;
      }
    }

    Trace* trace_{};
    Node* node_{};
  };

  Scope enter(std::string name, std::vector<uint64_t> args) {
    auto node = std::make_unique<Node>();
    node->name = std::move(name);
    node->args = std::move(args);
    node->depth = stack_.size();

    // Count this function call
    counts_[node->name] += 1;

    Node* raw = node.get();
    if (stack_.empty()) {
      roots_.push_back(std::move(node));
    } else {
      stack_.back()->children.push_back(std::move(node));
    }
    stack_.push_back(raw);
    return Scope(*this, raw);
  }

  void clear() {
    roots_.clear();
    stack_.clear();
    counts_.clear();
  }

  const std::vector<std::unique_ptr<Node>>& roots() const { return roots_; }

  void print(std::ostream& os) const {
    for (size_t i = 0; i < roots_.size(); ++i) {
      print_node_(os, *roots_[i], "", i + 1 == roots_.size());
    }
    // Print a summary of function call counts
    if (!counts_.empty()) {
      os << "\nSummary:\n";
      // Emit in descending count, then lexicographic by name for stability
      std::vector<std::pair<std::string, uint64_t>> items(counts_.begin(), counts_.end());
      std::sort(items.begin(), items.end(), [](const auto& a, const auto& b) {
        if (a.second != b.second)
          return a.second > b.second;
        return a.first < b.first;
      });
      for (const auto& [name, cnt] : items) {
        os << "  " << name << ": " << cnt << "\n";
      }
    }
  }

 private:
  friend class Scope;

  void leave_() {
    if (stack_.empty())
      throw std::logic_error("Trace stack underflow");
    stack_.pop_back();
  }

  void print_node_(std::ostream& os, const Node& node, std::string indent, bool last) const {
    os << indent;
    os << (last ? "└─ " : "├─ ");
    os << node.name << "(" << join_u64(node.args) << ")";
    if (node.result)
      os << " -> " << *node.result;
    os << "\n";

    std::string child_indent = indent + (last ? "   " : "│  ");
    for (size_t i = 0; i < node.children.size(); ++i) {
      print_node_(os, *node.children[i], child_indent, i + 1 == node.children.size());
    }
  }

  std::vector<std::unique_ptr<Node>> roots_;
  std::vector<Node*> stack_;
  std::unordered_map<std::string, uint64_t> counts_;
};

// -------------------- PRF Base --------------------

class Function {
 public:
  virtual ~Function() = default;
  virtual size_t arity() const = 0;
  virtual std::string_view name() const = 0;

  uint64_t operator()(const std::vector<uint64_t>& args, Trace& trace) const {
    if (args.size() != arity()) {
      std::ostringstream oss;
      oss << "Arity mismatch for '" << name() << "': expected " << arity() << ", got "
          << args.size();
      throw std::invalid_argument(oss.str());
    }
    auto scope = trace.enter(std::string{name()}, args);
    uint64_t r = eval_(args, trace);
    scope.set_result(r);
    return r;
  }

 protected:
  virtual uint64_t eval_(const std::vector<uint64_t>& args, Trace& trace) const = 0;
};

// -------------------- Primitives --------------------

class Zero : public Function {
 public:
  Zero(size_t n, std::string name = "") : n_(n) {
    name_ = name.empty() ? ("Z^" + std::to_string(n)) : std::move(name);
  }
  size_t arity() const override { return n_; }
  std::string_view name() const override { return name_; }

 protected:
  uint64_t eval_(const std::vector<uint64_t>&, Trace&) const override { return 0ULL; }

 private:
  size_t n_;
  std::string name_;
};

class Successor : public Function {
 public:
  explicit Successor(std::string name = "S") : name_(std::move(name)) {}
  size_t arity() const override { return 1; }
  std::string_view name() const override { return name_; }

 protected:
  uint64_t eval_(const std::vector<uint64_t>& args, Trace&) const override {
    if (args[0] == UINT64_MAX) {
      throw std::overflow_error("Successor overflow on uint64_t");
    }
    return args[0] + 1ULL;
  }

 private:
  std::string name_;
};

class Projection : public Function {
 public:
  Projection(size_t i, size_t n, std::string name = "") : i_(i), n_(n) {
    if (i_ == 0 || i_ > n_) {
      throw std::invalid_argument("Projection index out of range");
    }
    if (name.empty()) {
      std::ostringstream oss;
      oss << "P_" << i_ << "^" << n_;
      name_ = oss.str();
    } else {
      name_ = std::move(name);
    }
  }
  size_t arity() const override { return n_; }
  std::string_view name() const override { return name_; }

 protected:
  uint64_t eval_(const std::vector<uint64_t>& args, Trace&) const override { return args[i_ - 1]; }

 private:
  size_t i_;
  size_t n_;
  std::string name_;
};

// -------------------- Combinators --------------------

class Composition : public Function {
 public:
  Composition(
    std::shared_ptr<const Function> g,
    std::vector<std::shared_ptr<const Function>> hs,
    std::string name = ""
  )
      : g_(std::move(g)), hs_(std::move(hs)) {
    if (!g_)
      throw std::invalid_argument("Composition: g is null");
    if (hs_.empty())
      throw std::invalid_argument("Composition: no h functions provided");
    size_t n = hs_[0]->arity();
    for (const auto& h : hs_) {
      if (!h)
        throw std::invalid_argument("Composition: null h function");
      if (h->arity() != n)
        throw std::invalid_argument("Composition: mismatched h arities");
    }
    if (g_->arity() != hs_.size())
      throw std::invalid_argument("Composition: g arity mismatch (k)");

    n_ = n;
    if (name.empty()) {
      std::ostringstream oss;
      oss << "C(" << g_->name() << "∘[";
      for (size_t i = 0; i < hs_.size(); ++i) {
        if (i)
          oss << ", ";
        oss << hs_[i]->name();
      }
      oss << "])";
      name_ = oss.str();
    } else {
      name_ = std::move(name);
    }
  }

  size_t arity() const override { return n_; }
  std::string_view name() const override { return name_; }

 protected:
  uint64_t eval_(const std::vector<uint64_t>& args, Trace& trace) const override {
    std::vector<uint64_t> mid;
    mid.reserve(hs_.size());
    for (const auto& h : hs_) {
      mid.push_back((*h)(args, trace));
    }
    return (*g_)(mid, trace);
  }

 private:
  std::shared_ptr<const Function> g_;
  std::vector<std::shared_ptr<const Function>> hs_;
  size_t n_{};
  std::string name_;
};

// -------------------- Primitive Recursion (LAST arg) --------------------
// Given g: N^n -> N and h: N^(n+2) -> N, define f: N^(n+1) -> N by:
//   f(x, 0) = g(x)
//   f(x, y+1) = h(y, f(x, y), x)
// The recursion variable is the LAST argument of f.

class PrimitiveRecursion : public Function {
 public:
  PrimitiveRecursion(
    std::shared_ptr<const Function> g,
    std::shared_ptr<const Function> h,
    std::string name = ""
  )
      : g_(std::move(g)), h_(std::move(h)) {
    if (!g_ || !h_)
      throw std::invalid_argument("PrimitiveRecursion: null function");
    if (h_->arity() < 2)
      throw std::invalid_argument("PrimitiveRecursion: invalid h arity");
    if (h_->arity() != g_->arity() + 2)
      throw std::invalid_argument("PrimitiveRecursion: arity mismatch");

    // f has arity n + 1 where n = g->arity()
    n_ = g_->arity() + 1;
    if (name.empty()) {
      std::ostringstream oss;
      oss << "R_last(" << g_->name() << ", " << h_->name() << ")";
      name_ = oss.str();
    } else {
      name_ = std::move(name);
    }
  }

  size_t arity() const override { return n_; }
  std::string_view name() const override { return name_; }

 protected:
  uint64_t eval_(const std::vector<uint64_t>& args, Trace& trace) const override {
    // args = x + [y], where y is the last element.
    const size_t n = args.size();
    const uint64_t y = args.back();
    std::vector<uint64_t> xs(args.begin(), args.end() - 1);
    return eval_rec_last_(xs, y, trace);
  }

 private:
  uint64_t eval_rec_last_(const std::vector<uint64_t>& xs, uint64_t y, Trace& trace) const {
    if (y == 0ULL) {
      return (*g_)(xs, trace);
    } else {
      // Compute f(x, y-1)
      std::vector<uint64_t> prev_args;
      prev_args.reserve(xs.size() + 1);
      prev_args.insert(prev_args.end(), xs.begin(), xs.end());
      prev_args.push_back(y - 1ULL);
      uint64_t prev = this->operator()(prev_args, trace);

      // Then apply h(y-1, prev, x)
      std::vector<uint64_t> h_args;
      h_args.reserve(2 + xs.size());
      h_args.push_back(y - 1ULL);
      h_args.push_back(prev);
      h_args.insert(h_args.end(), xs.begin(), xs.end());
      return (*h_)(h_args, trace);
    }
  }

  std::shared_ptr<const Function> g_;
  std::shared_ptr<const Function> h_;
  size_t n_{};
  std::string name_;
};

// -------------------- Factories --------------------

inline std::shared_ptr<const Function> zero(size_t n, std::string name = "") {
  return std::make_shared<Zero>(n, std::move(name));
}

inline std::shared_ptr<const Function> succ(std::string name = "S") {
  return std::make_shared<Successor>(std::move(name));
}

inline std::shared_ptr<const Function> proj(size_t i, size_t n, std::string name = "") {
  return std::make_shared<Projection>(i, n, std::move(name));
}

inline std::shared_ptr<const Function> compose(
  std::shared_ptr<const Function> g,
  std::vector<std::shared_ptr<const Function>> hs,
  std::string name = ""
) {
  return std::make_shared<Composition>(std::move(g), std::move(hs), std::move(name));
}

inline std::shared_ptr<const Function> primitive_rec(
  std::shared_ptr<const Function> g,
  std::shared_ptr<const Function> h,
  std::string name = ""
) {
  return std::make_shared<PrimitiveRecursion>(std::move(g), std::move(h), std::move(name));
}

}  // namespace prf

// -------------------- Tiny DSL Layer --------------------
namespace dsl {

struct Arg {
  size_t i;
};

constexpr Arg _1{1};
constexpr Arg _2{2};
constexpr Arg _3{3};
constexpr Arg _4{4};
constexpr Arg _5{5};
constexpr Arg _6{6};
constexpr Arg _7{7};
constexpr Arg _8{8};

// A Context knows the current ambient arity n and can:
// - build Z() and id() at that arity
// - compose f with placeholders or functions of the same arity
struct Context {
  size_t n;

  std::shared_ptr<const prf::Function> Z(std::string name = "") const {
    return prf::zero(n, std::move(name));
  }
  std::shared_ptr<const prf::Function> id(std::string name = "id") const {
    return prf::proj(1, n, std::move(name));
  }
  std::shared_ptr<const prf::Function> P(size_t i, std::string name = "") const {
    return prf::proj(i, n, std::move(name));
  }

  template <typename... Ts>
  std::shared_ptr<const prf::Function> operator()(std::shared_ptr<const prf::Function> g, Ts... xs)
    const {
    std::vector<std::shared_ptr<const prf::Function>> hs;
    hs.reserve(sizeof...(Ts));
    (append_arg(hs, xs), ...);
    return prf::compose(std::move(g), std::move(hs));
  }

 private:
  void append_arg(std::vector<std::shared_ptr<const prf::Function>>& hs, Arg a) const {
    if (a.i == 0 || a.i > n) {
      throw std::invalid_argument("Context: Arg index out of range");
    }
    hs.push_back(prf::proj(a.i, n));
  }

  void append_arg(
    std::vector<std::shared_ptr<const prf::Function>>& hs,
    std::shared_ptr<const prf::Function> f
  ) const {
    if (!f)
      throw std::invalid_argument("Context: null function argument");
    if (f->arity() != n) {
      throw std::invalid_argument("Context: function arity mismatch");
    }
    hs.push_back(std::move(f));
  }
};

// Build a primitive recursion function of arity (n+1), with recursion
// over the LAST argument (this matches the core engine).
//
// Builder contracts:
// - g_builder receives Context(n) and must return g: N^n -> N
// - h_builder receives Context(n+2) and must return h: N^(n+2) -> N
//   In that context the arguments are ordered as (y, z, x).
template <typename GBuilder, typename HBuilder>
std::shared_ptr<const prf::Function>
  Rn(size_t n, GBuilder g_builder, HBuilder h_builder, std::string name = "") {
  Context gctx{n};
  Context hctx{n + 2};
  auto g = g_builder(gctx);
  auto h = h_builder(hctx);
  return prf::primitive_rec(std::move(g), std::move(h), std::move(name));
}

// Binary case (arity 2): recursion over the LAST argument (y).
template <typename GBuilder, typename HBuilder>
std::shared_ptr<const prf::Function>
  R1(GBuilder g_builder, HBuilder h_builder, std::string name = "") {
  return Rn(1, std::move(g_builder), std::move(h_builder), std::move(name));
}

}  // namespace dsl
