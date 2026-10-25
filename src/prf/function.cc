#include "prf/function.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdlib>
#include <expected>
#include <format>
#include <limits>
#include <memory>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "prf/trace.h"

namespace prf {

namespace {

/// Writes `number` with Unicode subscript or superscript digits: `₁₂`, `¹²`.
std::string Digits(std::size_t number, bool superscript) {
  static constexpr std::array<std::string_view, 10>
      kSubscripts{"₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"};
  static constexpr std::array<std::string_view, 10>
      kSuperscripts{"⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹"};
  const auto& digits = superscript ? kSuperscripts : kSubscripts;
  std::string text;
  for (const char digit : std::to_string(number)) {
    text += digits[static_cast<std::size_t>(digit - '0')];
  }
  return text;
}

std::string Subscript(std::size_t number, Notation notation) {
  return notation == Notation::kUnicode ? Digits(number, false)
                                        : std::format("_{}", number);
}

std::string Superscript(std::size_t number, Notation notation) {
  return notation == Notation::kUnicode ? Digits(number, true)
                                        : std::format("^{}", number);
}

/// Names `count` arguments for an equation: `x` alone, or `x₁, x₂, x₃`.
std::string Variables(std::size_t count, Notation notation) {
  if (count == 1) return "x";
  std::string text;
  for (std::size_t index = 1; index <= count; ++index) {
    if (index != 1) text += ", ";
    text += "x" + Subscript(index, notation);
  }
  return text;
}

class ZeroFunction final : public Function {
 public:
  ZeroFunction() : Function(1, 1) {}

  std::string Name(Notation) const override { return "Z"; }

 private:
  std::expected<void, Error> DoApply(
      std::span<const Natural>,
      std::span<Natural> results,
      Trace&
  ) const override {
    results[0] = 0;
    return {};
  }
};

class SuccessorFunction final : public Function {
 public:
  SuccessorFunction() : Function(1, 1) {}

  std::string Name(Notation) const override { return "S"; }

 private:
  std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace&
  ) const override {
    if (arguments[0] == std::numeric_limits<Natural>::max()) {
      return std::unexpected(
          Error{std::format("S({}) does not fit in 64 bits", arguments[0])}
      );
    }
    results[0] = arguments[0] + 1;
    return {};
  }
};

class ProjectionFunction final : public Function {
 public:
  ProjectionFunction(std::size_t index, std::size_t arity)
      : Function(arity, 1), index_(index) {}

  std::string Name(Notation notation) const override {
    return "P" + Subscript(index_, notation) + Superscript(Arity(), notation);
  }

 private:
  std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace&
  ) const override {
    results[0] = arguments[index_ - 1];
    return {};
  }

  std::size_t index_;
};

class CombinationFunction final : public Function {
 public:
  CombinationFunction(detail::FunctionPtr first, detail::FunctionPtr second)
      : Function(first->Arity(), first->Outputs() + second->Outputs()),
        first_(std::move(first)),
        second_(std::move(second)) {}

  std::string Name(Notation notation) const override {
    return std::format(
        "{} {} {}",
        first_->Operand(notation),
        notation == Notation::kUnicode ? "×" : "x",
        second_->Operand(notation)
    );
  }

 private:
  bool IsCompound() const override { return true; }

  std::vector<const Function*> Parts() const override {
    return {first_.get(), second_.get()};
  }

  std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace& trace
  ) const override {
    const auto split = first_->Outputs();
    if (auto first = Apply(*first_, arguments, results.first(split), trace);
        !first) {
      return first;
    }
    return Apply(*second_, arguments, results.subspan(split), trace);
  }

  detail::FunctionPtr first_;
  detail::FunctionPtr second_;
};

class CompositionFunction final : public Function {
 public:
  CompositionFunction(detail::FunctionPtr outer, detail::FunctionPtr inner)
      : Function(inner->Arity(), outer->Outputs()),
        outer_(std::move(outer)),
        inner_(std::move(inner)) {}

  std::string Name(Notation notation) const override {
    return std::format(
        "{} {} {}",
        outer_->Operand(notation),
        notation == Notation::kUnicode ? "∘" : "o",
        inner_->Operand(notation)
    );
  }

 private:
  bool IsCompound() const override { return true; }

  std::vector<const Function*> Parts() const override {
    return {outer_.get(), inner_.get()};
  }

  std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace& trace
  ) const override {
    // Most compositions pass one or two values, which fit on the stack.
    // Keeping them there avoids a heap buffer for each call, which doubles
    // the time of an evaluation.
    std::array<Natural, 4> stack_values{};
    Tuple heap_values;
    std::span<Natural> values(stack_values.data(), inner_->Outputs());
    if (inner_->Outputs() > stack_values.size()) {
      heap_values.resize(inner_->Outputs());
      values = heap_values;
    }
    if (auto inner = Apply(*inner_, arguments, values, trace); !inner) {
      return inner;
    }
    return Apply(*outer_, values, results, trace);
  }

  detail::FunctionPtr outer_;
  detail::FunctionPtr inner_;
};

class RecursionFunction final : public Function {
 public:
  RecursionFunction(
      detail::FunctionPtr base,
      detail::FunctionPtr step,
      std::string name
  )
      : Function(base->Arity() + 1, 1),
        base_(std::move(base)),
        step_(std::move(step)),
        name_(std::move(name)) {}

  std::string Name(Notation notation) const override {
    if (!name_.empty()) return name_;
    return std::format(
        "R({}, {})",
        base_->Name(notation),
        step_->Name(notation)
    );
  }

 private:
  std::vector<const Function*> Parts() const override {
    return {base_.get(), step_.get()};
  }

  std::vector<Equation> OwnEquations(Notation notation) const override {
    if (name_.empty()) return {};
    // In f(X, 0) = g(X) and f(X, S(y)) = h(X, y, f(X, y)), X is absent when
    // the recursion has no parameters.
    const auto fixed = Variables(Arity() - 1, notation);
    const auto with = [&](std::string_view last) {
      return fixed.empty() ? std::string(last)
                           : std::format("{}, {}", fixed, last);
    };
    return {
        {.left = std::format("{}({})", name_, with("0")),
         .right = std::format("{}({})", base_->Operand(notation), fixed)},
        {.left = std::format("{}({})", name_, with("S(y)")),
         .right = std::format(
             "{}({})",
             step_->Operand(notation),
             with(std::format("y, {}({})", name_, with("y")))
         )},
    };
  }

  /// Computes f(X, 0), f(X, 1), and so on up to f(X, y) in a loop, with each
  /// value in h's last argument. This arrangement lets all steps share one
  /// buffer for (X, y, f(X, y)).
  std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace& trace
  ) const override {
    const auto fixed = arguments.first(arguments.size() - 1);
    Tuple step_arguments(arguments.size() + 1);
    std::ranges::copy(fixed, step_arguments.begin());
    auto& counter = step_arguments[fixed.size()];
    auto& value = step_arguments.back();
    if (auto base = Apply(*base_, fixed, std::span(&value, 1), trace); !base) {
      return base;
    }
    for (counter = 0; counter < arguments.back(); ++counter) {
      // The step reads the old value, so it writes the new one elsewhere.
      Natural next = 0;
      if (auto step = Apply(*step_, step_arguments, std::span(&next, 1), trace);
          !step) {
        return step;
      }
      value = next;
    }
    results[0] = value;
    return {};
  }

  detail::FunctionPtr base_;
  detail::FunctionPtr step_;
  std::string name_;
};

}  // namespace

std::string Function::Operand(Notation notation) const {
  return IsCompound() ? std::format("({})", Name(notation)) : Name(notation);
}

std::vector<Equation> Function::Definition(Notation notation) const {
  std::set<std::string> seen;
  std::vector<Equation> equations;
  // Once a name is in `seen`, its equations and those of its parts are
  // already included, so another occurrence needs no traversal.
  const auto collect = [&](this const auto& self, const Function& function) {
    auto own = function.OwnEquations(notation);
    if (!own.empty() && !seen.insert(function.Name(notation)).second) return;
    equations.insert(equations.end(), own.begin(), own.end());
    for (const Function* part : function.Parts()) self(*part);
  };
  collect(*this);
  return equations;
}

std::expected<void, Error> Function::Apply(
    const Function& function,
    std::span<const Natural> arguments,
    std::span<Natural> results,
    Trace& trace
) {
  if (arguments.size() != function.Arity() ||
      results.size() != function.Outputs()) {
    std::abort();
  }
  auto scope = trace.Enter(function, arguments);
  if (!scope) return std::unexpected(std::move(scope).error());
  auto applied = function.DoApply(arguments, results, trace);
  if (applied) scope->SetResults(results);
  return applied;
}

namespace detail {

FunctionPtr NewZero() { return std::make_shared<ZeroFunction>(); }

FunctionPtr NewSuccessor() { return std::make_shared<SuccessorFunction>(); }

FunctionPtr NewProjection(std::size_t index, std::size_t arity) {
  return std::make_shared<ProjectionFunction>(index, arity);
}

FunctionPtr NewCombination(FunctionPtr first, FunctionPtr second) {
  return std::make_shared<CombinationFunction>(
      std::move(first),
      std::move(second)
  );
}

FunctionPtr NewComposition(FunctionPtr outer, FunctionPtr inner) {
  return std::make_shared<CompositionFunction>(
      std::move(outer),
      std::move(inner)
  );
}

FunctionPtr NewRecursion(FunctionPtr base, FunctionPtr step, std::string name) {
  return std::make_shared<RecursionFunction>(
      std::move(base),
      std::move(step),
      std::move(name)
  );
}

}  // namespace detail

// The slides fix the step's form, h(X, y, f(X, y)), but leave its argument
// order inside other calls open. Because each recursion runs on its last
// argument, add(mul(x, y), x) and mul(pow(x, y), x) put the previous value
// first. Each step of pow thus runs mul for x steps. Reversing both calls to
// add(x, mul(x, y)) and mul(x, pow(x, y)) instead runs both inner recursions
// on previous values, so the calls of pow grow with x²ʸ instead of xʸ.

Fn<2> Addition() {
  // add(x, 0)    = P₁¹(x)
  // add(x, S(y)) = (S ∘ P₃³)(x, y, add(x, y))
  return Recursion(
      Projection<1, 1>(),
      Composition(Successor(), Projection<3, 3>()),
      "add"
  );
}

Fn<2> Multiplication() {
  // mul(x, 0)    = Z(x)
  // mul(x, S(y)) = (add ∘ (P₃³ × P₁³))(x, y, mul(x, y))
  return Recursion(
      Zero(),
      Composition(
          Addition(),
          Combination(Projection<3, 3>(), Projection<1, 3>())
      ),
      "mul"
  );
}

Fn<2> Power() {
  // pow(x, 0)    = (S ∘ Z)(x)
  // pow(x, S(y)) = (mul ∘ (P₃³ × P₁³))(x, y, pow(x, y))
  return Recursion(
      Composition(Successor(), Zero()),
      Composition(
          Multiplication(),
          Combination(Projection<3, 3>(), Projection<1, 3>())
      ),
      "pow"
  );
}

}  // namespace prf
