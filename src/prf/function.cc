#include "prf/function.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <format>
#include <limits>
#include <memory>
#include <optional>
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "prf/code.h"
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

Expression Plain(std::string text) { return {{Role::kPlain, std::move(text)}}; }

Expression operator+(Expression left, const Expression& right) {
  left.insert(left.end(), right.begin(), right.end());
  return left;
}

/// Returns `left ∘ right` or `left × right` with the operator for `notation`.
Expression Binary(
    const Function& left,
    std::string_view unicode,
    std::string_view ascii,
    const Function& right,
    Notation notation
) {
  return left.Operand(notation) + Plain(" ") +
         Expression{
             {Role::kOperator,
              std::string(notation == Notation::kUnicode ? unicode : ascii)}
         } +
         Plain(" ") + right.Operand(notation);
}

/// Returns `function(arguments)`.
Expression Call(Expression function, const Expression& arguments) {
  return std::move(function) + Plain("(") + arguments + Plain(")");
}

class ZeroFunction final : public Function {
 public:
  ZeroFunction() : Function(1, 1) {}

  Expression Name(Notation) const override { return {{Role::kBasic, "Z"}}; }

 private:
  std::uint32_t Lower(detail::Assembler& assembler) const override {
    return assembler.Emit(
        {.function = this,
         .operation = detail::Operation::kZero,
         .arity = 1,
         .outputs = 1}
    );
  }

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

  Expression Name(Notation) const override { return {{Role::kBasic, "S"}}; }

 private:
  std::uint32_t Lower(detail::Assembler& assembler) const override {
    return assembler.Emit(
        {.function = this,
         .operation = detail::Operation::kSuccessor,
         .arity = 1,
         .outputs = 1}
    );
  }

  std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace&
  ) const override {
    if (arguments[0] == std::numeric_limits<Natural>::max()) {
      return std::unexpected(detail::SuccessorOverflow(arguments[0]));
    }
    results[0] = arguments[0] + 1;
    return {};
  }
};

class ProjectionFunction final : public Function {
 public:
  ProjectionFunction(std::size_t index, std::size_t arity)
      : Function(arity, 1), index_(index) {}

  Expression Name(Notation notation) const override {
    return {
        {Role::kProjection,
         "P" + Subscript(index_, notation) + Superscript(Arity(), notation)}
    };
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

  std::uint32_t Lower(detail::Assembler& assembler) const override {
    return assembler.Emit(
        {.function = this,
         .operation = detail::Operation::kProjection,
         .arity = static_cast<std::uint32_t>(Arity()),
         .outputs = 1,
         .first = static_cast<std::uint32_t>(index_ - 1)}
    );
  }

  std::size_t index_;
};

class CombinationFunction final : public Function {
 public:
  CombinationFunction(detail::FunctionPtr first, detail::FunctionPtr second)
      : Function(first->Arity(), first->Outputs() + second->Outputs()),
        first_(std::move(first)),
        second_(std::move(second)) {}

  Expression Name(Notation notation) const override {
    return Binary(*first_, "×", "x", *second_, notation);
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

  std::uint32_t Lower(detail::Assembler& assembler) const override {
    const auto first = assembler.Add(*first_);
    const auto second = assembler.Add(*second_);
    return assembler.Emit(
        {.function = this,
         .operation = detail::Operation::kCombination,
         .arity = static_cast<std::uint32_t>(Arity()),
         .outputs = static_cast<std::uint32_t>(Outputs()),
         .first = first,
         .second = second}
    );
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

  Expression Name(Notation notation) const override {
    return Binary(*outer_, "∘", "o", *inner_, notation);
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
    const detail::Scratch values(inner_->Outputs());
    if (auto inner = Apply(*inner_, arguments, values.values(), trace);
        !inner) {
      return inner;
    }
    return Apply(*outer_, values.values(), results, trace);
  }

  std::uint32_t Lower(detail::Assembler& assembler) const override {
    const auto outer = assembler.Add(*outer_);
    const auto inner = assembler.Add(*inner_);
    return assembler.Emit(
        {.function = this,
         .operation = detail::Operation::kComposition,
         .arity = static_cast<std::uint32_t>(Arity()),
         .outputs = static_cast<std::uint32_t>(Outputs()),
         .first = outer,
         .second = inner}
    );
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

  Expression Name(Notation notation) const override {
    if (!name_.empty()) return {{Role::kNamed, name_}};
    return Call(
        {{Role::kOperator, "R"}},
        base_->Name(notation) + Plain(", ") + step_->Name(notation)
    );
  }

 private:
  std::vector<const Function*> Parts() const override {
    return {base_.get(), step_.get()};
  }

  std::optional<Definition> OwnDefinition(Notation notation) const override {
    if (name_.empty()) return std::nullopt;
    // In f(X, 0) = g(X) and f(X, S(y)) = h(X, y, f(X, y)), X is absent when
    // the recursion has no parameters.
    const auto fixed = Variables(Arity() - 1, notation);
    const auto with = [&](Expression last) {
      return fixed.empty() ? last : Plain(fixed + ", ") + last;
    };
    const auto name = Name(notation);
    const auto successor = Call({{Role::kBasic, "S"}}, Plain("y"));
    return Definition{
        .limit =
            {.left = Call(name, with(Plain("0"))),
             .right = Call(base_->Operand(notation), Plain(fixed))},
        .recursion = {
            .left = Call(name, with(successor)),
            .right = Call(
                step_->Operand(notation),
                with(Plain("y, ") + Call(name, with(Plain("y"))))
            )
        },
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
    const detail::Scratch scratch(arguments.size() + 1);
    const auto step_arguments = scratch.values();
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

  std::uint32_t Lower(detail::Assembler& assembler) const override {
    const auto base = assembler.Add(*base_);
    const auto step = assembler.Add(*step_);
    return assembler.Emit(
        {.function = this,
         .operation = detail::Operation::kRecursion,
         .arity = static_cast<std::uint32_t>(Arity()),
         .outputs = 1,
         .first = base,
         .second = step}
    );
  }

  detail::FunctionPtr base_;
  detail::FunctionPtr step_;
  std::string name_;
};

}  // namespace

std::string Text(const Expression& expression) {
  std::string text;
  for (const auto& token : expression) text += token.text;
  return text;
}

Expression Function::Operand(Notation notation) const {
  return IsCompound() ? Plain("(") + Name(notation) + Plain(")")
                      : Name(notation);
}

std::vector<Definition> Function::Definitions(Notation notation) const {
  std::set<std::string> seen;
  std::vector<Definition> definitions;
  // Once a name is in `seen`, its definition and those of its parts are
  // already included, so another occurrence needs no traversal.
  const auto collect = [&](this const auto& self, const Function& function) {
    auto own = function.OwnDefinition(notation);
    if (own && !seen.insert(Text(function.Name(notation))).second) return;
    if (own) definitions.push_back(*std::move(own));
    for (const Function* part : function.Parts()) self(*part);
  };
  collect(*this);
  return definitions;
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

std::uint32_t Assembler::Add(const Function& function) {
  if (const auto found = index_.find(&function); found != index_.end()) {
    return found->second;
  }
  const auto index = function.Lower(*this);
  index_.emplace(&function, index);
  return index;
}

std::uint32_t Assembler::Emit(Node node) {
  nodes_.push_back(node);
  return static_cast<std::uint32_t>(nodes_.size() - 1);
}

Code Assembler::Finish(std::uint32_t root) && {
  return {.nodes = std::move(nodes_), .root = root};
}

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
