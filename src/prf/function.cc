#include "prf/function.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <limits>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "prf/trace.h"

namespace prf {

std::expected<std::uint64_t, Error> Function::Evaluate(
    std::span<const std::uint64_t> arguments, Trace& trace) const {
  if (arguments.size() != arity()) {
    return std::unexpected(
        Error{std::format("Arity mismatch for '{}': expected {}, got {}",
                          name(), arity(), arguments.size())});
  }
  auto scope = trace.Enter(name(), arguments);
  auto result = DoEvaluate(arguments, trace);
  if (result) scope.SetResult(*result);
  return result;
}

namespace {

class Zero final : public Function {
 public:
  Zero(std::size_t arity, std::string name)
      : arity_(arity),
        name_(name.empty() ? std::format("Z^{}", arity) : std::move(name)) {}

  std::size_t arity() const override { return arity_; }

  std::string_view name() const override { return name_; }

 private:
  std::expected<std::uint64_t, Error> DoEvaluate(std::span<const std::uint64_t>,
                                                 Trace&) const override {
    return 0;
  }

  std::size_t arity_;
  std::string name_;
};

class Successor final : public Function {
 public:
  explicit Successor(std::string name) : name_(std::move(name)) {}

  std::size_t arity() const override { return 1; }

  std::string_view name() const override { return name_; }

 private:
  std::expected<std::uint64_t, Error> DoEvaluate(
      std::span<const std::uint64_t> arguments, Trace&) const override {
    if (arguments.front() == std::numeric_limits<std::uint64_t>::max()) {
      return std::unexpected(Error{"Successor overflow on std::uint64_t"});
    }
    return arguments.front() + 1;
  }

  std::string name_;
};

class Projection final : public Function {
 public:
  Projection(std::size_t index, std::size_t arity, std::string name)
      : index_(index),
        arity_(arity),
        name_(name.empty() ? std::format("P_{}^{}", index, arity)
                           : std::move(name)) {}

  std::size_t arity() const override { return arity_; }

  std::string_view name() const override { return name_; }

 private:
  std::expected<std::uint64_t, Error> DoEvaluate(
      std::span<const std::uint64_t> arguments, Trace&) const override {
    return arguments[index_ - 1];
  }

  std::size_t index_;
  std::size_t arity_;
  std::string name_;
};

class Composition final : public Function {
 public:
  Composition(FunctionPtr outer, std::vector<FunctionPtr> inner,
              std::string name)
      : outer_(std::move(outer)),
        inner_(std::move(inner)),
        name_(std::move(name)) {
    if (name_.empty()) {
      name_ = std::format("C({}∘[", outer_->name());
      for (std::size_t i = 0; i < inner_.size(); ++i) {
        if (i != 0) name_ += ", ";
        name_ += inner_[i]->name();
      }
      name_ += "])";
    }
  }

  std::size_t arity() const override {
    return inner_.empty() ? 0 : inner_.front()->arity();
  }

  std::string_view name() const override { return name_; }

 private:
  std::expected<std::uint64_t, Error> DoEvaluate(
      std::span<const std::uint64_t> arguments, Trace& trace) const override {
    std::vector<std::uint64_t> values;
    values.reserve(inner_.size());
    for (const auto& function : inner_) {
      auto value = function->Evaluate(arguments, trace);
      if (!value) return std::unexpected(value.error());
      values.push_back(*value);
    }
    return outer_->Evaluate(values, trace);
  }

  FunctionPtr outer_;
  std::vector<FunctionPtr> inner_;
  std::string name_;
};

class PrimitiveRecursion final : public Function {
 public:
  PrimitiveRecursion(FunctionPtr base, FunctionPtr step, std::string name)
      : base_(std::move(base)),
        step_(std::move(step)),
        name_(name.empty()
                  ? std::format("R_last({}, {})", base_->name(), step_->name())
                  : std::move(name)) {}

  std::size_t arity() const override { return base_->arity() + 1; }

  std::string_view name() const override { return name_; }

 private:
  std::expected<std::uint64_t, Error> DoEvaluate(
      std::span<const std::uint64_t> arguments, Trace& trace) const override {
    const auto fixed_arguments = arguments.first(arguments.size() - 1);
    auto result = base_->Evaluate(fixed_arguments, trace);
    if (!result) return std::unexpected(result.error());
    std::vector<std::uint64_t> step_arguments(arguments.size() + 1);
    std::ranges::copy(fixed_arguments, step_arguments.begin() + 2);
    for (std::uint64_t counter = 0; counter < arguments.back(); ++counter) {
      step_arguments[0] = counter;
      step_arguments[1] = *result;
      result = step_->Evaluate(step_arguments, trace);
      if (!result) return std::unexpected(result.error());
    }
    return result;
  }

  FunctionPtr base_;
  FunctionPtr step_;
  std::string name_;
};

}  // namespace

FunctionPtr MakeZero(std::size_t arity, std::string name) {
  return std::make_shared<Zero>(arity, std::move(name));
}

FunctionPtr MakeSuccessor(std::string name) {
  return std::make_shared<Successor>(std::move(name));
}

std::expected<FunctionPtr, Error> CreateProjection(std::size_t index,
                                                   std::size_t arity,
                                                   std::string name) {
  if (index == 0 || index > arity)
    return std::unexpected(Error{"Projection index out of range"});
  return std::make_shared<Projection>(index, arity, std::move(name));
}

std::expected<FunctionPtr, Error> CreateComposition(
    FunctionPtr outer, std::vector<FunctionPtr> inner, std::string name) {
  if (!outer)
    return std::unexpected(Error{"Composition: outer function is null"});
  if (std::ranges::any_of(
          inner, [](const FunctionPtr& function) { return !function; })) {
    return std::unexpected(Error{"Composition: null inner function"});
  }
  if (!inner.empty() &&
      std::ranges::any_of(inner, [&](const FunctionPtr& function) {
        return function->arity() != inner.front()->arity();
      })) {
    return std::unexpected(Error{"Composition: mismatched inner arities"});
  }
  if (outer->arity() != inner.size())
    return std::unexpected(Error{"Composition: outer arity mismatch"});
  return std::make_shared<Composition>(std::move(outer), std::move(inner),
                                       std::move(name));
}

std::expected<FunctionPtr, Error> CreateRecursion(FunctionPtr base,
                                                  FunctionPtr step,
                                                  std::string name) {
  if (!base || !step)
    return std::unexpected(Error{"PrimitiveRecursion: null function"});
  if (step->arity() != base->arity() + 2)
    return std::unexpected(Error{"PrimitiveRecursion: arity mismatch"});
  return std::make_shared<PrimitiveRecursion>(std::move(base), std::move(step),
                                              std::move(name));
}

std::expected<FunctionPtr, Error> CreatePowerFunction() {
  auto identity = CreateProjection(1, 1, "id");
  auto previous = CreateProjection(2, 3);
  auto fixed = CreateProjection(3, 3);
  if (!identity) return std::unexpected(identity.error());
  if (!previous) return std::unexpected(previous.error());
  if (!fixed) return std::unexpected(fixed.error());
  auto successor = MakeSuccessor();
  auto add_step = CreateComposition(successor, {*previous});
  if (!add_step) return std::unexpected(add_step.error());
  auto add = CreateRecursion(*identity, *add_step, "add");
  if (!add) return std::unexpected(add.error());
  auto multiply_step = CreateComposition(*add, {*previous, *fixed});
  if (!multiply_step) return std::unexpected(multiply_step.error());
  auto multiply = CreateRecursion(MakeZero(1), *multiply_step, "mul");
  if (!multiply) return std::unexpected(multiply.error());
  auto one = CreateComposition(successor, {MakeZero(1)});
  if (!one) return std::unexpected(one.error());
  auto power_step = CreateComposition(*multiply, {*previous, *fixed});
  if (!power_step) return std::unexpected(power_step.error());
  return CreateRecursion(*one, *power_step, "pow");
}

}  // namespace prf
