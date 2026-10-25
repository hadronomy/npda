#ifndef NPDA_PRF_FUNCTION_H_
#define NPDA_PRF_FUNCTION_H_

#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <span>
#include <string>
#include <utility>
#include <vector>

/// Builders that follow the structure of primitive recursive definitions:
///
///     // mul(x, 0)    = Z(x)
///     // mul(x, S(y)) = (add ∘ (P₃³ × P₁³))(x, y, mul(x, y))
///     const Fn<2> mul = Recursion(
///         Zero(),
///         Composition(
///             Addition(),
///             Combination(Projection<3, 3>(), Projection<1, 3>())
///         ),
///         "mul"
///     );
///
/// Each builder returns an `Fn<N, M>` for f: ℕᴺ → ℕᴹ, so the compiler rejects
/// parts that do not fit. For composition f ∘ g, this check requires f to take
/// as many arguments as g returns results. The builder also creates a
/// `Function` subclass object, which evaluates the function and supplies its
/// name.
namespace prf {

class Trace;

/// A natural number stored in 64 bits, so the successor of the largest value
/// is an error.
using Natural = std::uint64_t;

using Tuple = std::vector<Natural>;

struct Error {
  std::string message;
};

/// The notation for indices and operators in names: `S ∘ P₃³` or `S o P_3^3`.
enum class Notation { kUnicode, kAscii };

/// One equation of a definition: `pow(x, 0) = (S ∘ Z)(x)`.
struct Equation {
  std::string left;
  std::string right;
};

/// A primitive recursive function with runtime arity and output count:
/// f: ℕⁿ → ℕᵐ with n = `Arity()` and m = `Outputs()`.
///
/// Functions never change after construction, so larger functions can share
/// them. Each function holds only the functions from which it was built, so
/// no function is a part of itself.
class Function {
 public:
  virtual ~Function() = default;

  Function(const Function&) = delete;
  Function& operator=(const Function&) = delete;

  [[nodiscard]] std::size_t Arity() const { return arity_; }

  [[nodiscard]] std::size_t Outputs() const { return outputs_; }

  /// Returns the name in the notation of the course: `S`, `P₃³`,
  /// `add ∘ (P₃³ × P₁³)`.
  [[nodiscard]] virtual std::string Name(Notation notation) const = 0;

  /// Returns the name for use in a call or as an operand of ∘ and ×, with
  /// parentheses around compositions and combinations, as in `(S ∘ Z)`.
  /// Other names remain unchanged.
  [[nodiscard]] std::string Operand(Notation notation) const;

  /// Returns the equations for this function and its named parts, outermost
  /// first, with each name once. For `pow`, these are its limit and recursion
  /// equations, followed by those of `mul` and `add`.
  [[nodiscard]] std::vector<Equation> Definition(Notation notation) const;

 protected:
  Function(std::size_t arity, std::size_t outputs)
      : arity_(arity), outputs_(outputs) {}

  /// Applies `function` to `arguments`, writes the results to `results`, and
  /// records this call and all calls it makes in `trace`.
  ///
  /// `arguments` must hold `Arity()` values and `results` `Outputs()` values.
  /// `Fn::Evaluate` and the parts of a function always pass these sizes, and
  /// any other size stops the program.
  ///
  /// # Errors
  ///
  /// An `Error` if a value is too large for 64 bits, or if the evaluation
  /// reaches the call limit of `trace`.
  [[nodiscard]] static std::expected<void, Error> Apply(
      const Function& function,
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace& trace
  );

 private:
  template <std::size_t N, std::size_t M>
  friend class Fn;

  /// Whether the name is an expression of ∘ or ×, which needs parentheses as
  /// an operand.
  [[nodiscard]] virtual bool IsCompound() const { return false; }

  [[nodiscard]] virtual std::vector<const Function*> Parts() const {
    return {};
  }

  /// The equations that define this function under its own name, or none if
  /// its name is an expression of its parts.
  [[nodiscard]] virtual std::vector<Equation> OwnEquations(Notation) const {
    return {};
  }

  virtual std::expected<void, Error> DoApply(
      std::span<const Natural> arguments,
      std::span<Natural> results,
      Trace& trace
  ) const = 0;

  std::size_t arity_;
  std::size_t outputs_;
};

template <std::size_t N, std::size_t M>
class Fn;

namespace detail {

using FunctionPtr = std::shared_ptr<const Function>;

// Builders check arities at compile time, so these object factories need no
// runtime arity checks.
[[nodiscard]] FunctionPtr NewZero();
[[nodiscard]] FunctionPtr NewSuccessor();
[[nodiscard]] FunctionPtr NewProjection(std::size_t index, std::size_t arity);
[[nodiscard]] FunctionPtr NewCombination(FunctionPtr first, FunctionPtr second);
[[nodiscard]] FunctionPtr NewComposition(FunctionPtr outer, FunctionPtr inner);
[[nodiscard]] FunctionPtr NewRecursion(
    FunctionPtr base,
    FunctionPtr step,
    std::string name
);

/// Keeps construction inside `detail`, so callers outside it cannot create
/// an `Fn` whose type disagrees with its function object.
struct Access {
  template <std::size_t N, std::size_t M>
  [[nodiscard]] static Fn<N, M> Wrap(FunctionPtr function) {
    return Fn<N, M>(std::move(function));
  }

  template <std::size_t N, std::size_t M>
  [[nodiscard]] static const FunctionPtr& Unwrap(const Fn<N, M>& function) {
    return function.function_;
  }
};

}  // namespace detail

/// A primitive recursive function f: ℕᴺ → ℕᴹ.
///
/// Copies share the same function object.
template <std::size_t N, std::size_t M = 1>
class Fn {
 public:
  /// Returns the underlying function for traces and names, without the
  /// compile-time arity and output count.
  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const Function& function() const { return *function_; }

  /// Applies the function to `arguments` and records each call in `trace`.
  ///
  /// # Errors
  ///
  /// An `Error` if a value is too large for 64 bits, or if the evaluation
  /// reaches the call limit of `trace`.
  [[nodiscard]] std::expected<std::array<Natural, M>, Error> Evaluate(
      const std::array<Natural, N>& arguments,
      Trace& trace
  ) const {
    std::array<Natural, M> results{};
    if (auto applied = Function::Apply(*function_, arguments, results, trace);
        !applied) {
      return std::unexpected(std::move(applied).error());
    }
    return results;
  }

 private:
  friend struct detail::Access;

  explicit Fn(detail::FunctionPtr function) : function_(std::move(function)) {}

  detail::FunctionPtr function_;
};

// Template parameter names explain mismatched parts in compiler errors:
// "deduced conflicting values for parameter 'Arity' (1 vs. 2)".

/// Z, which gives 0 for x.
[[nodiscard]] inline Fn<1> Zero() {
  return detail::Access::Wrap<1, 1>(detail::NewZero());
}

/// S, which gives x + 1 for x.
[[nodiscard]] inline Fn<1> Successor() {
  return detail::Access::Wrap<1, 1>(detail::NewSuccessor());
}

/// Pᵢᴺ, which returns argument I, where the first argument has the number 1.
template <std::size_t I, std::size_t N>
[[nodiscard]] Fn<N> Projection()
  requires(1 <= I && I <= N)
{
  return detail::Access::Wrap<N, 1>(detail::NewProjection(I, N));
}

/// f × g, which gives the outputs of f and then the outputs of g, for the same
/// arguments.
template <std::size_t Arity, std::size_t A, std::size_t B>
[[nodiscard]] Fn<Arity, A + B> Combination(
    const Fn<Arity, A>& first,
    const Fn<Arity, B>& second
) {
  return detail::Access::Wrap<Arity, A + B>(detail::NewCombination(
      detail::Access::Unwrap(first),
      detail::Access::Unwrap(second)
  ));
}

/// f ∘ g, which applies f to the outputs of g.
template <std::size_t Values, std::size_t M, std::size_t N>
[[nodiscard]] Fn<N, M> Composition(
    const Fn<Values, M>& outer,
    const Fn<N, Values>& inner
) {
  return detail::Access::Wrap<N, M>(detail::NewComposition(
      detail::Access::Unwrap(outer),
      detail::Access::Unwrap(inner)
  ));
}

/// The function f that primitive recursion on the last argument defines:
///
///     f(X, 0)    = g(X)
///     f(X, S(y)) = h(X, y, f(X, y))
///
/// Here, g = `base` and h = `step`, so h takes two more arguments than g.
/// The optional `name`, for example `add`, replaces `R(g, h)`.
template <std::size_t BaseArity, std::size_t StepArity>
[[nodiscard]] Fn<BaseArity + 1> Recursion(
    const Fn<BaseArity>& base,
    const Fn<StepArity>& step,
    std::string name = ""
)
  requires(StepArity == BaseArity + 2)
{
  return detail::Access::Wrap<BaseArity + 1, 1>(detail::NewRecursion(
      detail::Access::Unwrap(base),
      detail::Access::Unwrap(step),
      std::move(name)
  ));
}

/// add(x, y) = x + y, by recursion from P₁¹ and S.
[[nodiscard]] Fn<2> Addition();

/// mul(x, y) = x · y, by recursion from Z and add.
[[nodiscard]] Fn<2> Multiplication();

/// pow(x, y) = xʸ, by recursion from S ∘ Z and mul, so pow(0, 0) = 1.
[[nodiscard]] Fn<2> Power();

}  // namespace prf

#endif  // NPDA_PRF_FUNCTION_H_
