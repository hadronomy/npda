#ifndef NPDA_PRF_FUNCTION_H_
#define NPDA_PRF_FUNCTION_H_

#include <cstddef>
#include <cstdint>
#include <expected>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "prf/trace.h"

namespace prf {

struct Error {
  std::string message;
};

/// A primitive recursive function.
///
/// Functions do not change after they are made. Thus, one function can be a
/// part of many larger functions.
class Function {
 public:
  virtual ~Function() = default;
  [[nodiscard]] virtual std::size_t Arity() const = 0;
  [[nodiscard]] virtual std::string_view Name() const = 0;

  /// Applies the function to `arguments` and records the call in `trace`.
  ///
  /// # Errors
  ///
  /// An `Error` if `arguments` does not hold `Arity()` values, or if a result
  /// is too large for 64 bits.
  [[nodiscard]] std::expected<std::uint64_t, Error> Evaluate(
      std::span<const std::uint64_t> arguments,
      Trace& trace
  ) const;

 private:
  virtual std::expected<std::uint64_t, Error> DoEvaluate(
      std::span<const std::uint64_t> arguments,
      Trace& trace
  ) const = 0;
};

using FunctionPtr = std::shared_ptr<const Function>;

/// Returns `Z^arity`, which gives 0 for all arguments.
[[nodiscard]] FunctionPtr MakeZero(std::size_t arity, std::string name = "");
/// Returns `S`, which gives `x + 1` for `x`.
[[nodiscard]] FunctionPtr MakeSuccessor(std::string name = "S");
/// Returns `P_index^arity`, which gives the argument at `index`. The first
/// argument has the index 1.
///
/// # Errors
///
/// An `Error` if `index` is 0 or more than `arity`.
[[nodiscard]] std::expected<FunctionPtr, Error> CreateProjection(
    std::size_t index,
    std::size_t arity,
    std::string name = ""
);
/// Returns `outer ∘ [inner...]`, which applies `outer` to the results of the
/// `inner` functions.
///
/// # Errors
///
/// An `Error` if the number of arguments of `outer` is not the number of
/// `inner` functions. An `Error` also if the `inner` functions have different
/// arities.
[[nodiscard]] std::expected<FunctionPtr, Error> CreateComposition(
    FunctionPtr outer,
    std::vector<FunctionPtr> inner,
    std::string name = ""
);
/// Returns primitive recursion on the last argument.
///
/// `base` gives the result for 0. For `n + 1`, `step` receives `n`, the result
/// for `n`, and the other arguments, in this order.
///
/// # Errors
///
/// An `Error` if `step` does not take two more arguments than `base`.
[[nodiscard]] std::expected<FunctionPtr, Error> CreateRecursion(
    FunctionPtr base,
    FunctionPtr step,
    std::string name = ""
);
/// Returns `pow(base, exponent)`, made from the functions above.
///
/// # Errors
///
/// An `Error` if a part of the function does not accept its parts. The parts do
/// not change, so this error does not occur.
[[nodiscard]] std::expected<FunctionPtr, Error> CreatePowerFunction();

}  // namespace prf

#endif  // NPDA_PRF_FUNCTION_H_
