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

class Function {
 public:
  virtual ~Function() = default;
  [[nodiscard]] virtual std::size_t arity() const = 0;
  [[nodiscard]] virtual std::string_view name() const = 0;
  [[nodiscard]] std::expected<std::uint64_t, Error> Evaluate(
      std::span<const std::uint64_t> arguments, Trace& trace) const;

 private:
  virtual std::expected<std::uint64_t, Error> DoEvaluate(
      std::span<const std::uint64_t> arguments, Trace& trace) const = 0;
};

// Immutable graphs share subexpressions. A function never owns its trace.
using FunctionPtr = std::shared_ptr<const Function>;

[[nodiscard]] FunctionPtr MakeZero(std::size_t arity, std::string name = "");
[[nodiscard]] FunctionPtr MakeSuccessor(std::string name = "S");
// Projection indices are one-based.
[[nodiscard]] std::expected<FunctionPtr, Error> CreateProjection(
    std::size_t index, std::size_t arity, std::string name = "");
[[nodiscard]] std::expected<FunctionPtr, Error> CreateComposition(
    FunctionPtr outer, std::vector<FunctionPtr> inner, std::string name = "");
// Recursion uses the last argument. The step receives (counter, result, fixed
// arguments).
[[nodiscard]] std::expected<FunctionPtr, Error> CreateRecursion(
    FunctionPtr base, FunctionPtr step, std::string name = "");
[[nodiscard]] std::expected<FunctionPtr, Error> CreatePowerFunction();

}  // namespace prf

#endif  // NPDA_PRF_FUNCTION_H_
