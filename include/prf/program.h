#ifndef NPDA_PRF_PROGRAM_H_
#define NPDA_PRF_PROGRAM_H_

#include <array>
#include <cstddef>
#include <expected>
#include <memory>
#include <span>
#include <utility>

#include "prf/function.h"

namespace prf {

namespace detail {

struct Code;

/// Compiles `function` into one node for each of its objects.
[[nodiscard]] std::shared_ptr<const Code> Compile(const Function& function);

/// Applies the function represented by `code` with the call order, trace,
/// results, and errors of `Function::Apply`.
[[nodiscard]] std::expected<void, Error> Run(
    const Code& code,
    std::span<const Natural> arguments,
    std::span<Natural> results,
    Trace& trace
);

}  // namespace detail

/// A function compiled for fast evaluation.
///
/// A program makes the same calls as `Fn::Evaluate`, in the same order, and
/// records the same trace. Both evaluators thus produce the same results,
/// call counts, trees, and errors.
///
/// Each call that the trace does not keep in a tree costs one switch over a
/// flat array of nodes instead of a virtual call, size checks, and a trace
/// scope, which makes an evaluation several times faster.
template <std::size_t N, std::size_t M = 1>
class Program {
 public:
  explicit Program(const Fn<N, M>& function)
      : function_(detail::Access::Unwrap(function)),
        code_(detail::Compile(*function_)) {}

  /// Returns the function that the program evaluates, for traces and names.
  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] const Function& function() const { return *function_; }

  /// Applies the function to `arguments` and records each call in `trace`,
  /// as `Fn::Evaluate` does.
  ///
  /// # Errors
  ///
  /// Returns the same errors as `Fn::Evaluate`.
  [[nodiscard]] std::expected<std::array<Natural, M>, Error> Evaluate(
      const std::array<Natural, N>& arguments,
      Trace& trace
  ) const {
    std::array<Natural, M> results{};
    if (auto applied = detail::Run(*code_, arguments, results, trace);
        !applied) {
      return std::unexpected(std::move(applied).error());
    }
    return results;
  }

 private:
  // The nodes point to the function objects, so the program keeps them alive.
  detail::FunctionPtr function_;
  std::shared_ptr<const detail::Code> code_;
};

}  // namespace prf

#endif  // NPDA_PRF_PROGRAM_H_
