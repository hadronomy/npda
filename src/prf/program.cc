#include "prf/program.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <limits>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "prf/code.h"
#include "prf/function.h"
#include "prf/trace.h"

namespace prf::detail {

/// Evaluates compiled code and records its calls in `trace` for `kMode`.
///
/// The runner uses `Trace::Enter` only for calls that the tree keeps. For
/// other calls, it updates local counters and copies them to the trace when
/// evaluation ends, whether it succeeds or fails.
///
/// On an error, the runner stores it in `error_` and returns false, so each
/// call passes back one bool instead of an `std::expected` that carries a
/// string.
template <TraceMode kMode>
class Runner {
 public:
  Runner(const Code& code, Trace& trace)
      : nodes_(code.nodes),
        trace_(trace),
        calls_(trace.calls_),
        max_calls_(trace.options_.max_calls) {
    if constexpr (kMode == TraceMode::kCounts) counts_.assign(nodes_.size(), 0);
  }

  Runner(const Runner&) = delete;
  Runner& operator=(const Runner&) = delete;

  ~Runner() {
    trace_.calls_ = calls_;
    if constexpr (kMode == TraceMode::kCounts) {
      for (std::size_t index = 0; index < counts_.size(); ++index) {
        if (counts_[index] != 0) {
          trace_.counts_[nodes_[index].function] += counts_[index];
        }
      }
    }
  }

  std::expected<void, Error> Evaluate(
      std::uint32_t root,
      std::span<const Natural> arguments,
      std::span<Natural> results
  ) {
    const Node& node = nodes_[root];
    if (arguments.size() != node.arity || results.size() != node.outputs) {
      std::abort();
    }
    if (Call(root, arguments.data(), results.data())) return {};
    return std::unexpected(*std::move(error_));
  }

 private:
  bool Call(std::uint32_t id, const Natural* arguments, Natural* results) {
    const Node& node = nodes_[id];
    if constexpr (kMode == TraceMode::kTree) {
      if (trace_.kept_ < trace_.options_.tree_limit) {
        return Record(node, arguments, results);
      }
    }
    if (calls_ == max_calls_) return Fail(trace_.LimitError());
    ++calls_;
    if constexpr (kMode == TraceMode::kCounts) ++counts_[id];
    return Operate(node, arguments, results);
  }

  bool Record(const Node& node, const Natural* arguments, Natural* results) {
    trace_.calls_ = calls_;
    auto scope = trace_.Enter(*node.function, std::span(arguments, node.arity));
    calls_ = trace_.calls_;
    if (!scope) return Fail(std::move(scope).error());
    if (!Operate(node, arguments, results)) return false;
    scope->SetResults(std::span(results, node.outputs));
    return true;
  }

  bool Operate(const Node& node, const Natural* arguments, Natural* results) {
    switch (node.operation) {
      case Operation::kZero:
        results[0] = 0;
        return true;
      case Operation::kSuccessor:
        if (arguments[0] == std::numeric_limits<Natural>::max()) {
          return Fail(SuccessorOverflow(arguments[0]));
        }
        results[0] = arguments[0] + 1;
        return true;
      case Operation::kProjection:
        results[0] = arguments[node.first];
        return true;
      case Operation::kCombination:
        return Call(node.first, arguments, results) &&
               Call(
                   node.second,
                   arguments,
                   results + nodes_[node.first].outputs
               );
      case Operation::kComposition: {
        const Scratch values(nodes_[node.second].outputs);
        return Call(node.second, arguments, values.values().data()) &&
               Call(node.first, values.values().data(), results);
      }
      case Operation::kRecursion:
        return Recurse(node, arguments, results);
    }
    std::abort();
  }

  /// Computes f(X, 0) up to f(X, y) in a loop, as the object evaluator does.
  bool Recurse(const Node& node, const Natural* arguments, Natural* results) {
    const std::uint32_t fixed = node.arity - 1;
    const Scratch scratch(node.arity + 1);
    Natural* step_arguments = scratch.values().data();
    std::copy_n(arguments, fixed, step_arguments);
    Natural& counter = step_arguments[fixed];
    Natural& value = step_arguments[node.arity];
    if (!Call(node.first, arguments, &value)) return false;
    for (counter = 0; counter < arguments[fixed]; ++counter) {
      // The step still needs the previous value, so its result must use
      // separate storage.
      Natural next = 0;
      if (!Call(node.second, step_arguments, &next)) return false;
      value = next;
    }
    results[0] = value;
    return true;
  }

  bool Fail(Error error) {
    error_ = std::move(error);
    return false;
  }

  const std::vector<Node>& nodes_;
  Trace& trace_;
  Natural calls_;
  Natural max_calls_;
  std::vector<Natural> counts_;
  std::optional<Error> error_;
};

std::shared_ptr<const Code> Compile(const Function& function) {
  Assembler assembler;
  const auto root = assembler.Add(function);
  return std::make_shared<const Code>(std::move(assembler).Finish(root));
}

std::expected<void, Error> Run(
    const Code& code,
    std::span<const Natural> arguments,
    std::span<Natural> results,
    Trace& trace
) {
  switch (trace.mode()) {
    case TraceMode::kOff:
      return Runner<TraceMode::kOff>(code, trace)
          .Evaluate(code.root, arguments, results);
    case TraceMode::kCounts:
      return Runner<TraceMode::kCounts>(code, trace)
          .Evaluate(code.root, arguments, results);
    case TraceMode::kTree:
      return Runner<TraceMode::kTree>(code, trace)
          .Evaluate(code.root, arguments, results);
  }
  std::abort();
}

}  // namespace prf::detail
