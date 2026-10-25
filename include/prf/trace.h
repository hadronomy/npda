#ifndef NPDA_PRF_TRACE_H_
#define NPDA_PRF_TRACE_H_

#include <cstddef>
#include <cstdint>
#include <expected>
#include <limits>
#include <memory>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include "prf/function.h"

namespace prf {

/// The information that a trace records in addition to the total call count.
enum class TraceMode {
  kOff,
  kCounts,
  /// Records the calls as a tree, each with its arguments and its results.
  kTree,
};

namespace detail {

template <TraceMode kMode>
class Runner;

}  // namespace detail

struct TraceOptions {
  TraceMode mode = TraceMode::kCounts;
  /// The maximum number of calls in an evaluation, after which a call fails.
  std::uint64_t max_calls = std::numeric_limits<std::uint64_t>::max();
  /// The maximum number of calls that the tree keeps, in call order, so each
  /// retained call also has all its callers in the tree.
  std::size_t tree_limit = std::numeric_limits<std::size_t>::max();
};

struct TraceNode {
  const Function* function;
  std::vector<Natural> arguments;
  /// Empty if the call failed.
  std::optional<Tuple> results;
  std::vector<std::unique_ptr<TraceNode>> children;
};

/// Records the calls that evaluations make. Because the trace holds pointers to
/// the functions that it records, those functions must live as long as it.
class Trace {
 public:
  explicit Trace(TraceOptions options = {}) : options_(options) {}

  Trace(const Trace&) = delete;
  Trace& operator=(const Trace&) = delete;

  /// Returns the total call count regardless of trace mode.
  [[nodiscard]] std::uint64_t Calls() const { return calls_; }

  /// Returns the number of calls that the tree does not keep, or 0 if the
  /// trace is not a tree.
  [[nodiscard]] std::uint64_t HiddenCalls() const;

  // NOLINTBEGIN(readability-identifier-naming): Google style accessors.
  [[nodiscard]] TraceMode mode() const { return options_.mode; }

  [[nodiscard]] const auto& roots() const { return roots_; }

  /// The calls of each function, in counts mode.
  [[nodiscard]] const auto& counts() const { return counts_; }

  // NOLINTEND(readability-identifier-naming)

 private:
  friend class Function;
  template <TraceMode kMode>
  friend class detail::Runner;

  /// Returns the error for a call after `max_calls`, shared by both evaluators.
  [[nodiscard]] Error LimitError() const;

  /// Owns an open call and closes it when the scope ends, whether evaluation
  /// succeeds or returns an error.
  class Scope {
   public:
    Scope(const Scope&) = delete;
    Scope& operator=(const Scope&) = delete;
    Scope(Scope&& other) noexcept;
    Scope& operator=(Scope&& other) noexcept;
    ~Scope();

    void SetResults(std::span<const Natural> results) const;

   private:
    friend class Trace;

    Scope(Trace& trace, TraceNode* node) : trace_(&trace), node_(node) {}

    void Leave();

    Trace* trace_;
    /// Null if the trace does not keep this call.
    TraceNode* node_;
  };

  /// Opens a call of `function` with `arguments` below the open call.
  ///
  /// # Errors
  ///
  /// An `Error` if the evaluation already made `max_calls` calls.
  [[nodiscard]] std::expected<Scope, Error> Enter(
      const Function& function,
      std::span<const Natural> arguments
  );

  TraceOptions options_;
  std::uint64_t calls_ = 0;
  std::uint64_t kept_ = 0;
  std::unordered_map<const Function*, std::uint64_t> counts_;
  std::vector<std::unique_ptr<TraceNode>> roots_;
  /// The open calls that the tree keeps, innermost last.
  std::vector<TraceNode*> open_;
};

}  // namespace prf

#endif  // NPDA_PRF_TRACE_H_
