#ifndef NPDA_SRC_PRF_CODE_H_
#define NPDA_SRC_PRF_CODE_H_

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <span>
#include <unordered_map>
#include <vector>

#include "prf/function.h"

/// The parts that the object evaluator and the compiled evaluator share, so
/// that both make the same calls and report the same errors.
namespace prf::detail {

enum class Operation : std::uint8_t {
  kZero,
  kSuccessor,
  kProjection,
  kCombination,
  kComposition,
  kRecursion,
};

/// One function object in a compiled program.
struct Node {
  /// The function object that the trace uses for this node's name and count.
  const Function* function;
  Operation operation;
  std::uint32_t arity;
  std::uint32_t outputs;
  /// The zero-based argument index for a projection, or the node index of
  /// f in f × g, f in f ∘ g, or the base g of a recursion.
  std::uint32_t first = 0;
  /// The node index of g in f × g, g in f ∘ g, or the step h of a recursion.
  std::uint32_t second = 0;
};

/// A compiled function whose `root` indexes its node in the flat `nodes` array.
struct Code {
  std::vector<Node> nodes;
  std::uint32_t root = 0;
};

/// Builds one node per function object, so shared parts keep one counter in
/// a trace.
class Assembler {
 public:
  /// Returns the node index of `function`, adding its parts and then its node
  /// on first use.
  std::uint32_t Add(const Function& function);

  /// Appends `node` and returns its index. The nodes of its parts must already
  /// exist.
  std::uint32_t Emit(Node node);

  [[nodiscard]] Code Finish(std::uint32_t root) &&;

 private:
  std::vector<Node> nodes_;
  std::unordered_map<const Function*, std::uint32_t> index_;
};

/// Holds temporary values for one call, with four values or fewer on the
/// stack. Almost every call fits there, which matters because a heap buffer
/// for each call doubles the time of an evaluation.
///
/// The values start uninitialized because each caller writes every value
/// before it reads it.
class Scratch {
 public:
  explicit Scratch(std::size_t size)
      : heap_(
            size > kStack ? std::make_unique_for_overwrite<Natural[]>(size)
                          : nullptr
        ),
        values_(heap_ ? heap_.get() : stack_.data(), size) {}

  Scratch(const Scratch&) = delete;
  Scratch& operator=(const Scratch&) = delete;

  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] std::span<Natural> values() const { return values_; }

 private:
  static constexpr std::size_t kStack = 4;

  std::array<Natural, kStack> stack_;
  std::unique_ptr<Natural[]> heap_;
  std::span<Natural> values_;
};

[[nodiscard]] inline Error SuccessorOverflow(Natural value) {
  return Error{std::format("S({}) does not fit in 64 bits", value)};
}

}  // namespace prf::detail

#endif  // NPDA_SRC_PRF_CODE_H_
