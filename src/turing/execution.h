#ifndef NPDA_SRC_TURING_EXECUTION_H_
#define NPDA_SRC_TURING_EXECUTION_H_

#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <vector>

#include "turing/machine.h"

namespace turing {

class Machine::Execution {
 public:
  Execution(const Machine& machine, std::span<const Symbol> input,
            RunOptions options);
  std::expected<RunResult, Error> Execute();

 private:
  class History {
   public:
    explicit History(const TapeConfiguration& root);
    void Append(const TapeConfiguration& configuration);
    std::span<const TapeConfiguration> Nodes() const;
    std::size_t LastIndex() const;

   private:
    // Construction records a root, so LastIndex cannot underflow.
    std::vector<TapeConfiguration> nodes_;
  };

  void Observe(EventKind kind) const;
  std::optional<std::size_t> FindTransition() const;
  void Advance(std::size_t transition_index);
  RunResult Accept();

  const Machine& machine_;
  const RunOptions options_;
  TapeConfiguration current_;
  std::optional<History> history_;
  std::optional<std::vector<std::size_t>> witness_;
  std::size_t steps_ = 0;
};

}  // namespace turing

#endif  // NPDA_SRC_TURING_EXECUTION_H_
