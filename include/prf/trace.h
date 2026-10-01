#ifndef NPDA_PRF_TRACE_H_
#define NPDA_PRF_TRACE_H_

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace prf {

struct TraceNode {
  std::string name;
  std::vector<std::uint64_t> args;
  std::optional<std::uint64_t> result;
  std::vector<std::unique_ptr<TraceNode>> children;
};

class Trace {
 public:
  enum class Mode { kFull, kCountsOnly, kOff };

  explicit Trace(Mode mode = Mode::kFull) : mode_(mode) {}

  Trace(const Trace&) = delete;
  Trace& operator=(const Trace&) = delete;

  class Scope {
   public:
    Scope(const Scope&) = delete;
    Scope& operator=(const Scope&) = delete;
    Scope(Scope&& other) noexcept;
    Scope& operator=(Scope&& other) noexcept;
    ~Scope();
    void SetResult(std::uint64_t result) const;

   private:
    friend class Trace;

    Scope(Trace& trace, TraceNode* node) : trace_(&trace), node_(node) {}

    void Leave();
    Trace* trace_;
    TraceNode* node_;
  };

  // A scope closes the active call on both success and error paths.
  [[nodiscard]] Scope Enter(std::string_view name,
                            std::span<const std::uint64_t> arguments);
  [[nodiscard]] std::uint64_t TotalCalls() const;

  [[nodiscard]] Mode mode() const { return mode_; }

  [[nodiscard]] const auto& roots() const { return roots_; }

  [[nodiscard]] const auto& counts() const { return counts_; }

 private:
  std::vector<std::unique_ptr<TraceNode>> roots_;
  std::vector<TraceNode*> stack_;
  std::unordered_map<std::string, std::uint64_t> counts_;
  Mode mode_;
};

}  // namespace prf

#endif  // NPDA_PRF_TRACE_H_
