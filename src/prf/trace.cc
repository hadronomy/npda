#include "prf/trace.h"

#include <cstdint>
#include <cstdlib>
#include <expected>
#include <format>
#include <memory>
#include <span>
#include <utility>

#include "prf/function.h"

namespace prf {

Trace::Scope::Scope(Scope&& other) noexcept
    : trace_(std::exchange(other.trace_, nullptr)),
      node_(std::exchange(other.node_, nullptr)) {}

Trace::Scope& Trace::Scope::operator=(Scope&& other) noexcept {
  if (this != &other) {
    Leave();
    trace_ = std::exchange(other.trace_, nullptr);
    node_ = std::exchange(other.node_, nullptr);
  }
  return *this;
}

Trace::Scope::~Scope() { Leave(); }

void Trace::Scope::SetResults(std::span<const Natural> results) const {
  if (node_) node_->results.emplace(results.begin(), results.end());
}

void Trace::Scope::Leave() {
  if (trace_ && node_) {
    // Scopes close in the reverse order of their calls, so the innermost open
    // call is this one.
    if (trace_->open_.empty() || trace_->open_.back() != node_) std::abort();
    trace_->open_.pop_back();
  }
  trace_ = nullptr;
  node_ = nullptr;
}

std::expected<Trace::Scope, Error> Trace::Enter(
    const Function& function,
    std::span<const Natural> arguments
) {
  if (calls_ == options_.max_calls) {
    return std::unexpected(
        Error{std::format(
            "the evaluation needs more than {} calls",
            options_.max_calls
        )}
    );
  }
  ++calls_;
  switch (options_.mode) {
    case TraceMode::kOff:
      return Scope(*this, nullptr);
    case TraceMode::kCounts:
      ++counts_[&function];
      return Scope(*this, nullptr);
    case TraceMode::kTree:
      break;
  }
  // Because callers precede callees in call order, keeping the first calls
  // also keeps all their callers, while omitted calls have no retained callees.
  if (kept_ == options_.tree_limit) return Scope(*this, nullptr);
  ++kept_;
  auto node = std::make_unique<TraceNode>();
  node->function = &function;
  node->arguments.assign(arguments.begin(), arguments.end());
  auto* kept = node.get();
  auto& siblings = open_.empty() ? roots_ : open_.back()->children;
  siblings.push_back(std::move(node));
  open_.push_back(kept);
  return Scope(*this, kept);
}

std::uint64_t Trace::HiddenCalls() const {
  return options_.mode == TraceMode::kTree ? calls_ - kept_ : 0;
}

}  // namespace prf
