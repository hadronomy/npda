#include "prf/trace.h"

#include <cstdint>
#include <cstdlib>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <utility>

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

void Trace::Scope::SetResult(std::uint64_t result) const {
  if (node_) node_->result = result;
}

void Trace::Scope::Leave() {
  if (trace_ && node_) {
    if (trace_->stack_.empty()) std::abort();
    trace_->stack_.pop_back();
    trace_ = nullptr;
    node_ = nullptr;
  }
}

Trace::Scope Trace::Enter(std::string_view name,
                          std::span<const std::uint64_t> arguments) {
  if (mode_ != Mode::kOff) ++counts_[std::string(name)];
  if (mode_ != Mode::kFull) return Scope(*this, nullptr);
  auto node = std::make_unique<TraceNode>();
  node->name = name;
  node->args.assign(arguments.begin(), arguments.end());
  auto* current = node.get();
  if (stack_.empty())
    roots_.push_back(std::move(node));
  else
    stack_.back()->children.push_back(std::move(node));
  stack_.push_back(current);
  return Scope(*this, current);
}

std::uint64_t Trace::TotalCalls() const {
  std::uint64_t calls = 0;
  for (const auto& [name, count] : counts_) calls += count;
  return calls;
}

}  // namespace prf
