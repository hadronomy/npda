#ifndef NPDA_NPDA_STACK_VIEW_H_
#define NPDA_NPDA_STACK_VIEW_H_

#include <cstddef>
#include <iterator>
#include <span>

#include "automata/symbol.h"

namespace npda {

using automata::Symbol;

/// The stack of a configuration, from the top down.
///
/// A view refers to the run's shared table, where each cell holds a symbol and
/// the index of the stack below it. Because equal stacks share a cell and a
/// push adds cells above existing ones, the run never copies a stack.
class StackView {
 public:
  struct Cell {
    Symbol symbol;
    std::size_t below;
  };

  static constexpr std::size_t kEmpty = static_cast<std::size_t>(-1);

  class Iterator {
   public:
    // NOLINTBEGIN(readability-identifier-naming): std names these.
    using iterator_concept = std::forward_iterator_tag;
    using value_type = Symbol;
    using difference_type = std::ptrdiff_t;
    // NOLINTEND(readability-identifier-naming)

    Iterator() = default;

    Symbol operator*() const { return cells_[index_].symbol; }

    Iterator& operator++() {
      index_ = cells_[index_].below;
      return *this;
    }

    Iterator operator++(int) {
      Iterator before = *this;
      ++*this;
      return before;
    }

    bool operator==(const Iterator&) const = default;

   private:
    friend class StackView;

    Iterator(const Cell* cells, std::size_t index)
        : cells_(cells), index_(index) {}

    const Cell* cells_ = nullptr;
    std::size_t index_ = kEmpty;
  };

  StackView(std::span<const Cell> cells, std::size_t top)
      : cells_(cells), top_(top) {}

  // NOLINTBEGIN(readability-identifier-naming): std::ranges needs these.
  [[nodiscard]] bool empty() const { return top_ == kEmpty; }

  [[nodiscard]] Iterator begin() const { return {cells_.data(), top_}; }

  [[nodiscard]] Iterator end() const { return {cells_.data(), kEmpty}; }

  // NOLINTEND(readability-identifier-naming)

 private:
  std::span<const Cell> cells_;
  std::size_t top_;
};

}  // namespace npda

#endif  // NPDA_NPDA_STACK_VIEW_H_
