#include "automata/symbol.h"

#include <cstdint>
#include <cstdlib>
#include <deque>
#include <limits>
#include <mutex>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace automata {

namespace {

/// The table of texts for `Symbol`. It lives until the process ends.
///
/// A deque does not move its elements, and the table never removes a text.
/// Thus, views into the table stay valid.
class Interner {
 public:
  [[nodiscard]] std::uint32_t Intern(std::string_view text) {
    std::lock_guard lock(mutex_);
    if (const auto it = ids_.find(text); it != ids_.end()) return it->second;
    if (texts_.size() == std::numeric_limits<std::uint32_t>::max()) {
      std::abort();
    }
    const auto id = static_cast<std::uint32_t>(texts_.size());
    ids_.try_emplace(texts_.emplace_back(text), id);
    return id;
  }

  [[nodiscard]] std::string_view Lookup(std::uint32_t id) const {
    std::lock_guard lock(mutex_);
    return texts_[id];
  }

 private:
  mutable std::mutex mutex_;
  std::deque<std::string> texts_;
  std::unordered_map<std::string_view, std::uint32_t> ids_;
};

Interner& Table() {
  static Interner table;
  return table;
}

/// Returns the length of the UTF-8 sequence that starts with `lead`. A
/// continuation byte without a lead byte counts as one byte. Thus, bad input
/// does not stop the loop.
std::size_t SequenceLength(unsigned char lead) {
  if (lead >= 0xF0) return 4;
  if (lead >= 0xE0) return 3;
  if (lead >= 0xC0) return 2;
  return 1;
}

}  // namespace

std::string_view Symbol::name() const { return Table().Lookup(id_); }

Symbol Intern(std::string_view text) { return Symbol(Table().Intern(text)); }

std::vector<Symbol> InputSymbols(std::string_view text) {
  std::vector<Symbol> symbols;
  symbols.reserve(text.size());
  for (std::size_t i = 0; i < text.size();) {
    const auto length = SequenceLength(static_cast<unsigned char>(text[i]));
    symbols.push_back(Intern(text.substr(i, length)));
    i += length;
  }
  return symbols;
}

}  // namespace automata
