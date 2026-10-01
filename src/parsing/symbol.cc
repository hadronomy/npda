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

// Process-lifetime string table behind Symbol. Entries never move
// (deque) and never free, so views into the table stay valid.
class Interner {
 public:
  [[nodiscard]] std::uint32_t Intern(std::string_view text);
  [[nodiscard]] std::string_view Lookup(std::uint32_t id) const;

 private:
  mutable std::mutex mutex_;
  std::deque<std::string> strings_;
  std::unordered_map<std::string_view, std::uint32_t> table_;
  std::uint32_t next_{};
};

std::uint32_t Interner::Intern(std::string_view s) {
  std::lock_guard lock(mutex_);
  if (const auto it = table_.find(s); it != table_.end()) return it->second;
  if (next_ == std::numeric_limits<std::uint32_t>::max()) std::abort();
  const std::uint32_t id = next_;
  strings_.emplace_back(s);
  table_.try_emplace(std::string_view(strings_.back()), id);
  ++next_;
  return id;
}

std::string_view Interner::Lookup(std::uint32_t id) const {
  std::lock_guard lock(mutex_);
  return strings_[id];
}

Interner& SharedTable() {
  static Interner table;
  return table;
}

}  // namespace

Symbol Intern(std::string_view s) { return Symbol(SharedTable().Intern(s)); }

std::string_view SymbolName(Symbol s) {
  return s.valid() ? SharedTable().Lookup(s.id_) : "<invalid>";
}

std::vector<Symbol> InputSymbols(std::string_view s) {
  std::vector<Symbol> v;
  v.reserve(s.size());
  for (char c : s)
    v.push_back(Intern(std::string_view(&c, 1)));  // "a" from 'a'
  return v;
}

}  // namespace automata
