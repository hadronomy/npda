#ifndef NPDA_AUTOMATA_SYMBOL_H_
#define NPDA_AUTOMATA_SYMBOL_H_

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <string_view>
#include <vector>

namespace automata {

// Intern() is the only way to create a valid symbol. Copies share stable text.
class Symbol {
 public:
  Symbol() = default;

  [[nodiscard]] constexpr bool valid() const noexcept {
    return id_ != kInvalidId;
  }

  [[nodiscard]] constexpr bool operator==(const Symbol&) const noexcept =
      default;

  [[nodiscard]] std::size_t Hash() const noexcept {
    return std::hash<std::uint32_t>{}(id_);
  }

 private:
  friend Symbol Intern(std::string_view text);
  friend std::string_view SymbolName(Symbol symbol);

  explicit Symbol(std::uint32_t id) : id_(id) {}

  static constexpr std::uint32_t kInvalidId = static_cast<std::uint32_t>(-1);
  std::uint32_t id_ = kInvalidId;
};

[[nodiscard]] Symbol Intern(std::string_view text);
// The table owns the returned bytes for the lifetime of the process.
[[nodiscard]] std::string_view SymbolName(Symbol symbol);
// Command input uses one symbol per byte. File alphabets retain whole tokens.
[[nodiscard]] std::vector<Symbol> InputSymbols(std::string_view text);

}  // namespace automata

namespace std {

template <>
struct hash<automata::Symbol> {
  std::size_t operator()(automata::Symbol s) const noexcept { return s.Hash(); }
};

// Format through the table, so "{}" keeps working on symbols.
template <typename Char>
struct formatter<automata::Symbol, Char> {
  formatter<std::string_view, Char> base_;

  constexpr auto parse(auto& pc) { return base_.parse(pc); }

  template <typename Ctx>
  auto format(automata::Symbol s, Ctx& ctx) const {
    return base_.format(automata::SymbolName(s), ctx);
  }
};

}  // namespace std

#endif  // NPDA_AUTOMATA_SYMBOL_H_
