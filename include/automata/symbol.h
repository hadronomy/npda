#ifndef NPDA_AUTOMATA_SYMBOL_H_
#define NPDA_AUTOMATA_SYMBOL_H_

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <string_view>
#include <vector>

namespace automata {

/// An interned token: a state, an alphabet symbol, or an input character.
///
/// Because only `Intern` makes symbols, each symbol refers to interned text.
/// Copies, equality, and hashing use one integer without reading that text.
class Symbol {
 public:
  [[nodiscard]] constexpr bool operator==(const Symbol&) const noexcept =
      default;

  /// Returns a view of the text that stays valid until the process ends.
  // NOLINTNEXTLINE(readability-identifier-naming): Google style accessor.
  [[nodiscard]] std::string_view name() const;

 private:
  friend Symbol Intern(std::string_view text);
  friend struct std::hash<Symbol>;

  explicit constexpr Symbol(std::uint32_t id) noexcept : id_(id) {}

  std::uint32_t id_;
};

/// Returns the symbol for `text`. Equal texts give equal symbols.
[[nodiscard]] Symbol Intern(std::string_view text);

/// Divides an input string into symbols, one for each UTF-8 code point.
///
/// Machine file alphabets instead keep whole tokens, so `Z0` in a file is one
/// symbol.
[[nodiscard]] std::vector<Symbol> InputSymbols(std::string_view text);

}  // namespace automata

template <>
struct std::hash<automata::Symbol> {
  std::size_t operator()(automata::Symbol symbol) const noexcept {
    return std::hash<std::uint32_t>{}(symbol.id_);
  }
};

template <typename Char>
struct std::formatter<automata::Symbol, Char>
    : std::formatter<std::string_view, Char> {
  // NOLINTNEXTLINE(readability-identifier-naming): std::formatter needs it.
  auto format(automata::Symbol symbol, auto& context) const {
    return std::formatter<std::string_view, Char>::format(
        symbol.name(),
        context
    );
  }
};

#endif  // NPDA_AUTOMATA_SYMBOL_H_
