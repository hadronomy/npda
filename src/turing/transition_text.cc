#include "turing/transition_text.h"

#include <functional>
#include <span>
#include <string>
#include <string_view>

#include "turing/transition.h"

namespace turing {

namespace {

template <typename Field>
std::string Join(
    std::span<const TapeAction> tapes,
    std::string_view separator,
    Field field
) {
  std::string text;
  for (const auto& action : tapes) {
    if (!text.empty()) text += separator;
    text += std::invoke(field, action);
  }
  return text;
}

}  // namespace

std::string JoinReads(
    std::span<const TapeAction> tapes,
    std::string_view separator
) {
  return Join(tapes, separator, [](const TapeAction& action) {
    return action.read.name();
  });
}

std::string JoinWrites(
    std::span<const TapeAction> tapes,
    std::string_view separator
) {
  return Join(tapes, separator, [](const TapeAction& action) {
    return action.write.name();
  });
}

std::string JoinMoves(
    std::span<const TapeAction> tapes,
    std::string_view separator
) {
  return Join(tapes, separator, [](const TapeAction& action) {
    return std::string(1, ToChar(action.move));
  });
}

std::string ActionText(const TapeAction& action) {
  if (action.move == HeadMove::kStay) return std::string(action.write.name());
  return std::string(1, ToChar(action.move));
}

std::string JoinActions(
    std::span<const TapeAction> tapes,
    std::string_view separator
) {
  return Join(tapes, separator, ActionText);
}

}  // namespace turing
