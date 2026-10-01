#include "turing/transition_text.h"

#include <functional>
#include <span>
#include <string>

#include "automata/symbol.h"
#include "turing/machine.h"

namespace turing {

namespace {

template <typename Projection>
std::string JoinTapes(std::span<const TapeTransition> tapes,
                      Projection projection) {
  std::string text;
  for (const auto& tape : tapes) {
    if (!text.empty()) text += ',';
    text += std::invoke(projection, tape);
  }
  return text;
}

}  // namespace

std::string JoinReads(std::span<const TapeTransition> tapes) {
  return JoinTapes(tapes, [](const TapeTransition& tape) {
    return automata::SymbolName(tape.read);
  });
}

std::string JoinWrites(std::span<const TapeTransition> tapes) {
  return JoinTapes(tapes, [](const TapeTransition& tape) {
    return automata::SymbolName(tape.write);
  });
}

std::string JoinMoves(std::span<const TapeTransition> tapes) {
  return JoinTapes(tapes, [](const TapeTransition& tape) {
    return std::string(1, ToChar(tape.move));
  });
}

}  // namespace turing
