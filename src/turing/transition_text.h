#ifndef NPDA_SRC_TURING_TRANSITION_TEXT_H_
#define NPDA_SRC_TURING_TRANSITION_TEXT_H_

#include <span>
#include <string>

#include "turing/machine.h"

namespace turing {

[[nodiscard]] std::string JoinReads(std::span<const TapeTransition> tapes);
[[nodiscard]] std::string JoinWrites(std::span<const TapeTransition> tapes);
[[nodiscard]] std::string JoinMoves(std::span<const TapeTransition> tapes);

}  // namespace turing

#endif  // NPDA_SRC_TURING_TRANSITION_TEXT_H_
