#ifndef NPDA_SRC_TURING_TRANSITION_TEXT_H_
#define NPDA_SRC_TURING_TRANSITION_TEXT_H_

#include <span>
#include <string>
#include <string_view>

#include "turing/transition.h"

namespace turing {

/// Returns one field of all the tape actions, with a separator between them.
/// For the reads of a rule with two tapes, the result is `a,b`. Moves use the
/// letters of the file.
[[nodiscard]] std::string JoinReads(
    std::span<const TapeAction> tapes,
    std::string_view separator = ","
);
[[nodiscard]] std::string JoinWrites(
    std::span<const TapeAction> tapes,
    std::string_view separator = ","
);
[[nodiscard]] std::string JoinMoves(
    std::span<const TapeAction> tapes,
    std::string_view separator = ","
);

}  // namespace turing

#endif  // NPDA_SRC_TURING_TRANSITION_TEXT_H_
