#ifndef NPDA_TURING_CONFIG_H_
#define NPDA_TURING_CONFIG_H_

#include <cstddef>

namespace turing {

enum class TapeDirection {
  /// The tape has no end on either side.
  kBidirectional,
  /// The tape has no end on the right. Transitions cannot move left.
  kRightOnly,
};

/// The order in which a transition writes and moves on its tapes.
///
/// The write and the move on a tape change only that tape. Thus, both orders
/// give the same configuration.
enum class OperationMode { kSimultaneous, kIndependent };

/// The settings in the `# /// config` block of the file.
struct Config {
  std::size_t num_tapes = 1;
  TapeDirection tape_direction = TapeDirection::kBidirectional;
  OperationMode operation_mode = OperationMode::kIndependent;
  bool allow_stay = true;
};

}  // namespace turing

#endif  // NPDA_TURING_CONFIG_H_
