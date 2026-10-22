#ifndef NPDA_TURING_CONFIG_H_
#define NPDA_TURING_CONFIG_H_

namespace turing {

enum class TapeDirection {
  /// The tape has no end on either side.
  kBidirectional,
  /// The tape has a left end at the first cell of the input, where a move to
  /// the left keeps the head on that cell.
  kRightOnly,
};

/// What one transition can do to each tape.
enum class OperationMode {
  /// The transition writes a symbol and then moves the head: `X R`.
  kSimultaneous,
  /// The transition either writes a symbol with the head fixed (`X`) or moves
  /// the head with the symbol unchanged (`R`).
  kIndependent,
};

/// The machine variant, selected by the file's `# /// config` block, with
/// defaults from the course statement.
struct Config {
  TapeDirection tape_direction = TapeDirection::kBidirectional;
  OperationMode operation_mode = OperationMode::kSimultaneous;
  /// If false, the only head moves are L and R.
  bool allow_stay = true;
};

}  // namespace turing

#endif  // NPDA_TURING_CONFIG_H_
