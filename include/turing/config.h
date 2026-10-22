#ifndef NPDA_TURING_CONFIG_H_
#define NPDA_TURING_CONFIG_H_

namespace turing {

enum class TapeDirection {
  /// The tape has no end on either side.
  kBidirectional,
  /// The tape has a left end at the first cell of the input. A move to the
  /// left from that cell keeps the head on the cell.
  kRightOnly,
};

/// What one transition can do to each tape.
enum class OperationMode {
  /// The transition writes a symbol and then moves the head: `X R`.
  kSimultaneous,
  /// The transition writes a symbol or moves the head, not both: `X` or `R`.
  /// A write keeps the head on its cell. A move keeps the symbol of the cell.
  kIndependent,
};

/// The variant of the machine. The `# /// config` block of the file sets it.
/// The defaults are the variant of the course statement.
struct Config {
  TapeDirection tape_direction = TapeDirection::kBidirectional;
  OperationMode operation_mode = OperationMode::kSimultaneous;
  /// If false, the only head moves are L and R.
  bool allow_stay = true;
};

}  // namespace turing

#endif  // NPDA_TURING_CONFIG_H_
