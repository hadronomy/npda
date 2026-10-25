#ifndef NPDA_SRC_TERMINAL_CAPABILITIES_H_
#define NPDA_SRC_TERMINAL_CAPABILITIES_H_

namespace terminal {

enum class OutputStream { kStdout, kStderr };

struct Capabilities {
  bool tty;
  bool utf8;
  bool color;
};

/// Returns what `stream` can show. Each stream is examined once, from its
/// locale and whether it is a terminal, and `NO_COLOR` or `TERM=dumb` turns its
/// color off.
[[nodiscard]] const Capabilities& CapabilitiesFor(OutputStream stream);

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_CAPABILITIES_H_
