#ifndef NPDA_SRC_TERMINAL_CAPABILITIES_H_
#define NPDA_SRC_TERMINAL_CAPABILITIES_H_

namespace terminal {

enum class OutputStream { kStdout, kStderr };

struct Capabilities {
  bool tty;
  bool utf8;
  bool color;
};

// Captures the locale, terminal, and color policy once per output stream.
[[nodiscard]] const Capabilities& CapabilitiesFor(OutputStream stream);

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_CAPABILITIES_H_
