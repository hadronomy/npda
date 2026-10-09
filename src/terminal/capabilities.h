#ifndef NPDA_SRC_TERMINAL_CAPABILITIES_H_
#define NPDA_SRC_TERMINAL_CAPABILITIES_H_

namespace terminal {

enum class OutputStream { kStdout, kStderr };

/// What the destination of a stream can show.
struct Capabilities {
  bool tty;
  bool utf8;
  bool color;
};

/// Returns what `stream` can show.
///
/// The function examines each stream one time only. It reads the locale and
/// finds if the stream is a terminal. It also obeys `NO_COLOR` and `TERM=dumb`.
[[nodiscard]] const Capabilities& CapabilitiesFor(OutputStream stream);

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_CAPABILITIES_H_
