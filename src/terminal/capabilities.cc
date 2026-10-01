#include "terminal/capabilities.h"

#include <unistd.h>

#include <cctype>
#include <cstdlib>
#include <string>
#include <string_view>

namespace terminal {

namespace {

Capabilities Probe(int descriptor) {
  Capabilities result{
      .tty = isatty(descriptor) != 0, .utf8 = true, .color = false};
  std::string locale;
  for (const char* key : {"LC_ALL", "LC_CTYPE", "LANG"}) {
    if (const char* value = std::getenv(key); value && value[0]) {
      locale = value;
      break;
    }
  }
  if (!locale.empty()) {
    for (auto& character : locale) {
      character = static_cast<char>(
          std::tolower(static_cast<unsigned char>(character)));
    }
    result.utf8 = locale.contains("utf-8") || locale.contains("utf8");
  }
  const char* no_color = std::getenv("NO_COLOR");
  const char* term = std::getenv("TERM");
  result.color = result.tty && !(no_color && no_color[0]) &&
                 !(term && std::string_view(term) == "dumb");
  return result;
}

}  // namespace

const Capabilities& CapabilitiesFor(OutputStream stream) {
  static const Capabilities kStdout = Probe(STDOUT_FILENO);
  static const Capabilities kStderr = Probe(STDERR_FILENO);
  return stream == OutputStream::kStdout ? kStdout : kStderr;
}

}  // namespace terminal
