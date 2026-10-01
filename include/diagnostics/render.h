#ifndef NPDA_DIAGNOSTICS_RENDER_H_
#define NPDA_DIAGNOSTICS_RENDER_H_

#include <cstddef>
#include <ostream>
#include <string_view>

#include "diagnostics/diagnostic.h"

namespace diag {

enum class ColorMode { kAuto, kAlways, kNever };

enum class CharSet { kAuto, kUnicode, kAscii };

struct RenderOptions {
  ColorMode color = ColorMode::kAuto;
  CharSet charset = CharSet::kAuto;
  std::size_t context_lines = 1;
  std::size_t tab_width = 4;
  bool links = true;  // file:// links; needs a TTY
  bool inline_marks =
      true;              // curly underline on the source row; needs color TTY
  bool verbose = false;  // expand same-code groups
};

void Render(std::ostream& output, const SourceFile& source,
            const DiagnosticSet& diagnostics,
            const RenderOptions& options = {});
void Render(std::ostream& output, const SourceCache& sources,
            std::string_view filename, const DiagnosticSet& diagnostics,
            const RenderOptions& options = {});

}  // namespace diag

#endif  // NPDA_DIAGNOSTICS_RENDER_H_
