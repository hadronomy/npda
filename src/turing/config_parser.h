#ifndef NPDA_SRC_TURING_CONFIG_PARSER_H_
#define NPDA_SRC_TURING_CONFIG_PARSER_H_

#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "turing/config.h"

namespace turing {

struct ConfigParseResult {
  Config config;
  diag::DiagnosticSet diagnostics;
};

/// Reads the `# /// config` comment block of a Turing machine file.
///
/// Omitted keys keep their `Config` defaults. For entries in the block, an
/// unknown key gives a warning, while an invalid line or value gives an error.
[[nodiscard]] ConfigParseResult ParseConfig(const diag::SourceFile& source);

}  // namespace turing

#endif  // NPDA_SRC_TURING_CONFIG_PARSER_H_
