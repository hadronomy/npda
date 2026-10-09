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
/// If the block does not give a key, the key keeps its `Config` default. An
/// unknown key gives a warning. A bad line or a bad value gives an error.
[[nodiscard]] ConfigParseResult ParseConfig(const diag::SourceFile& source);

}  // namespace turing

#endif  // NPDA_SRC_TURING_CONFIG_PARSER_H_
