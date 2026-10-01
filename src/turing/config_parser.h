#ifndef NPDA_SRC_TURING_CONFIG_PARSER_H_
#define NPDA_SRC_TURING_CONFIG_PARSER_H_
#include "diagnostics/diagnostic.h"
#include "turing/machine.h"

namespace turing::parse {

struct ConfigParseResult {
  MachineConfig config;
  diag::DiagnosticSet diagnostics;
};

[[nodiscard]] ConfigParseResult ParseConfiguration(
    const diag::SourceFile& source);

}  // namespace turing::parse
#endif  // NPDA_SRC_TURING_CONFIG_PARSER_H_
