#ifndef NPDA_TURING_PARSER_H_
#define NPDA_TURING_PARSER_H_

#include <expected>
#include <istream>
#include <string>

#include "diagnostics/diagnostic.h"
#include "turing/machine.h"

namespace turing::parse {

struct ParseResult {
  std::expected<Machine, diag::DiagnosticSet> value =
      std::unexpected(diag::DiagnosticSet{});
  diag::SourceFile source;
  diag::DiagnosticSet warnings;
};

[[nodiscard]] ParseResult ParseWithDiagnostics(std::istream& input,
                                               std::string filename);

}  // namespace turing::parse

#endif  // NPDA_TURING_PARSER_H_
