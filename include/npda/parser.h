#ifndef NPDA_NPDA_PARSER_H_
#define NPDA_NPDA_PARSER_H_

#include <expected>
#include <istream>
#include <string>

#include "diagnostics/diagnostic.h"
#include "npda/machine.h"

namespace npda::parse {

struct ParseResult {
  std::expected<Machine, diag::DiagnosticSet> value =
      std::unexpected(diag::DiagnosticSet{});
  diag::SourceFile source;
};

[[nodiscard]] ParseResult ParseWithDiagnostics(std::istream& input,
                                               std::string filename);

}  // namespace npda::parse

#endif  // NPDA_NPDA_PARSER_H_
