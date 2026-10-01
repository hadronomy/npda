#include <cstddef>
#include <sstream>
#include <string>
#include <utility>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "npda/machine.h"
#include "npda/parser.h"
#include "turing/machine.h"
#include "turing/parser.h"

namespace {

const std::string kTuringText = "q f\na\na _\nq\n_\nf\nq a f a S\n";

TEST_CASE("NPDA parser accepts each documented epsilon spelling",
          "[parser][npda]") {
  const std::string epsilon =
      GENERATE("-", ".", "ε", "λ", "eps", "epsilon", "lambda");
  CAPTURE(epsilon);
  std::istringstream source("q f\na\nZ\nq\nZ\nf\nq " + epsilon + " Z f " +
                            epsilon + "\n");
  const auto parsed = npda::parse::ParseWithDiagnostics(source, "epsilon.npda");
  REQUIRE(parsed.value.has_value());
  const auto result = parsed.value->Run({});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
}

TEST_CASE("NPDA parser reports an empty document", "[parser][npda]") {
  std::istringstream source;
  const auto parsed = npda::parse::ParseWithDiagnostics(source, "empty.npda");
  REQUIRE_FALSE(parsed.value.has_value());
  CHECK(parsed.value.error().HasErrors());
  CHECK(parsed.source.filename == "empty.npda");
}

TEST_CASE("Turing parser returns an executable machine", "[parser][turing]") {
  std::istringstream source(kTuringText);
  const auto parsed =
      turing::parse::ParseWithDiagnostics(source, "machine.turing");
  REQUIRE(parsed.value.has_value());
  const auto result = parsed.value->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(parsed.warnings.items.empty());
}

TEST_CASE("Turing parser rejects invalid and duplicate configuration",
          "[parser][turing][configuration]") {
  const std::string configuration = GENERATE(
      "num_tapes = 0", "allow_stay = yes", "num_tapes = 1\n# num_tapes = 2");
  CAPTURE(configuration);
  std::istringstream source("# /// config\n# " + configuration + "\n# ///\n" +
                            kTuringText);
  const auto parsed = turing::parse::ParseWithDiagnostics(source, "bad.turing");
  REQUIRE_FALSE(parsed.value.has_value());
  CHECK(parsed.value.error().HasErrors());
}

TEST_CASE("Turing parser reports unknown configuration as a warning",
          "[parser][turing][configuration]") {
  std::istringstream source("# /// config\n# unknown_key = true\n# ///\n" +
                            kTuringText);
  const auto parsed =
      turing::parse::ParseWithDiagnostics(source, "warn.turing");
  REQUIRE(parsed.value.has_value());
  CHECK_FALSE(parsed.warnings.HasErrors());
  REQUIRE(parsed.warnings.items.size() == 1);
  CHECK(parsed.warnings.items.front().code == "C0012");
}

TEST_CASE("Source positions use one-based lines and columns with CRLF input",
          "[diagnostics][source]") {
  const auto file = diag::SourceFile::From("sample", "one\r\ntwo\n");
  CHECK(file.line_view(1) == "one");
  CHECK(file.line_view(2) == "two");
  CHECK(file.line_col(5) == std::pair<std::size_t, std::size_t>{2, 1});
  CHECK(file.line_view(100).empty());
}

}  // namespace
