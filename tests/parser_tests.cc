#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "automata/symbol.h"
#include "diagnostics/diagnostic.h"
#include "diagnostics/source.h"
#include "npda/definition.h"
#include "npda/machine.h"
#include "npda/parser.h"
#include "turing/machine.h"
#include "turing/parser.h"
#include "turing/transition.h"

namespace {

const std::string kTuringText = "q f\na\na _\nq\n_\nf\nq a f a S\n";

bool HasCode(const diag::DiagnosticSet& diagnostics, std::string_view code) {
  return std::ranges::any_of(diagnostics.items, [&](const diag::Diagnostic& d) {
    return d.code == code;
  });
}

npda::ParseResult ParseNpda(std::string text) {
  return npda::Parse(diag::SourceFile::From("test.npda", std::move(text)));
}

TEST_CASE(
    "NPDA parser accepts each documented epsilon spelling",
    "[parser][npda]"
) {
  const std::string epsilon =
      GENERATE("-", ".", "ε", "λ", "eps", "epsilon", "lambda");
  CAPTURE(epsilon);
  const auto parsed =
      ParseNpda("q f\na\nZ\nq\nZ\nf\nq " + epsilon + " Z f " + epsilon + "\n");
  REQUIRE(parsed.machine.has_value());
  CHECK(parsed.machine->Run({})->Accepted());
}

TEST_CASE("NPDA parser picks acceptance from the F line", "[parser][npda]") {
  SECTION("An F line selects final-state acceptance") {
    const auto parsed = ParseNpda("q f\na\nZ\nq\nZ\nf\nq a Z f Z\n");
    REQUIRE(parsed.machine.has_value());
    CHECK(
        std::holds_alternative<npda::AcceptByFinalState>(
            parsed.machine->definition().acceptance
        )
    );
  }
  SECTION("No F line selects empty-stack acceptance") {
    const auto parsed = ParseNpda("q\na\nZ\nq\nZ\nq a Z q .\n");
    REQUIRE(parsed.machine.has_value());
    CHECK(
        std::holds_alternative<npda::AcceptByEmptyStack>(
            parsed.machine->definition().acceptance
        )
    );
  }
}

TEST_CASE(
    "NPDA parser splits push strings into stack symbols",
    "[parser][npda]"
) {
  const auto parsed = ParseNpda("q\na\nZ AB\nq\nZ\nq a Z q ABZ\n");
  REQUIRE(parsed.machine.has_value());
  const auto& push = parsed.machine->definition().transitions.front().push;
  REQUIRE(push.size() == 2);
  CHECK(push[0].name() == "AB");
  CHECK(push[1].name() == "Z");
}

TEST_CASE("NPDA parser requires a stack top from Γ", "[parser][npda]") {
  const auto parsed = ParseNpda("q\na\nZ\nq\nZ\nq a . q Z\n");
  CHECK_FALSE(parsed.machine.has_value());
  CHECK(HasCode(parsed.diagnostics, "E0013"));
}

TEST_CASE("NPDA parser reports a missing section once", "[parser][npda]") {
  const auto parsed = ParseNpda("q\na\nZ\nq\n");
  CHECK_FALSE(parsed.machine.has_value());
  REQUIRE(parsed.diagnostics.items.size() == 1);
  CHECK(parsed.diagnostics.items.front().code == "E0001");
}

TEST_CASE("NPDA parser reports an empty document", "[parser][npda]") {
  const auto parsed = ParseNpda("");
  CHECK_FALSE(parsed.machine.has_value());
  CHECK(parsed.diagnostics.HasErrors());
}

TEST_CASE("Turing parser returns an executable machine", "[parser][turing]") {
  const auto parsed =
      turing::Parse(diag::SourceFile::From("machine.turing", kTuringText));
  REQUIRE(parsed.machine.has_value());
  CHECK(parsed.diagnostics.items.empty());
  CHECK(parsed.machine->Run(automata::InputSymbols("a"))->accepted);
}

TEST_CASE(
    "Turing parser reads one write and move pair per tape",
    "[parser][turing]"
) {
  const auto parsed = turing::Parse(
      diag::SourceFile::From(
          "two.turing",
          "# /// config\n# num_tapes = 2\n# ///\nq f\na\na b _\nq\n_\nf\n"
          "q a _ f b R a L\n"
      )
  );
  REQUIRE(parsed.machine.has_value());
  const auto& tapes = parsed.machine->definition().transitions.front().tapes;
  REQUIRE(tapes.size() == 2);
  CHECK(tapes[0].write.name() == "b");
  CHECK(tapes[0].move == turing::HeadMove::kRight);
  CHECK(tapes[1].write.name() == "a");
  CHECK(tapes[1].move == turing::HeadMove::kLeft);
}

TEST_CASE(
    "Turing parser rejects invalid and duplicate configuration",
    "[parser][turing][configuration]"
) {
  const std::string configuration = GENERATE(
      "num_tapes = 0",
      "allow_stay = yes",
      "num_tapes = 1\n# num_tapes = 2"
  );
  CAPTURE(configuration);
  const auto parsed = turing::Parse(
      diag::SourceFile::From(
          "bad.turing",
          "# /// config\n# " + configuration + "\n# ///\n" + kTuringText
      )
  );
  CHECK_FALSE(parsed.machine.has_value());
  CHECK(parsed.diagnostics.HasErrors());
}

TEST_CASE(
    "Turing parser reports unknown configuration as a warning",
    "[parser][turing][configuration]"
) {
  const auto parsed = turing::Parse(
      diag::SourceFile::From(
          "warn.turing",
          "# /// config\n# unknown_key = true\n# ///\n" + kTuringText
      )
  );
  REQUIRE(parsed.machine.has_value());
  CHECK_FALSE(parsed.diagnostics.HasErrors());
  REQUIRE(parsed.diagnostics.items.size() == 1);
  CHECK(parsed.diagnostics.items.front().code == "C0012");
}

TEST_CASE(
    "Source positions use one-based lines and columns with CRLF input",
    "[diagnostics][source]"
) {
  const auto file = diag::SourceFile::From("sample", "one\r\ntwo\n");
  CHECK(file.Line(1) == "one");
  CHECK(file.Line(2) == "two");
  CHECK(file.LineColumn(5) == std::pair<std::size_t, std::size_t>{2, 1});
  CHECK(file.Line(100).empty());
}

}  // namespace
