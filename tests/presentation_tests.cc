#include <sstream>

#include <catch2/catch_test_macros.hpp>

#include "prf/function.h"
#include "prf/trace.h"
#include "prf/trace_render.h"
#include "terminal/style.h"

namespace {

const terminal::Style kPlain = terminal::Style::Plain();

TEST_CASE(
    "PRF trace output shows call counts or a call tree",
    "[presentation][prf]"
) {
  const auto power = prf::Power();
  const auto write = [&](prf::TraceOptions options,
                         const terminal::Style& style) {
    prf::Trace trace(options);
    REQUIRE(power.Evaluate({2, 3}, trace).has_value());
    std::ostringstream out;
    prf::WriteTrace(out, trace, style);
    return out.str();
  };
  SECTION("Counts share a row for each name, with the largest count first") {
    const auto output = write({.mode = prf::TraceMode::kCounts}, kPlain);
    CHECK(output.starts_with("\n  calls  function\n     23  P₃³\n"));
    CHECK(output.contains("      4  Z\n"));
    CHECK(output.ends_with("      1  pow\n"));
  }
  SECTION("The tree starts below the top-level call") {
    const auto output = write({.mode = prf::TraceMode::kTree}, kPlain);
    CHECK(output.starts_with("\n├─ (S ∘ Z)(2) = 1\n"));
    CHECK(output.contains("(P₃³ × P₁³)(2, 0, 1) = (1, 2)\n"));
    CHECK_FALSE(output.contains("pow("));
  }
  SECTION("The tree reports how many calls it does not show") {
    const auto output =
        write({.mode = prf::TraceMode::kTree, .tree_limit = 3}, kPlain);
    CHECK(output.contains("└─ (S ∘ Z)(2) = 1\n   └─ Z(2) = 0\n"));
    CHECK(output.ends_with(
        "does not show 97 more calls. Increase -trace-limit "
        "to show them.\n"
    ));
  }
  SECTION("A definition groups its equations and aligns them") {
    std::ostringstream out;
    prf::WriteDefinition(out, power.function(), kPlain);
    CHECK(out.str().starts_with(
        "pow(x, 0)    = (S ∘ Z)(x)\n"
        "pow(x, S(y)) = (mul ∘ (P₃³ × P₁³))(x, y, pow(x, y))\n"
        "\n"
        "mul(x, 0)    = Z(x)\n"
    ));
    CHECK(out.str().ends_with("add(x, S(y)) = (S ∘ P₃³)(x, y, add(x, y))\n"));
  }
  SECTION("A call shows one result alone and more results in parentheses") {
    CHECK(
        prf::FormatCall(
            power.function(),
            prf::Tuple{2, 3},
            prf::Tuple{8},
            kPlain
        ) == "pow(2, 3) = 8"
    );
    const auto pair =
        prf::Combination(prf::Projection<2, 2>(), prf::Projection<1, 2>());
    CHECK(
        prf::FormatCall(
            pair.function(),
            prf::Tuple{4, 9},
            prf::Tuple{9, 4},
            kPlain
        ) == "(P₂² × P₁²)(4, 9) = (9, 4)"
    );
  }
}

TEST_CASE(
    "Plain styles never emit escapes and keep glyphs",
    "[presentation][style]"
) {
  CHECK(kPlain.Paint(terminal::Tone::kBad, "error") == "error");
  CHECK(kPlain.glyphs().ellipsis == "…");
  CHECK(terminal::PadLeft("7", 3) == "  7");
  CHECK(terminal::PadRight("…", 3) == "…  ");
}

TEST_CASE(
    "Large numbers group their digits in threes",
    "[presentation][style]"
) {
  CHECK(kPlain.Number(0) == "0");
  CHECK(kPlain.Number(9999) == "9999");
  CHECK(kPlain.Number(10000) == "10\u202F000");
  CHECK(kPlain.Number(805306852) == "805\u202F306\u202F852");
  CHECK(
      kPlain.Number(18446744073709551615U) ==
      "18\u202F446\u202F744\u202F073\u202F709\u202F551\u202F615"
  );
}

}  // namespace
