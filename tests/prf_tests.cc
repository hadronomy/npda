#include <array>
#include <cstdint>
#include <limits>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "prf/function.h"
#include "prf/trace.h"

namespace {

TEST_CASE("PRF exponentiation is independent of trace mode", "[prf][trace]") {
  const auto mode =
      GENERATE(prf::Trace::Mode::kOff, prf::Trace::Mode::kCountsOnly,
               prf::Trace::Mode::kFull);
  CAPTURE(static_cast<int>(mode));
  const auto power = prf::CreatePowerFunction();
  REQUIRE(power.has_value());
  prf::Trace trace(mode);
  const auto result =
      (*power)->Evaluate(std::array<std::uint64_t, 2>{2, 3}, trace);
  REQUIRE(result.has_value());
  CHECK(*result == 8);
  CHECK((trace.TotalCalls() == 0) == (mode == prf::Trace::Mode::kOff));
  CHECK(trace.roots().empty() == (mode != prf::Trace::Mode::kFull));
}

TEST_CASE("PRF evaluation checks argument arity", "[prf][validation]") {
  const auto power = prf::CreatePowerFunction();
  REQUIRE(power.has_value());
  prf::Trace trace;
  CHECK_FALSE(
      (*power)->Evaluate(std::array<std::uint64_t, 1>{2}, trace).has_value());
  CHECK(trace.roots().empty());
}

TEST_CASE("PRF trace scopes close after arithmetic failure", "[prf][trace]") {
  prf::Trace trace;
  const auto successor = prf::MakeSuccessor();
  const auto overflow = successor->Evaluate(
      std::array{std::numeric_limits<std::uint64_t>::max()}, trace);
  REQUIRE_FALSE(overflow.has_value());
  const auto result =
      successor->Evaluate(std::array<std::uint64_t, 1>{0}, trace);
  REQUIRE(result.has_value());
  CHECK(*result == 1);
  REQUIRE(trace.roots().size() == 2);
  CHECK_FALSE(trace.roots()[0]->result.has_value());
  CHECK(trace.roots()[0]->children.empty());
  CHECK(trace.roots()[1]->result == 1);
}

TEST_CASE("PRF construction checks indices, dependencies, and arity",
          "[prf][validation]") {
  const auto successor = prf::MakeSuccessor();
  SECTION("Projection indices are one-based") {
    CHECK_FALSE(prf::CreateProjection(0, 1).has_value());
    CHECK_FALSE(prf::CreateProjection(2, 1).has_value());
  }
  SECTION("Composition requires an outer function") {
    CHECK_FALSE(prf::CreateComposition(nullptr, {}).has_value());
  }
  SECTION("Composition requires one inner function per outer argument") {
    CHECK_FALSE(prf::CreateComposition(successor, {}).has_value());
  }
  SECTION("Recursion requires a base function") {
    CHECK_FALSE(prf::CreateRecursion(nullptr, successor).has_value());
  }
}

TEST_CASE("PRF projection and zero preserve their value contracts", "[prf]") {
  prf::Trace trace;
  const auto projection = prf::CreateProjection(2, 2);
  REQUIRE(projection.has_value());
  const auto selected =
      (*projection)->Evaluate(std::array<std::uint64_t, 2>{4, 9}, trace);
  REQUIRE(selected.has_value());
  CHECK(*selected == 9);
  const auto zero = prf::MakeZero(0)->Evaluate({}, trace);
  REQUIRE(zero.has_value());
  CHECK(*zero == 0);
}

}  // namespace
