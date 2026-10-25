#include <array>
#include <concepts>
#include <cstddef>
#include <limits>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "prf/function.h"
#include "prf/trace.h"

namespace {

using prf::Fn;
using prf::Natural;
using prf::Notation;
using prf::Tuple;

template <std::size_t N, std::size_t M>
std::array<Natural, M> Evaluate(
    const Fn<N, M>& function,
    const std::array<Natural, N>& arguments
) {
  prf::Trace trace({.mode = prf::TraceMode::kOff});
  const auto results = function.Evaluate(arguments, trace);
  REQUIRE(results.has_value());
  return *results;
}

/// xʸ with pow(0, 0) = 1, as the recursion defines it.
Natural Power(Natural base, Natural exponent) {
  Natural power = 1;
  for (Natural step = 0; step < exponent; ++step) power *= base;
  return power;
}

/// add(a, b) calls add and P₁¹ once, then S ∘ P₃³, P₃³, and S for each of the
/// b steps.
Natural AdditionCalls(Natural b) { return 2 + 3 * b; }

/// mul(a, b) calls mul and Z once, then the composition, the combination, two
/// projections, and add(m, a) for each of the b steps.
Natural MultiplicationCalls(Natural a, Natural b) {
  return 2 + b * (4 + AdditionCalls(a));
}

/// pow(x, y) calls pow, S ∘ Z, Z, and S once, then the same four parts as mul
/// plus mul(xᵏ, x) for each step k.
Natural PowerCalls(Natural x, Natural y) {
  Natural calls = 4;
  for (Natural step = 0; step < y; ++step) {
    calls += 4 + MultiplicationCalls(Power(x, step), x);
  }
  return calls;
}

template <std::size_t I, std::size_t N>
concept CanProject = requires { prf::Projection<I, N>(); };

template <typename First, typename Second>
concept CanCombine =
    requires(First first, Second second) { prf::Combination(first, second); };

template <typename Outer, typename Inner>
concept CanCompose =
    requires(Outer outer, Inner inner) { prf::Composition(outer, inner); };

template <typename Base, typename Step>
concept CanRecurse =
    requires(Base base, Step step) { prf::Recursion(base, step); };

static_assert(CanProject<1, 1> && CanProject<3, 3>);
static_assert(!CanProject<0, 1> && !CanProject<2, 1>);
static_assert(CanCombine<Fn<3>, Fn<3, 2>>);
static_assert(!CanCombine<Fn<1>, Fn<2>>);
static_assert(CanCompose<Fn<2>, Fn<3, 2>>);
static_assert(!CanCompose<Fn<1>, Fn<2, 2>>);
static_assert(CanRecurse<Fn<1>, Fn<3>>);
static_assert(!CanRecurse<Fn<1>, Fn<1>>);
static_assert(!CanRecurse<Fn<1, 2>, Fn<3>>);

static_assert(std::same_as<
              decltype(prf::Combination(
                  prf::Projection<2, 3>(),
                  prf::Projection<1, 3>()
              )),
              Fn<3, 2>>);
static_assert(
    std::same_as<
        decltype(prf::Composition(prf::Successor(), prf::Projection<2, 4>())),
        Fn<4>>
);
static_assert(std::same_as<
              decltype(prf::Recursion(prf::Zero(), prf::Projection<3, 3>())),
              Fn<2>>);

TEST_CASE("PRF basic functions return zero, x + 1, and one argument", "[prf]") {
  CHECK(Evaluate(prf::Zero(), {4}) == std::array<Natural, 1>{0});
  CHECK(Evaluate(prf::Successor(), {41}) == std::array<Natural, 1>{42});
  CHECK(Evaluate(prf::Projection<1, 2>(), {4, 9}) == std::array<Natural, 1>{4});
  CHECK(Evaluate(prf::Projection<2, 2>(), {4, 9}) == std::array<Natural, 1>{9});
}

TEST_CASE("PRF combination returns the outputs of both functions", "[prf]") {
  const auto pair =
      prf::Combination(prf::Projection<3, 3>(), prf::Projection<1, 3>());
  CHECK(Evaluate(pair, {7, 8, 9}) == std::array<Natural, 2>{9, 7});
  const auto triple = prf::Combination(pair, prf::Projection<2, 3>());
  CHECK(Evaluate(triple, {7, 8, 9}) == std::array<Natural, 3>{9, 7, 8});
}

TEST_CASE("PRF composition applies the outer function to a tuple", "[prf]") {
  const auto swap =
      prf::Combination(prf::Projection<2, 2>(), prf::Projection<1, 2>());
  const auto second = prf::Composition(prf::Projection<1, 2>(), swap);
  CHECK(Evaluate(second, {4, 9}) == std::array<Natural, 1>{9});
}

TEST_CASE("PRF recursion passes X, y, and f(X, y) to the step", "[prf]") {
  // With h = P₂³, f(x, S(y)) = y, while h = P₁³ gives f(x, S(y)) = x.
  // With h = P₃³ instead, f(x, S(y)) = f(x, y).
  const auto counter =
      prf::Recursion(prf::Projection<1, 1>(), prf::Projection<2, 3>());
  const auto fixed = prf::Recursion(prf::Zero(), prf::Projection<1, 3>());
  const auto previous =
      prf::Recursion(prf::Projection<1, 1>(), prf::Projection<3, 3>());
  CHECK(Evaluate(counter, {7, 0}) == std::array<Natural, 1>{7});
  CHECK(Evaluate(counter, {7, 4}) == std::array<Natural, 1>{3});
  CHECK(Evaluate(fixed, {7, 4}) == std::array<Natural, 1>{7});
  CHECK(Evaluate(previous, {7, 4}) == std::array<Natural, 1>{7});
}

TEST_CASE("PRF arithmetic agrees with C++ arithmetic", "[prf]") {
  const auto add = prf::Addition();
  const auto mul = prf::Multiplication();
  const auto pow = prf::Power();
  for (Natural x = 0; x <= 12; ++x) {
    for (Natural y = 0; y <= 12; ++y) {
      CAPTURE(x, y);
      CHECK(Evaluate(add, {x, y}) == std::array{x + y});
      CHECK(Evaluate(mul, {x, y}) == std::array{x * y});
      if (Power(x, y) <= 1'000'000) {
        CHECK(Evaluate(pow, {x, y}) == std::array{Power(x, y)});
      }
    }
  }
}

TEST_CASE("PRF power call counts match its recursive definition", "[prf]") {
  const auto pow = prf::Power();
  for (Natural x = 0; x <= 6; ++x) {
    for (Natural y = 0; y <= 6; ++y) {
      CAPTURE(x, y);
      prf::Trace trace({.mode = prf::TraceMode::kOff});
      REQUIRE(pow.Evaluate({x, y}, trace).has_value());
      CHECK(trace.Calls() == PowerCalls(x, y));
    }
  }
  prf::Trace trace({.mode = prf::TraceMode::kOff});
  REQUIRE(pow.Evaluate({2, 3}, trace).has_value());
  CHECK(trace.Calls() == 100);
}

TEST_CASE("PRF names use the notation of the course", "[prf][names]") {
  const auto one = prf::Composition(prf::Successor(), prf::Zero());
  const auto step = prf::Composition(
      prf::Multiplication(),
      prf::Combination(prf::Projection<3, 3>(), prf::Projection<1, 3>())
  );
  const auto unnamed =
      prf::Recursion(prf::Projection<1, 1>(), prf::Projection<3, 3>());
  CHECK(prf::Power().function().Name(Notation::kUnicode) == "pow");
  CHECK(one.function().Name(Notation::kUnicode) == "S ∘ Z");
  CHECK(one.function().Operand(Notation::kUnicode) == "(S ∘ Z)");
  CHECK(step.function().Name(Notation::kUnicode) == "mul ∘ (P₃³ × P₁³)");
  CHECK(step.function().Name(Notation::kAscii) == "mul o (P_3^3 x P_1^3)");
  CHECK(
      prf::Projection<12, 13>().function().Name(Notation::kUnicode) == "P₁₂¹³"
  );
  CHECK(unnamed.function().Name(Notation::kUnicode) == "R(P₁¹, P₃³)");
}

TEST_CASE("PRF successor reports an overflow", "[prf][validation]") {
  prf::Trace trace({.mode = prf::TraceMode::kTree});
  const auto successor = prf::Successor();
  const auto overflow =
      successor.Evaluate({std::numeric_limits<Natural>::max()}, trace);
  REQUIRE_FALSE(overflow.has_value());
  CHECK(
      overflow.error().message ==
      "S(18446744073709551615) does not fit in 64 bits"
  );
  // The failed call closes, so the next call is a new root.
  REQUIRE(successor.Evaluate({0}, trace).has_value());
  REQUIRE(trace.roots().size() == 2);
  CHECK_FALSE(trace.roots()[0]->results.has_value());
  CHECK(trace.roots()[1]->results == Tuple{1});
}

TEST_CASE("PRF traces count every call in each mode", "[prf][trace]") {
  const auto mode = GENERATE(
      prf::TraceMode::kOff,
      prf::TraceMode::kCounts,
      prf::TraceMode::kTree
  );
  CAPTURE(static_cast<int>(mode));
  const auto pow = prf::Power();
  prf::Trace trace({.mode = mode});
  const auto result = pow.Evaluate({2, 3}, trace);
  REQUIRE(result.has_value());
  CHECK(*result == std::array<Natural, 1>{8});
  CHECK(trace.Calls() == 100);
  CHECK(trace.HiddenCalls() == 0);
  Natural counted = 0;
  for (const auto& [function, count] : trace.counts()) counted += count;
  CHECK(counted == (mode == prf::TraceMode::kCounts ? 100 : 0));
  CHECK(trace.roots().empty() == (mode != prf::TraceMode::kTree));
}

TEST_CASE("PRF tree keeps the first calls and their callers", "[prf][trace]") {
  const auto pow = prf::Power();
  prf::Trace trace({.mode = prf::TraceMode::kTree, .tree_limit = 4});
  REQUIRE(pow.Evaluate({2, 3}, trace).has_value());
  CHECK(trace.Calls() == 100);
  CHECK(trace.HiddenCalls() == 96);
  // Keeping the callers of Z and S retains S ∘ Z and pow too.
  REQUIRE(trace.roots().size() == 1);
  const auto& root = *trace.roots().front();
  CHECK(root.results == Tuple{8});
  REQUIRE(root.children.size() == 1);
  CHECK(root.children.front()->children.size() == 2);
}

TEST_CASE("PRF evaluation stops at the call limit", "[prf][trace]") {
  const auto pow = prf::Power();
  prf::Trace trace({.mode = prf::TraceMode::kTree, .max_calls = 99});
  const auto result = pow.Evaluate({2, 3}, trace);
  REQUIRE_FALSE(result.has_value());
  CHECK(result.error().message == "the evaluation needs more than 99 calls");
  CHECK(trace.Calls() == 99);
  CHECK_FALSE(trace.roots().front()->results.has_value());

  prf::Trace enough({.mode = prf::TraceMode::kOff, .max_calls = 100});
  CHECK(pow.Evaluate({2, 3}, enough).has_value());
}

}  // namespace
