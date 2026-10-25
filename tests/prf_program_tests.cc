#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "prf/function.h"
#include "prf/program.h"
#include "prf/trace.h"

namespace {

using prf::Fn;
using prf::Natural;
using prf::TraceMode;
using prf::TraceOptions;

bool SameCall(const prf::TraceNode& left, const prf::TraceNode& right) {
  if (left.function != right.function || left.arguments != right.arguments ||
      left.results != right.results ||
      left.children.size() != right.children.size()) {
    return false;
  }
  for (std::size_t index = 0; index < left.children.size(); ++index) {
    if (!SameCall(*left.children[index], *right.children[index])) return false;
  }
  return true;
}

/// Compares results and errors as well as call counts and trees between the
/// object evaluator and the compiled program.
template <std::size_t N, std::size_t M>
void CheckSameCalls(
    const Fn<N, M>& function,
    const std::array<Natural, N>& arguments,
    TraceOptions options
) {
  prf::Trace interpreted(options);
  prf::Trace compiled(options);
  const auto expected = function.Evaluate(arguments, interpreted);
  const auto actual = prf::Program(function).Evaluate(arguments, compiled);
  REQUIRE(expected.has_value() == actual.has_value());
  if (expected) {
    CHECK(*expected == *actual);
  } else {
    CHECK(expected.error().message == actual.error().message);
  }
  CHECK(interpreted.Calls() == compiled.Calls());
  CHECK(interpreted.HiddenCalls() == compiled.HiddenCalls());
  CHECK(interpreted.counts() == compiled.counts());
  REQUIRE(interpreted.roots().size() == compiled.roots().size());
  for (std::size_t index = 0; index < interpreted.roots().size(); ++index) {
    CHECK(SameCall(*interpreted.roots()[index], *compiled.roots()[index]));
  }
}

TEST_CASE(
    "PRF programs preserve the object evaluator's calls",
    "[prf][program]"
) {
  const auto mode =
      GENERATE(TraceMode::kOff, TraceMode::kCounts, TraceMode::kTree);
  CAPTURE(static_cast<int>(mode));
  const auto add = prf::Addition();
  const auto mul = prf::Multiplication();
  const auto pow = prf::Power();
  for (Natural x = 0; x <= 5; ++x) {
    for (Natural y = 0; y <= 5; ++y) {
      CAPTURE(x, y);
      CheckSameCalls(add, {x, y}, {.mode = mode});
      CheckSameCalls(mul, {x, y}, {.mode = mode});
      CheckSameCalls(pow, {x, y}, {.mode = mode});
    }
  }
}

TEST_CASE(
    "PRF programs match the object evaluator at limits and overflow",
    "[prf][program]"
) {
  const auto mode =
      GENERATE(TraceMode::kOff, TraceMode::kCounts, TraceMode::kTree);
  CAPTURE(static_cast<int>(mode));
  const auto pow = prf::Power();
  SECTION("Call limits include failure before the first call") {
    for (const std::uint64_t limit : {0, 1, 2, 57, 99, 100, 101}) {
      CAPTURE(limit);
      CheckSameCalls(pow, {2, 3}, {.mode = mode, .max_calls = limit});
    }
  }
  SECTION("Tree limits preserve the same calls") {
    for (const std::size_t limit : {0, 1, 4, 50, 99, 100, 1000}) {
      CAPTURE(limit);
      CheckSameCalls(pow, {2, 3}, {.mode = mode, .tree_limit = limit});
    }
  }
  SECTION("S reports the same overflow") {
    CheckSameCalls(
        prf::Successor(),
        {std::numeric_limits<Natural>::max()},
        {.mode = mode}
    );
  }
}

TEST_CASE(
    "PRF programs handle tuples and calls that need a heap buffer",
    "[prf][program]"
) {
  const auto mode =
      GENERATE(TraceMode::kOff, TraceMode::kCounts, TraceMode::kTree);
  CAPTURE(static_cast<int>(mode));
  const auto pair =
      prf::Combination(prf::Projection<3, 3>(), prf::Projection<1, 3>());
  CheckSameCalls(pair, {7, 8, 9}, {.mode = mode});

  // The composition passes five values, while the recursion step takes six
  // arguments, so both exceed the four values that fit on the stack.
  const auto five = prf::Combination(
      prf::Combination(prf::Projection<1, 1>(), prf::Successor()),
      prf::Combination(
          prf::Zero(),
          prf::Combination(prf::Successor(), prf::Projection<1, 1>())
      )
  );
  CheckSameCalls(
      prf::Composition(prf::Projection<4, 5>(), five),
      {6},
      {.mode = mode}
  );
  const auto wide = prf::Recursion(
      prf::Projection<2, 4>(),
      prf::Composition(prf::Successor(), prf::Projection<6, 6>()),
      "wide"
  );
  CheckSameCalls(wide, {1, 2, 3, 4, 5}, {.mode = mode});
}

TEST_CASE(
    "PRF programs preserve counts and trees across evaluations",
    "[prf][program]"
) {
  const auto mode =
      GENERATE(TraceMode::kOff, TraceMode::kCounts, TraceMode::kTree);
  CAPTURE(static_cast<int>(mode));
  const auto add = prf::Addition();
  const prf::Program program(add);
  prf::Trace interpreted({.mode = mode, .max_calls = 30, .tree_limit = 12});
  prf::Trace compiled({.mode = mode, .max_calls = 30, .tree_limit = 12});
  // These evaluations share a trace, so the second reaches the call limit
  // with counts and a tree from the first.
  for (const auto& arguments :
       {std::array<Natural, 2>{1, 3}, std::array<Natural, 2>{2, 9}}) {
    const auto expected = add.Evaluate(arguments, interpreted);
    const auto actual = program.Evaluate(arguments, compiled);
    REQUIRE(expected.has_value() == actual.has_value());
  }
  CHECK(interpreted.Calls() == compiled.Calls());
  CHECK(interpreted.HiddenCalls() == compiled.HiddenCalls());
  CHECK(interpreted.counts() == compiled.counts());
  REQUIRE(interpreted.roots().size() == compiled.roots().size());
  for (std::size_t index = 0; index < interpreted.roots().size(); ++index) {
    CHECK(SameCall(*interpreted.roots()[index], *compiled.roots()[index]));
  }
}

}  // namespace
