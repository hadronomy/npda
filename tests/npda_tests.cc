#include <cstddef>
#include <future>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "automata/symbol.h"
#include "npda/definition.h"
#include "npda/machine.h"
#include "npda/run.h"

namespace {

using automata::Intern;
using automata::Symbol;

/// Reads one 'a' with Z on top of the stack, pops Z, and goes to f.
npda::Definition SingleMove(npda::Acceptance acceptance) {
  return {
      .start_state = Intern("q"),
      .initial_stack_symbol = Intern("Z"),
      .acceptance = std::move(acceptance),
      .transitions = {
          {.from = Intern("q"),
           .input = Intern("a"),
           .top = Intern("Z"),
           .to = Intern("f"),
           .push = {}}
      }
  };
}

npda::Definition FinalState(std::vector<Symbol> states = {Intern("f")}) {
  return SingleMove(npda::AcceptByFinalState{std::move(states)});
}

TEST_CASE("NPDA acceptance follows the selected mode", "[npda][acceptance]") {
  SECTION("Final state accepts in F after the whole input") {
    const npda::Machine machine(FinalState());
    CHECK(machine.Run(automata::InputSymbols("a"))->Accepted());
  }
  SECTION("Final state ignores an empty stack outside F") {
    const npda::Machine machine(FinalState({}));
    CHECK_FALSE(machine.Run(automata::InputSymbols("a"))->Accepted());
  }
  SECTION("Empty stack accepts in any state") {
    const npda::Machine machine(SingleMove(npda::AcceptByEmptyStack{}));
    CHECK(machine.Run(automata::InputSymbols("a"))->Accepted());
  }
}

TEST_CASE("NPDA acceptance requires the whole input", "[npda][acceptance]") {
  const npda::Machine machine(FinalState());
  const auto input = GENERATE("", "aa");
  const auto result = machine.Run(automata::InputSymbols(input));
  REQUIRE(result.has_value());
  CHECK_FALSE(result->Accepted());
}

TEST_CASE("NPDA counts expansions against the limit", "[npda][limits]") {
  const npda::Machine machine(FinalState());
  const auto input = automata::InputSymbols("a");
  const auto result = machine.Run(input, {.max_expansions = 1});
  REQUIRE(result.has_value());
  CHECK(result->accepting_path == std::vector<std::size_t>{0});
  CHECK(result->expansions == 1);
  const auto limited = machine.Run(input, {.max_expansions = 0});
  REQUIRE_FALSE(limited.has_value());
  CHECK(limited.error().expansions == 0);
}

TEST_CASE(
    "NPDA accepts a start configuration with no budget",
    "[npda][limits]"
) {
  const npda::Machine machine(FinalState({Intern("q")}));
  const auto result = machine.Run({}, {.max_expansions = 0});
  REQUIRE(result.has_value());
  CHECK(result->accepting_path == std::vector<std::size_t>{});
}

TEST_CASE("NPDA skips moves whose top is not on the stack", "[npda][stack]") {
  auto definition = FinalState();
  definition.transitions.front().top = Intern("X");
  const npda::Machine machine(definition);
  const auto result =
      machine.Run(automata::InputSymbols("a"), {.max_expansions = 0});
  REQUIRE(result.has_value());
  CHECK_FALSE(result->Accepted());
  CHECK(result->expansions == 0);
}

TEST_CASE("NPDA pushes the first symbol as the new top", "[npda][stack]") {
  auto definition = FinalState();
  const auto bottom = Intern("Z");
  const auto marker = Intern("X");
  definition.transitions = {
      {.from = Intern("q"),
       .input = std::nullopt,
       .top = bottom,
       .to = Intern("m"),
       .push = {marker, bottom}},
      {.from = Intern("m"),
       .input = Intern("a"),
       .top = marker,
       .to = Intern("f"),
       .push = {}}
  };
  const npda::Machine machine(definition);
  const auto result = machine.Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepting_path == std::vector<std::size_t>{0, 1});
}

TEST_CASE("NPDA finds the shortest accepting path", "[npda][search]") {
  auto definition = FinalState();
  const auto start = Intern("q");
  const auto bottom = Intern("Z");
  const auto middle = Intern("m");
  definition.transitions = {
      {.from = start,
       .input = std::nullopt,
       .top = bottom,
       .to = middle,
       .push = {bottom}},
      {.from = middle,
       .input = std::nullopt,
       .top = bottom,
       .to = Intern("f"),
       .push = {bottom}},
      {.from = start,
       .input = std::nullopt,
       .top = bottom,
       .to = Intern("f"),
       .push = {bottom}}
  };
  const npda::Machine machine(definition);
  const auto result = machine.Run({});
  REQUIRE(result.has_value());
  CHECK(result->accepting_path == std::vector<std::size_t>{2});
}

TEST_CASE(
    "NPDA ε-cycles that grow the stack stop at the limit",
    "[npda][search]"
) {
  auto definition = FinalState({});
  const auto bottom = Intern("Z");
  definition.transitions = {
      {.from = Intern("q"),
       .input = std::nullopt,
       .top = bottom,
       .to = Intern("q"),
       .push = {bottom, bottom}}
  };
  const npda::Machine machine(definition);
  const auto result = machine.Run({}, {.max_expansions = 50});
  REQUIRE_FALSE(result.has_value());
  CHECK(result.error().expansions == 50);
}

TEST_CASE(
    "NPDA ε-cycles that revisit configurations terminate",
    "[npda][search]"
) {
  auto definition = FinalState({});
  const auto bottom = Intern("Z");
  definition.transitions = {
      {.from = Intern("q"),
       .input = std::nullopt,
       .top = bottom,
       .to = Intern("q"),
       .push = {bottom}}
  };
  const npda::Machine machine(definition);
  const auto result = machine.Run({});
  REQUIRE(result.has_value());
  CHECK_FALSE(result->Accepted());
  CHECK(result->expansions == 1);
}

TEST_CASE(
    "NPDA visits report origins and applicable moves",
    "[npda][observer]"
) {
  auto definition = FinalState();
  const auto start = Intern("q");
  const auto bottom = Intern("Z");
  definition.transitions = {
      {.from = start,
       .input = Intern("a"),
       .top = bottom,
       .to = Intern("f"),
       .push = {}},
      {.from = start,
       .input = Intern("b"),
       .top = bottom,
       .to = Intern("f"),
       .push = {}},
      {.from = start,
       .input = std::nullopt,
       .top = bottom,
       .to = Intern("d"),
       .push = {bottom}},
      {.from = start,
       .input = Intern("a"),
       .top = Intern("Y"),
       .to = Intern("f"),
       .push = {}}
  };
  const npda::Machine machine(definition);
  std::vector<std::vector<npda::Move>> moves;
  const auto result = machine.Run(
      automata::InputSymbols("a"),
      {.observer = [&](const npda::Visit& visit) {
        CHECK(visit.id == moves.size());
        if (visit.origin) CHECK(visit.origin->parent < visit.id);
        moves.emplace_back(visit.moves.begin(), visit.moves.end());
      }}
  );
  REQUIRE(result.has_value());
  CHECK(result->Accepted());
  REQUIRE(moves.size() == 3);
  REQUIRE(moves[0].size() == 2);
  CHECK(moves[0][0].transition == 2);
  CHECK(moves[0][0].target == 1);
  CHECK(moves[0][1].transition == 0);
  CHECK(moves[0][1].target == 2);
  CHECK(moves[1].empty());
  CHECK(moves[2].empty());
}

TEST_CASE("NPDA moves name configurations reached before", "[npda][observer]") {
  auto definition = FinalState();
  const auto start = Intern("q");
  const auto bottom = Intern("Z");
  definition.transitions = {
      {.from = start,
       .input = std::nullopt,
       .top = bottom,
       .to = start,
       .push = {bottom}},
  };
  const npda::Machine machine(definition);
  std::vector<npda::Move> moves;
  const auto result = machine.Run(
      automata::InputSymbols("a"),
      {.observer = [&](const npda::Visit& visit) {
        moves.insert(moves.end(), visit.moves.begin(), visit.moves.end());
      }}
  );
  REQUIRE(result.has_value());
  CHECK_FALSE(result->Accepted());
  REQUIRE(moves.size() == 1);
  CHECK(moves[0].target == 0);
  CHECK(moves[0].repeated);
}

TEST_CASE("NPDA runs are independent", "[npda][concurrency]") {
  const npda::Machine machine(FinalState());
  const auto input = automata::InputSymbols("a");
  auto concurrent =
      std::async(std::launch::async, [&] { return machine.Run(input); });
  const auto first = machine.Run(input);
  const auto second = concurrent.get();
  REQUIRE(first.has_value());
  REQUIRE(second.has_value());
  CHECK(first->accepting_path == second->accepting_path);
}

TEST_CASE("NPDA handles a wide ε frontier", "[npda][search][regression]") {
  auto definition = FinalState();
  const auto start = Intern("q");
  const auto bottom = Intern("Z");
  definition.transitions.clear();
  for (std::size_t i = 0; i < 65538; ++i) {
    definition.transitions.push_back(
        {.from = start,
         .input = std::nullopt,
         .top = bottom,
         .to = Intern("wide-" + std::to_string(i)),
         .push = {bottom}}
    );
  }
  definition.transitions.push_back(
      {.from = start,
       .input = Intern("a"),
       .top = bottom,
       .to = Intern("f"),
       .push = {}}
  );
  const npda::Machine machine(std::move(definition));
  const auto result =
      machine.Run(automata::InputSymbols("a"), {.max_expansions = 65539});
  REQUIRE(result.has_value());
  CHECK(result->accepting_path == std::vector<std::size_t>{65538});
}

}  // namespace
