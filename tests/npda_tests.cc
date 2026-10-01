#include <array>
#include <cstddef>
#include <future>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "automata/symbol.h"
#include "npda/machine.h"

namespace {

using automata::Intern;
using automata::Symbol;

npda::Definition PushdownDefinition() {
  return {.start_state = Intern("q"),
          .stack_bottom = Intern("Z"),
          .accepting_states = {Intern("f")},
          .transitions = {
              {Intern("q"), Intern("a"), Intern("Z"), Intern("f"), {}}}};
}

TEST_CASE("NPDA acceptance follows the selected policy", "[npda][acceptance]") {
  const auto policy = GENERATE(
      npda::AcceptancePolicy::kFinalState, npda::AcceptancePolicy::kEmptyStack,
      npda::AcceptancePolicy::kBoth, npda::AcceptancePolicy::kAny);
  CAPTURE(npda::AcceptanceName(policy));
  auto definition = PushdownDefinition();
  definition.acceptance = policy;
  SECTION("An empty stack in a final state satisfies every policy") {
    const auto machine = npda::Machine::Create(definition);
    REQUIRE(machine.has_value());
    const auto result = machine->Run(automata::InputSymbols("a"));
    REQUIRE(result.has_value());
    CHECK(result->accepted);
  }
  SECTION(
      "An empty stack outside a final state only satisfies stack policies") {
    definition.accepting_states.clear();
    const auto machine = npda::Machine::Create(definition);
    REQUIRE(machine.has_value());
    const auto result = machine->Run(automata::InputSymbols("a"));
    REQUIRE(result.has_value());
    CHECK(result->accepted == (policy == npda::AcceptancePolicy::kEmptyStack ||
                               policy == npda::AcceptancePolicy::kAny));
  }
}

TEST_CASE("NPDA acceptance requires complete input", "[npda][acceptance]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  const auto input = GENERATE("", "aa");
  CAPTURE(input);
  const auto result = machine->Run(automata::InputSymbols(input));
  REQUIRE(result.has_value());
  CHECK_FALSE(result->accepted);
  CHECK_FALSE(result->witness.has_value());
}

TEST_CASE("NPDA accepts on the last permitted expansion", "[npda][limits]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  const auto input = automata::InputSymbols("a");
  const auto result = machine->Run(input, {.max_expansions = 1});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->expansions == 1);
  REQUIRE(result->witness.has_value());
  CHECK(*result->witness == std::vector<std::size_t>{0});
  CHECK_FALSE(machine->Run(input, {.max_expansions = 0}).has_value());
}

TEST_CASE("NPDA witness collection is optional", "[npda][witness]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  const auto result =
      machine->Run(automata::InputSymbols("a"), {.track_witness = false});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK_FALSE(result->witness.has_value());
}

TEST_CASE("NPDA rejects invalid symbols before execution",
          "[npda][validation]") {
  auto definition = PushdownDefinition();
  SECTION("The initial state must be valid") {
    definition.start_state = {};
    CHECK_FALSE(npda::Machine::Create(definition).has_value());
  }
  SECTION("Push symbols must be valid") {
    definition.transitions.front().push = {Symbol{}};
    CHECK_FALSE(npda::Machine::Create(definition).has_value());
  }
  SECTION("Input symbols must be valid") {
    const auto machine = npda::Machine::Create(definition);
    REQUIRE(machine.has_value());
    CHECK_FALSE(machine->Run(std::array{Symbol{}}).has_value());
  }
}

TEST_CASE("NPDA runs own independent execution state", "[npda][concurrency]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  const auto input = automata::InputSymbols("a");
  auto concurrent = std::async(
      std::launch::async, [&machine, &input] { return machine->Run(input); });
  const auto first = machine->Run(input);
  const auto second = concurrent.get();
  REQUIRE(first.has_value());
  REQUIRE(second.has_value());
  CHECK(first->accepted);
  CHECK(second->accepted);
  CHECK(first->witness == second->witness);
}

TEST_CASE("NPDA breadth-first search returns the shortest witness",
          "[npda][search]") {
  auto definition = PushdownDefinition();
  const auto state = Intern("q");
  const auto end = Intern("f");
  const auto middle = Intern("m");
  definition.transitions = {
      {state, std::nullopt, std::nullopt, state, {}},
      {state, std::nullopt, std::nullopt, end, {}},
      {state, std::nullopt, std::nullopt, middle, {}},
      {middle, std::nullopt, std::nullopt, end, {Intern("X")}}};
  const auto machine = npda::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto breadth = machine->Run({});
  const auto depth =
      machine->Run({}, {.search_order = npda::SearchOrder::kDepthFirst});
  REQUIRE(breadth.has_value());
  REQUIRE(depth.has_value());
  CHECK(breadth->accepted);
  CHECK(depth->accepted);
  REQUIRE(breadth->witness.has_value());
  REQUIRE(depth->witness.has_value());
  CHECK(*breadth->witness == std::vector<std::size_t>{1});
  CHECK(*depth->witness == std::vector<std::size_t>{2, 3});
}

TEST_CASE("NPDA pushes the first symbol as the new stack top",
          "[npda][stack]") {
  auto definition = PushdownDefinition();
  const auto bottom = Intern("Z");
  definition.transitions = {
      {Intern("q"), std::nullopt, bottom, Intern("m"), {Intern("X"), bottom}},
      {Intern("m"), Intern("a"), Intern("X"), Intern("f"), {}}};
  const auto machine = npda::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->stack_depth == 1);
}

TEST_CASE("NPDA epsilon cycles terminate and count revisits",
          "[npda][search]") {
  auto definition = PushdownDefinition();
  definition.accepting_states.clear();
  definition.transitions = {
      {Intern("q"), std::nullopt, std::nullopt, Intern("q"), {}}};
  const auto machine = npda::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run({}, {.max_expansions = 1});
  REQUIRE(result.has_value());
  CHECK_FALSE(result->accepted);
  CHECK(result->expansions == 1);
}

TEST_CASE("NPDA handles a large epsilon frontier before a consuming transition",
          "[npda][search][regression]") {
  auto definition = PushdownDefinition();
  definition.transitions.clear();
  for (std::size_t i = 0; i < 65538; ++i) {
    definition.transitions.push_back({Intern("q"),
                                      std::nullopt,
                                      std::nullopt,
                                      Intern("wide-" + std::to_string(i)),
                                      {}});
  }
  definition.transitions.push_back(
      {Intern("q"), Intern("a"), Intern("Z"), Intern("f"), {}});
  const auto machine = npda::Machine::Create(std::move(definition));
  REQUIRE(machine.has_value());
  std::size_t events = 0;
  bool valid_node_indices = true;
  const auto result = machine->Run(
      automata::InputSymbols("a"),
      {.max_expansions = 65539,
       .observer = [&](const npda::ExecutionEvent& event) {
         valid_node_indices &= event.node_index < event.nodes.size();
         ++events;
       }});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  REQUIRE(result->witness.has_value());
  CHECK(*result->witness == std::vector<std::size_t>{65538});
  CHECK(valid_node_indices);
  CHECK(events > 65538);
}

TEST_CASE("NPDA observer nodes have complete predecessor edges",
          "[npda][observer]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  std::vector<npda::EventKind> events;
  const auto result =
      machine->Run(automata::InputSymbols("a"),
                   {.observer = [&](const npda::ExecutionEvent& event) {
                     REQUIRE(event.node_index < event.nodes.size());
                     const auto& node = event.nodes[event.node_index];
                     if (node.predecessor) {
                       CHECK(node.predecessor->node_index < event.node_index);
                       CHECK(node.predecessor->transition_index == 0);
                     } else {
                       CHECK(event.node_index == 0);
                     }
                     events.push_back(event.kind);
                   }});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(events == std::vector{npda::EventKind::kStep, npda::EventKind::kStep,
                              npda::EventKind::kAccepted});
}

TEST_CASE("NPDA accepts an initial configuration with no expansion budget",
          "[npda][limits][witness]") {
  auto definition = PushdownDefinition();
  definition.accepting_states.push_back(definition.start_state);
  const auto machine = npda::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run({}, {.max_expansions = 0});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->expansions == 0);
  REQUIRE(result->witness.has_value());
  CHECK(result->witness->empty());
}

TEST_CASE("NPDA ignores stack mismatches without using expansion budget",
          "[npda][limits]") {
  auto definition = PushdownDefinition();
  definition.transitions.front().stack_top = Intern("X");
  const auto machine = npda::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result =
      machine->Run(automata::InputSymbols("a"), {.max_expansions = 0});
  REQUIRE(result.has_value());
  CHECK_FALSE(result->accepted);
  CHECK(result->expansions == 0);
}

TEST_CASE("NPDA options stay fixed throughout a run", "[npda][observer]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  npda::RunOptions options{.max_expansions = 1};
  options.observer = [&](const npda::ExecutionEvent&) {
    options.max_expansions = 0;
    options.track_witness = false;
  };
  const auto result = machine->Run(automata::InputSymbols("a"), options);
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  REQUIRE(result->witness.has_value());
  CHECK(*result->witness == std::vector<std::size_t>{0});
}

TEST_CASE("NPDA rejects an invalid search order at the public interface",
          "[npda][validation]") {
  const auto machine = npda::Machine::Create(PushdownDefinition());
  REQUIRE(machine.has_value());
  const auto result =
      machine->Run({}, {.search_order = static_cast<npda::SearchOrder>(-1)});
  REQUIRE_FALSE(result.has_value());
  CHECK(result.error().message == "invalid search order");
}

}  // namespace
