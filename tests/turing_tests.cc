#include <cstddef>
#include <string>
#include <vector>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "automata/symbol.h"
#include "turing/machine.h"

namespace {

using automata::Intern;

turing::Definition TuringDefinition() {
  return {
      .config = {},
      .start_state = Intern("q"),
      .blank_symbol = Intern("_"),
      .accepting_states = {Intern("f")},
      .transitions = {{Intern("q"),
                       {{Intern("a"), Intern("x"), turing::Direction::kLeft}},
                       Intern("f")}}};
}

TEST_CASE("Turing execution grows a tape to the left", "[turing][tape]") {
  const auto machine = turing::Machine::Create(TuringDefinition());
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->steps == 1);
  CHECK(result->final_tapes ==
        std::vector<std::vector<std::string>>{{"_", "x"}});
  CHECK(result->final_head_positions == std::vector<std::size_t>{0});
  REQUIRE(result->witness.has_value());
  CHECK(*result->witness == std::vector<std::size_t>{0});
}

TEST_CASE("Turing accepts on the last permitted step", "[turing][limits]") {
  const auto machine = turing::Machine::Create(TuringDefinition());
  REQUIRE(machine.has_value());
  const auto input = automata::InputSymbols("a");
  const auto result = machine->Run(input, {.max_steps = 1});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->steps == 1);
  CHECK_FALSE(machine->Run(input, {.max_steps = 0}).has_value());
}

TEST_CASE("Turing rejects input without an applicable rule",
          "[turing][execution]") {
  const auto machine = turing::Machine::Create(TuringDefinition());
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("b"));
  REQUIRE(result.has_value());
  CHECK_FALSE(result->accepted);
  CHECK(result->steps == 0);
  CHECK_FALSE(result->witness.has_value());
}

TEST_CASE("Turing observers receive current tapes without witness history",
          "[turing][observer]") {
  const auto machine = turing::Machine::Create(TuringDefinition());
  REQUIRE(machine.has_value());
  std::size_t events = 0;
  const auto result =
      machine->Run(automata::InputSymbols("a"),
                   {.track_witness = false,
                    .observer = [&](const turing::ExecutionEvent& event) {
                      CHECK(event.nodes.size() == 1);
                      CHECK(event.node_index == 0);
                      CHECK_FALSE(event.track_witness);
                      ++events;
                    }});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK_FALSE(result->witness.has_value());
  CHECK(result->final_tapes ==
        std::vector<std::vector<std::string>>{{"_", "x"}});
  CHECK(events == 3);
}

TEST_CASE("Turing construction rejects invalid configurations and rules",
          "[turing][validation]") {
  auto definition = TuringDefinition();
  SECTION("Right-only tapes cannot move left") {
    definition.config.tape_direction = turing::TapeDirection::kRightOnly;
  }
  SECTION("A machine needs at least one tape") {
    definition.config.num_tapes = 0;
  }
  SECTION("Read patterns must be deterministic") {
    definition.transitions.push_back(definition.transitions.front());
  }
  SECTION("Stay movement requires allow_stay") {
    definition.config.allow_stay = false;
    definition.transitions.front().tapes.front().move =
        turing::Direction::kStay;
  }
  SECTION("Each transition needs one entry per tape") {
    definition.config.num_tapes = 2;
  }
  CHECK_FALSE(turing::Machine::Create(definition).has_value());
}

TEST_CASE("Turing multi-tape execution writes and moves each tape",
          "[turing][tape]") {
  const auto mode = GENERATE(turing::OperationMode::kIndependent,
                             turing::OperationMode::kSimultaneous);
  CAPTURE(static_cast<int>(mode));
  auto definition = TuringDefinition();
  definition.config.num_tapes = 2;
  definition.config.operation_mode = mode;
  definition.transitions.front().tapes.push_back(
      {Intern("_"), Intern("b"), turing::Direction::kRight});
  const auto machine = turing::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  REQUIRE(result->final_tapes.size() == 2);
  REQUIRE(result->final_head_positions.size() == 2);
  CHECK(result->final_tapes[1] == std::vector<std::string>{"b"});
  CHECK(result->final_head_positions[1] == 1);
}

TEST_CASE("Turing reads blank cells beyond stored tape before extending it",
          "[turing][tape]") {
  auto definition = TuringDefinition();
  definition.transitions = {
      {Intern("q"),
       {{Intern("a"), Intern("a"), turing::Direction::kRight}},
       Intern("m")},
      {Intern("m"),
       {{Intern("_"), Intern("b"), turing::Direction::kStay}},
       Intern("f")}};
  const auto machine = turing::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run(
      automata::InputSymbols("a"),
      {.observer = [&](const turing::ExecutionEvent& event) {
        REQUIRE(event.node_index < event.nodes.size());
        const auto& node = event.nodes[event.node_index];
        REQUIRE(node.tapes.size() == 1);
        if (event.steps == 1) {
          CHECK(node.tapes.front().head_position == 1);
          CHECK(node.tapes.front().cells.size() == 1);
        }
        if (node.predecessor) {
          CHECK(node.predecessor->node_index + 1 == event.node_index);
          CHECK(node.predecessor->transition_index <
                definition.transitions.size());
        } else {
          CHECK(event.node_index == 0);
        }
      }});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->final_tapes ==
        std::vector<std::vector<std::string>>{{"a", "b"}});
  CHECK(result->final_head_positions == std::vector<std::size_t>{1});
}

TEST_CASE("Turing accepts a recorded root with no step budget",
          "[turing][limits][observer]") {
  auto definition = TuringDefinition();
  definition.accepting_states.push_back(definition.start_state);
  const auto machine = turing::Machine::Create(definition);
  REQUIRE(machine.has_value());
  std::vector<turing::EventKind> events;
  const auto result = machine->Run(
      {},
      {.max_steps = 0, .observer = [&](const turing::ExecutionEvent& event) {
         REQUIRE(event.nodes.size() == 1);
         CHECK(event.node_index == 0);
         CHECK_FALSE(event.nodes.front().predecessor.has_value());
         events.push_back(event.kind);
       }});
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->steps == 0);
  REQUIRE(result->witness.has_value());
  CHECK(result->witness->empty());
  CHECK(events ==
        std::vector{turing::EventKind::kStep, turing::EventKind::kAccepted});
}

TEST_CASE("Turing options stay fixed throughout a run", "[turing][observer]") {
  const auto machine = turing::Machine::Create(TuringDefinition());
  REQUIRE(machine.has_value());
  turing::RunOptions options{.max_steps = 1};
  options.observer = [&](const turing::ExecutionEvent& event) {
    CHECK(event.track_witness);
    options.max_steps = 0;
    options.track_witness = false;
  };
  const auto result = machine->Run(automata::InputSymbols("a"), options);
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  REQUIRE(result->witness.has_value());
  CHECK(*result->witness == std::vector<std::size_t>{0});
}

}  // namespace
