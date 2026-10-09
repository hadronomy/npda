#include <cstddef>
#include <string>
#include <vector>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include "automata/symbol.h"
#include "turing/config.h"
#include "turing/definition.h"
#include "turing/machine.h"
#include "turing/run.h"
#include "turing/tape.h"
#include "turing/transition.h"

namespace {

using automata::Intern;

/// Writes x on the 'a' below the head, moves left, and accepts.
turing::Definition SingleRule() {
  return {
      .config = {},
      .start_state = Intern("q"),
      .blank = Intern("_"),
      .accepting_states = {Intern("f")},
      .transitions = {
          {.from = Intern("q"),
           .tapes =
               {{.read = Intern("a"),
                 .write = Intern("x"),
                 .move = turing::HeadMove::kLeft}},
           .to = Intern("f")}
      }
  };
}

std::string Cells(const turing::Tape& tape) {
  std::string text;
  for (const auto symbol : tape.cells) text += symbol.name();
  return text;
}

TEST_CASE("Turing execution grows a tape to the left", "[turing][tape]") {
  const auto machine = turing::Machine::Create(SingleRule());
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(result->steps == 1);
  CHECK(Cells(result->halted.tapes.front()) == "_x");
  CHECK(result->halted.tapes.front().head == 0);
}

TEST_CASE("Turing counts steps against the limit", "[turing][limits]") {
  const auto machine = turing::Machine::Create(SingleRule());
  REQUIRE(machine.has_value());
  const auto input = automata::InputSymbols("a");
  CHECK(machine->Run(input, {.max_steps = 1})->accepted);
  const auto limited = machine->Run(input, {.max_steps = 0});
  REQUIRE_FALSE(limited.has_value());
  CHECK(limited.error().steps == 0);
}

TEST_CASE("Turing halts without an applicable rule", "[turing][execution]") {
  const auto machine = turing::Machine::Create(SingleRule());
  REQUIRE(machine.has_value());
  const auto result =
      machine->Run(automata::InputSymbols("b"), {.max_steps = 0});
  REQUIRE(result.has_value());
  CHECK_FALSE(result->accepted);
  CHECK(result->steps == 0);
  CHECK(Cells(result->halted.tapes.front()) == "b");
}

TEST_CASE(
    "Turing observers see every configuration in order",
    "[turing][observer]"
) {
  const auto machine = turing::Machine::Create(SingleRule());
  REQUIRE(machine.has_value());
  std::vector<std::size_t> numbers;
  std::vector<std::size_t> transitions;
  const auto result = machine->Run(
      automata::InputSymbols("a"),
      {.observer = [&](const turing::Step& step) {
        numbers.push_back(step.number);
        if (step.transition) transitions.push_back(*step.transition);
      }}
  );
  REQUIRE(result.has_value());
  CHECK(numbers == std::vector<std::size_t>{0, 1});
  CHECK(transitions == std::vector<std::size_t>{0});
}

TEST_CASE(
    "Turing construction rejects invalid configurations and rules",
    "[turing][validation]"
) {
  auto definition = SingleRule();
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
    definition.transitions.front().tapes.front().move = turing::HeadMove::kStay;
  }
  SECTION("Each transition needs one action per tape") {
    definition.config.num_tapes = 2;
  }
  CHECK_FALSE(turing::Machine::Create(definition).has_value());
}

TEST_CASE(
    "Turing multi-tape execution writes and moves each tape",
    "[turing][tape]"
) {
  const auto mode = GENERATE(
      turing::OperationMode::kIndependent,
      turing::OperationMode::kSimultaneous
  );
  CAPTURE(static_cast<int>(mode));
  auto definition = SingleRule();
  definition.config.num_tapes = 2;
  definition.config.operation_mode = mode;
  definition.transitions.front().tapes.push_back(
      {.read = Intern("_"),
       .write = Intern("b"),
       .move = turing::HeadMove::kRight}
  );
  const auto machine = turing::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  REQUIRE(result->halted.tapes.size() == 2);
  CHECK(Cells(result->halted.tapes[1]) == "b");
  CHECK(result->halted.tapes[1].head == 1);
}

TEST_CASE("Turing reads blank cells beyond the stored tape", "[turing][tape]") {
  auto definition = SingleRule();
  definition.transitions = {
      {.from = Intern("q"),
       .tapes =
           {{.read = Intern("a"),
             .write = Intern("a"),
             .move = turing::HeadMove::kRight}},
       .to = Intern("m")},
      {.from = Intern("m"),
       .tapes =
           {{.read = Intern("_"),
             .write = Intern("b"),
             .move = turing::HeadMove::kStay}},
       .to = Intern("f")}
  };
  const auto machine = turing::Machine::Create(definition);
  REQUIRE(machine.has_value());
  const auto result = machine->Run(automata::InputSymbols("a"));
  REQUIRE(result.has_value());
  CHECK(result->accepted);
  CHECK(Cells(result->halted.tapes.front()) == "ab");
  CHECK(result->halted.tapes.front().head == 1);
}

}  // namespace
