#include <algorithm>
#include <cstddef>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>

#include "automata/symbol.h"
#include "diagnostics/source.h"
#include "support/files.h"
#include "terminal/style.h"
#include "turing/machine.h"
#include "turing/parser.h"
#include "turing/trace.h"

namespace {

const terminal::Style kPlain = terminal::Style::Plain();

turing::Machine Load(std::string_view name) {
  const auto path = test_support::SourceRoot() / "examples/turing" / name;
  const auto parsed = turing::Parse(
      diag::SourceFile::From(path.string(), test_support::ReadFile(path))
  );
  REQUIRE(parsed.diagnostics.items.empty());
  REQUIRE(parsed.machine.has_value());
  return *parsed.machine;
}

/// All of the words over `alphabet` with `max_length` symbols or fewer,
/// shortest first.
std::vector<std::string> Words(
    std::string_view alphabet,
    std::size_t max_length
) {
  std::vector<std::string> words{""};
  for (std::size_t start = 0; start < words.size(); ++start) {
    if (words[start].size() == max_length) continue;
    for (const char symbol : alphabet) words.push_back(words[start] + symbol);
  }
  return words;
}

struct Outcome {
  bool accepted;
  /// The first tape in the form of the course, `ab[c]d`.
  std::string tape;
};

Outcome Run(const turing::Machine& machine, const std::string& word) {
  const auto result = machine.Run(automata::InputSymbols(word));
  REQUIRE(result.has_value());
  return {
      .accepted = result->accepted,
      .tape = turing::FormatTape(
          result->halted.tapes.front(),
          machine.definition().blank,
          kPlain
      ),
  };
}

/// n in the code of machine 2: 1ⁿ, or 0 for zero.
std::string Unary(std::size_t count) {
  return count == 0 ? "0" : std::string(count, '1');
}

TEST_CASE(
    "Machine 1 accepts exactly aⁿbᵐ with m ≥ n and n > 0",
    "[turing][designs]"
) {
  const auto machine = Load("anbm.turing");
  for (const auto& word : Words("ab", 12)) {
    CAPTURE(word);
    const auto n = word.find_first_not_of('a');
    const auto a_count = n == std::string::npos ? word.size() : n;
    const auto rest = std::string_view(word).substr(a_count);
    const bool expected =
        a_count > 0 &&
        std::ranges::all_of(rest, [](char c) { return c == 'b'; }) &&
        rest.size() >= a_count;
    CHECK(Run(machine, word).accepted == expected);
  }
}

TEST_CASE(
    "Machine 2 writes the count of b, a blank, and the count of a",
    "[turing][designs]"
) {
  const auto machine = Load("count_replace.turing");
  for (const auto& word : Words("ab", 10)) {
    CAPTURE(word);
    const auto result =
        Unary(static_cast<std::size_t>(std::ranges::count(word, 'b'))) + "_" +
        Unary(static_cast<std::size_t>(std::ranges::count(word, 'a')));
    const auto outcome = Run(machine, word);
    CHECK(outcome.accepted);
    // The statement asks for the head on the first symbol of the result.
    CHECK(outcome.tape == "[" + result.substr(0, 1) + "]" + result.substr(1));
  }
}

TEST_CASE("Machine 2 gives the results of the statement", "[turing][designs]") {
  const auto machine = Load("count_replace.turing");
  CHECK(Run(machine, "abbabaabb").tape == "[1]1111_1111");
  CHECK(Run(machine, "aa").tape == "[0]_11");
  CHECK(Run(machine, "bb").tape == "[1]1_0");
}

TEST_CASE(
    "The other example machines do what their comments say",
    "[turing][designs]"
) {
  SECTION("example_1 accepts an odd number of 0") {
    const auto machine = Load("example_1.turing");
    for (const auto& word : Words("01", 10)) {
      CAPTURE(word);
      CHECK(
          Run(machine, word).accepted ==
          (std::ranges::count(word, '0') % 2 == 1)
      );
    }
  }
  SECTION("palindrome accepts the palindromes") {
    const auto machine = Load("palindrome.turing");
    for (const auto& word : Words("ab", 10)) {
      CAPTURE(word);
      const bool expected =
          std::ranges::equal(word, word | std::views::reverse);
      CHECK(Run(machine, word).accepted == expected);
    }
  }
  SECTION("successor adds one in unary") {
    const auto machine = Load("successor.turing");
    for (const auto& word : Words("1", 12)) {
      CAPTURE(word);
      const auto outcome = Run(machine, word);
      CHECK(outcome.accepted);
      CHECK(outcome.tape == "[1]" + word);
    }
  }
  SECTION("simple_tm accepts every word") {
    const auto machine = Load("simple_tm.turing");
    for (const auto& word : Words("ab", 8)) {
      CAPTURE(word);
      CHECK(Run(machine, word).accepted);
    }
  }
}

}  // namespace
