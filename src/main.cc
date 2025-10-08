
#include <iostream>
#include <string_view>
#include <vector>

#include "npda.h"

enum class S { push, mid, pop, acc };

// Formatter for State enum
template <>
struct fmt::formatter<S> {
  constexpr auto parse(format_parse_context& ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(S s, FormatContext& ctx) const {
    switch (s) {
      case S::push:
        return fmt::format_to(ctx.out(), "push");
      case S::mid:
        return fmt::format_to(ctx.out(), "mid");
      case S::pop:
        return fmt::format_to(ctx.out(), "pop");
      case S::acc:
        return fmt::format_to(ctx.out(), "acc");
      default:
        return fmt::format_to(ctx.out(), "unknown");
    }
  }
};

int main() {
  using State = S;
  using Input = char;
  using Sym = char;
  using Rule = npda::Rule<State, Input, Sym>;
  using NPDA = npda::NPDA<State, Input, Sym>;

  // Stack bottom
  const Sym Z = '$';

  // NPDA that recognizes palindromes over {a,b} with optional middle 'c'
  auto mexp =
    NPDA::Builder()
      .start(State::push)
      .accepting({State::acc})
      .accept_by(npda::AcceptBy::FinalState)
      .stack_bottom(Z)
      // push phase: read 'a' or 'b' and push to stack
      .rule(Rule{
        .from = State::push,
        .input = 'a',
        .stack_top = std::nullopt,
        .to = State::push,
        .push = {'a'}
      })
      .rule(Rule{
        .from = State::push,
        .input = 'b',
        .stack_top = std::nullopt,
        .to = State::push,
        .push = {'b'}
      })
      // nondet guess midpoint (even length): epsilon to pop phase
      .rule(Rule{
        .from = State::push,
        .input = std::nullopt,
        .stack_top = std::nullopt,
        .to = State::pop,
        .push = {}
      })
      // nondet guess middle 'c' (odd length): consume 'c' then pop
      .rule(Rule{
        .from = State::push, .input = 'c', .stack_top = std::nullopt, .to = State::pop, .push = {}
      })
      // pop phase: match mirror by popping matching symbols
      .rule(Rule{.from = State::pop, .input = 'a', .stack_top = 'a', .to = State::pop, .push = {}})
      .rule(Rule{.from = State::pop, .input = 'b', .stack_top = 'b', .to = State::pop, .push = {}})
      // accept by final state at end when stack has only bottom:
      // epsilon move to acc if top is bottom '$'
      .rule(Rule{
        .from = State::pop, .input = std::nullopt, .stack_top = Z, .to = State::acc, .push = {Z}
      })
      .build();

  if (!mexp) {
    std::cerr << "Build error: " << mexp.error().message << "\n";
    return 1;
  }
  NPDA m = *std::move(mexp);

  auto run = [&](std::string_view s, bool trace = false) {
    auto r = m.run(
      s,
      npda::RunOptions{
        .bfs = true,
        .max_expansions = 100000,
        .track_witness = true,
        .trace = trace,
        .trace_colors = true,
        .trace_compact = false
      }
    );
    if (!r) {
      std::cout << s << " -> error: " << r.error().message << "\n";
      return;
    }
    std::cout << s << " -> accepted=" << std::boolalpha << r->accepted
              << " expansions=" << r->expansions;
    if (r->witness) {
      std::cout << " witness_rules=[";
      for (std::size_t i = 0; i < r->witness->size(); ++i) {
        std::cout << (*r->witness)[i] << (i + 1 < r->witness->size() ? "," : "");
      }
      std::cout << "]";
    }
    std::cout << "\n";
  };

  // Tests
  std::cout << "\n=== Regular Tests ===\n";
  run("");       // true (even palindrome)
  run("a");      // true? No, needs center c: this NPDA requires even or with c
  run("c");      // true (empty mirror around 'c')
  run("aa");     // true
  run("abba");   // true
  run("abcba");  // true (with 'c' center)
  run("abca");   // false
  run("abab");   // false

  // Trace demonstration
  std::cout << "\n=== Trace Mode Demonstration ===\n";
  std::cout << "Showing beautiful step-by-step trace for 'abba':\n";
  run("abba", true);  // with trace enabled

  // Trace with natural language explanations demonstration
  std::cout << "\n=== Trace with Natural Language Explanations ===\n";
  std::cout << "Showing trace with natural language explanations for 'abcba':\n";
  auto run_with_explanations = [&](std::string_view s) {
    auto r = m.run(
      s,
      npda::RunOptions{
        .bfs = true,
        .max_expansions = 100000,
        .track_witness = true,
        .trace = true,
        .trace_colors = true,
        .trace_compact = false,
        .trace_explanations = true  // Enable natural language explanations
      }
    );
    if (!r) {
      std::cout << s << " -> error: " << r.error().message << "\n";
      return;
    }
    std::cout << s << " -> accepted=" << std::boolalpha << r->accepted
              << " expansions=" << r->expansions;
    if (r->witness) {
      std::cout << " witness_rules=[";
      for (std::size_t i = 0; i < r->witness->size(); ++i) {
        std::cout << (*r->witness)[i] << (i + 1 < r->witness->size() ? "," : "");
      }
      std::cout << "]";
    }
    std::cout << "\n";
  };
  run_with_explanations("abcba");

  std::cout << "\n=== Trace for Rejected Input ===\n";
  std::cout << "Showing trace for rejected input 'abab':\n";
  run("abab", true);

  std::cout << "\n=== Trace for Another Rejected Input ===\n";
  std::cout << "Showing trace for rejected input 'abca':\n";
  run("abca", true);
}