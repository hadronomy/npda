#include "cli/prf.h"
#include "prf.h"

int PRFHandler::operator()(const CommandContext&) {
  using namespace prf;

  // Primitive available to the DSL.
  auto S = succ("S");

  // add with recursion over LAST argument:
  // add(x, 0) = x
  // add(x, y+1) = S(add(x, y))
  // g has arity 1: g(x) = id(x)
  // h has arity 3 with args (y, z, x): h(y, z, x) = S(z)
  auto add = dsl::R1(
    [](dsl::Context g) { return g.id("id"); }, [&](dsl::Context h) { return h(S, dsl::_2); }, "add"
  );

  // mul with recursion over LAST argument:
  // mul(x, 0) = 0
  // mul(x, y+1) = add(mul(x, y), x)
  // g(x) = 0
  // h(y, z, x) = add(z, x)
  auto mul = dsl::R1(
    [](dsl::Context g) { return g.Z("Z^1"); },
    [&](dsl::Context h) { return h(add, dsl::_2, dsl::_3); },
    "mul"
  );

  // pow with recursion over LAST argument:
  // pow(x, 0) = 1
  // pow(x, y+1) = mul(pow(x, y), x)
  // Build constant-1 of arity 1 as S ∘ [Z^1]
  auto one_arity1 = [&] {
    dsl::Context c{1};
    return c(S, c.Z("Z^1"));
  }();

  auto pow = dsl::R1(
    [&](dsl::Context) { return one_arity1; },
    [&](dsl::Context h) { return h(mul, dsl::_2, dsl::_3); },
    "pow"
  );

  prf::Trace trace(this->mode);

  {
    std::vector<uint64_t> args = this->params;
    uint64_t r = (*pow)(args, trace);
    std::cout << "pow(" << prf::join_u64(args) << ") = " << r << "\n";
    trace.print(std::cout);
    trace.clear();
    std::cout << "\n";
  }

  return 0;
}