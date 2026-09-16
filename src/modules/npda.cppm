// Wrap the NPDA headers in one module.
// Keep all includes in the global fragment above the module line.
// Re-export public names with export using.
// Change the headers to change the API. Add no code here.
module;
#include "npda.h"
#include "parser.h"

export module npda;

namespace npda {

// Core engine.
export using npda::AcceptBy;
export using npda::Error;
export using npda::Hashable;
export using npda::Key;
export using npda::KeyHash;
export using npda::Node;
export using npda::NPDA;
export using npda::Rule;
export using npda::RunOptions;
export using npda::RunResult;
export using npda::explain_rule;
export using npda::generate_non_accepting_state_color;
export using npda::wrap_text;

}  // namespace npda

namespace npda::parse {

// Text format parser with diagnostics.
export using npda::parse::Line;
export using npda::parse::ParseResult;
export using npda::parse::PDA;
export using npda::parse::Rule;
export using npda::parse::SpecTokens;
export using npda::parse::Token;
export using npda::parse::add_simple_error;
export using npda::parse::add_symbol_error;
export using npda::parse::is_eps_tok;
export using npda::parse::lex;
export using npda::parse::line_has_tokens;
export using npda::parse::looks_like_transition;
export using npda::parse::next_nonempty;
export using npda::parse::parse_with_diagnostics;
export using npda::parse::read_all;
export using npda::parse::to_set;

}  // namespace npda::parse
