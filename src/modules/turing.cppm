// Wrap the Turing machine headers in one module.
// Keep all includes in the global fragment above the module line.
// Re-export public names with export using.
// Change the headers to change the API. Add no code here.
module;
#include "turing/turing.h"
#include "turing/parser.h"

export module turing;

namespace turing {

// Core engine.
export using turing::Direction;
export using turing::Error;
export using turing::GraphvizOptions;
export using turing::Hashable;
export using turing::MultiTapeNode;
export using turing::MultiTapeRule;
export using turing::OperationMode;
export using turing::PairHash;
export using turing::Rule;
export using turing::RunOptions;
export using turing::RunResult;
export using turing::TMConfig;
export using turing::TapeDirection;
export using turing::TuringMachine;
export using turing::make_single_rule;

}  // namespace turing

namespace turing::parse {

// Text format parser with diagnostics.
export using turing::parse::ConfigKeySpec;
export using turing::parse::ConfigParseResult;
export using turing::parse::Line;
export using turing::parse::ParseResult;
export using turing::parse::Rule;
export using turing::parse::SpecTokens;
export using turing::parse::TM;
export using turing::parse::Token;
export using turing::parse::TomlToken;
export using turing::parse::TomlTokenType;
export using turing::parse::add_simple_error;
export using turing::parse::add_symbol_error;
export using turing::parse::default_config_schema;
export using turing::parse::levenshtein_distance_bounded;
export using turing::parse::lex;
export using turing::parse::line_has_tokens;
export using turing::parse::looks_like_multi_tape_transition;
export using turing::parse::looks_like_single_tape_transition;
export using turing::parse::next_nonempty;
export using turing::parse::parse_bool_literal;
export using turing::parse::parse_enum_value;
export using turing::parse::parse_structured_config;
export using turing::parse::parse_structured_config_with_schema;
export using turing::parse::parse_uint_nonzero;
export using turing::parse::parse_with_diagnostics;
export using turing::parse::read_all;
export using turing::parse::suggest_keys;
export using turing::parse::to_set;
export using turing::parse::trim_ws;
export using turing::parse::unquote_if;

}  // namespace turing::parse
