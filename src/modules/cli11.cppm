// Wrap the CLI11 headers in one module. Pinned to v2.5.0.
// Keep the include in the global fragment above the module line.
// Re-export the used API with export using.
// Upstream shapes two validators as const objects with internal
// linkage, so this file copies them into exported inline copies.
// The copies hold identical values and print identical messages.
// A version change needs a check of these copies.
module;
#include <CLI/CLI.hpp>

export module cli11;

namespace CLI {

// Core types.
export using CLI::App;
export using CLI::AppFormatMode;
export using CLI::CheckedTransformer;
export using CLI::Formatter;
export using CLI::MultiOptionPolicy;
export using CLI::Option;
export using CLI::Option_group;
export using CLI::ParseError;
export using CLI::Range;
export using CLI::detail::ExistingFileValidator;
export using CLI::ignore_case;

}  // namespace CLI

namespace cli11 {

// Copies of the upstream const validators.
export inline const CLI::Range PositiveNumber{CLI::PositiveNumber};
export inline const CLI::detail::ExistingFileValidator ExistingFile{CLI::ExistingFile};

}  // namespace cli11
