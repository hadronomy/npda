#ifndef NPDA_SRC_TURING_GRAPHVIZ_H_
#define NPDA_SRC_TURING_GRAPHVIZ_H_

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

#include "turing/machine.h"

namespace turing {

/// Returns the machine as a Graphviz digraph.
///
/// States appear as nodes, with a double border for accepting states, and each
/// connected pair has one edge whose label lists its transitions as
/// `reads / writes / moves`. States and edges follow file order so that each
/// call produces the same text.
[[nodiscard]] std::string ToDot(const Definition& definition);

/// Writes `ToDot(definition)` to `path`.
///
/// # Errors
///
/// An `Error` with the file name if the file cannot be written.
[[nodiscard]] std::expected<void, Error> WriteDot(
    const Definition& definition,
    const std::filesystem::path& path
);

/// Draws the machine with Graphviz.
///
/// The extension of `output` sets the image format, so `machine.svg` runs
/// `executable -Tsvg`. The program finds the executable in `PATH` and runs it
/// without a shell.
///
/// # Errors
///
/// An `Error` if the executable cannot start or if it fails.
[[nodiscard]] std::expected<void, Error> RenderGraph(
    const Definition& definition,
    const std::filesystem::path& output,
    std::string_view executable = "dot"
);

}  // namespace turing

#endif  // NPDA_SRC_TURING_GRAPHVIZ_H_
