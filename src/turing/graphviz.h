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
/// Each state is a node. An accepting state has a double border. Each pair of
/// states has one edge, with a label that lists all the rules between the two
/// states as `reads / writes / moves`. States and edges are in the order of the
/// file. Thus, each run gives the same text.
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
/// The extension of `output` sets the image format. Thus, `machine.svg` runs
/// `executable -Tsvg`. The program finds the executable in `PATH` and does not
/// use a shell.
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
