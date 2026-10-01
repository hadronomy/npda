#ifndef NPDA_TURING_GRAPHVIZ_H_
#define NPDA_TURING_GRAPHVIZ_H_

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

#include "turing/machine.h"

namespace turing {

struct GraphvizOptions {
  std::string rankdir = "LR";              // "LR", "TB", "RL", "BT"
  std::string fontname = "Anonymous Pro";  // Font for nodes/edges
  bool color_accepting = true;             // Accepting states styled
  bool show_start_edge = true;             // Start arrow to start state
  bool show_legend = false;                // Include legend subgraph
  bool compact_labels = true;              // Single-line transition labels
  // Styling (prettier defaults)
  std::string background = "#FFFFFF";
  std::string node_shape = "circle";
  std::string node_color = "#455A64";
  std::string node_fill = "#ECEFF1";
  std::string node_font = "#263238";
  std::string accept_color = "#2E7D32";
  std::string accept_fill = "#E8F5E9";
  std::string edge_color = "#37474F";
  std::string edge_font = "#263238";
  double node_penwidth = 1.2;
  double accept_penwidth = 1.6;
  double edge_penwidth = 1.4;
  double arrowsize = 0.9;
  double nodesep = 0.35;
  double ranksep = 0.6;
};

[[nodiscard]] std::string ToGraphvizDot(const Definition& definition,
                                        const GraphvizOptions& options = {});
[[nodiscard]] std::expected<void, Error> WriteGraphvizDot(
    const Definition& definition, const std::filesystem::path& path,
    const GraphvizOptions& options = {});
[[nodiscard]] std::expected<void, Error> ExportGraphvizImage(
    const Definition& definition, const std::filesystem::path& output,
    std::string_view executable = "dot", std::string_view format = "png",
    const GraphvizOptions& options = {});

}  // namespace turing

#endif  // NPDA_TURING_GRAPHVIZ_H_
