#include "turing/graphviz.h"

#include <spawn.h>
#include <sys/types.h>  // IWYU pragma: keep
#include <sys/wait.h>
#include <unistd.h>

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <ios>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "turing/machine.h"
#include "turing/transition_text.h"

extern char** environ;

namespace turing {

namespace {

[[nodiscard]] std::string DotEscape(std::string_view s) {
  std::string out;
  out.reserve(s.size() + 8);
  for (char c : s) {
    if (c == '"' || c == '\\') out.push_back('\\');
    if (c == '\n') {
      out += "\\n";
      continue;
    }
    out.push_back(c);
  }
  return out;
}

}  // namespace

template <typename A, typename B>
struct PairHash {
  std::size_t operator()(const std::pair<A, B>& p) const noexcept {
    std::size_t h1 = std::hash<A>{}(p.first);
    std::size_t h2 = std::hash<B>{}(p.second);
    // boost-ish hash combine
    return h1 ^ (h2 + 0x9e3779b97f4a7c15ULL + (h1 << 6) + (h1 >> 2));
  }
};

std::string ToGraphvizDot(const Definition& definition,
                          const GraphvizOptions& opt) {
  std::unordered_set<std::string> accepting_str;
  accepting_str.reserve(definition.accepting_states.size());
  for (const auto& s : definition.accepting_states) {
    accepting_str.insert(std::format("{}", s));
  }

  auto state_id = [](const Symbol& s) { return std::format("{}", s); };

  auto rule_text = [&](const Transition& r) {
    const auto reads = JoinReads(r.tapes);
    const auto writes = JoinWrites(r.tapes);
    const auto moves = JoinMoves(r.tapes);

    if (opt.compact_labels) {
      // Compact, readable item
      return std::format("r:({}) | w:({}) | m:({})", reads, writes, moves);
    }
    return std::format("read: ({})  write: ({})  move: ({})", reads, writes,
                       moves);
  };

  // Collect all states explicitly
  std::unordered_set<std::string> all_states;
  all_states.insert(state_id(definition.start_state));
  for (const auto& s : definition.accepting_states)
    all_states.insert(state_id(s));
  for (const auto& r : definition.transitions) {
    all_states.insert(state_id(r.from_state));
    all_states.insert(state_id(r.to_state));
  }

  // Group edges by (from, to) and aggregate labels
  using EdgeKey = std::pair<std::string, std::string>;
  std::unordered_map<EdgeKey, std::vector<std::string>,
                     PairHash<std::string, std::string>>
      edge_labels;

  edge_labels.reserve(definition.transitions.size());
  for (const auto& r : definition.transitions) {
    const auto from_str = state_id(r.from_state);
    const auto to_str = state_id(r.to_state);
    edge_labels[{from_str, to_str}].push_back(rule_text(r));
  }

  // Build DOT
  std::string dot;
  dot += "digraph TM {\n";
  dot += "  graph [\n";
  dot += std::format("    bgcolor=\"{}\",\n", opt.background);
  dot += "    splines=true,\n";
  dot += "    overlap=false,\n";
  dot += std::format("    pad=\"{}\",\n", 0.15);
  dot += std::format("    nodesep=\"{}\",\n", opt.nodesep);
  dot += std::format("    ranksep=\"{}\"\n", opt.ranksep);
  dot += "  ];\n";
  dot += std::format("  rankdir={};\n", opt.rankdir);
  dot += std::format("  dpi={};", 300);
  dot += std::format(
      "  node [fontname=\"{}\", shape={}, style=filled, color=\"{}\", "
      "fillcolor=\"{}\", fontcolor=\"{}\", penwidth={}];\n",
      DotEscape(opt.fontname), opt.node_shape, opt.node_color, opt.node_fill,
      opt.node_font, opt.node_penwidth);
  dot += std::format(
      "  edge [fontname=\"{}\", color=\"{}\", fontcolor=\"{}\", penwidth={}, "
      "arrowsize={}];\n",
      DotEscape(opt.fontname), opt.edge_color, opt.edge_font, opt.edge_penwidth,
      opt.arrowsize);
  dot += "  labelloc=\"t\";\n";
  dot += "  labeljust=\"l\";\n";
  dot += std::format(
      "  label=\"Turing Machine\\nTapes: {} | Tape dir: {} | Mode: {} | "
      "AllowStay: {} | Blank: {}\";\n",
      definition.config.num_tapes,
      (definition.config.tape_direction == TapeDirection::kBidirectional
           ? "Bidirectional"
           : "Right-only"),
      (definition.config.operation_mode == OperationMode::kSimultaneous
           ? "Simultaneous"
           : "Independent"),
      (definition.config.allow_stay ? "true" : "false"),
      DotEscape(std::format("{}", definition.blank_symbol)));

  // Start node arrow
  if (opt.show_start_edge) {
    dot += "  __start [shape=point, width=0.15, label=\"\", color=\"" +
           opt.edge_color + "\"];\n";
    dot += std::format("  __start -> \"{}\" [color=\"{}\"];\n",
                       DotEscape(state_id(definition.start_state)),
                       opt.edge_color);
  }

  // Render states
  for (const auto& s_str : all_states) {
    const bool is_accept = accepting_str.find(s_str) != accepting_str.end();
    if (is_accept) {
      if (opt.color_accepting) {
        dot += std::format(
            "  \"{}\" [shape=doublecircle, color=\"{}\", fontcolor=\"{}\", "
            "fillcolor=\"{}\", "
            "penwidth={}];\n",
            DotEscape(s_str), opt.accept_color, opt.node_font, opt.accept_fill,
            opt.accept_penwidth);
      } else {
        dot +=
            std::format("  \"{}\" [shape=doublecircle];\n", DotEscape(s_str));
      }
    } else {
      dot += std::format(
          "  \"{}\" [shape={}, color=\"{}\", fillcolor=\"{}\", "
          "fontcolor=\"{}\", penwidth={}];\n",
          DotEscape(s_str), opt.node_shape, opt.node_color, opt.node_fill,
          opt.node_font, opt.node_penwidth);
    }
  }

  // Render merged edges; each edge lists all rule variants
  for (const auto& [key, labels] : edge_labels) {
    const auto& from_str = key.first;
    const auto& to_str = key.second;

    // Build combined label with bullets (•) and newlines
    std::string combined;
    combined.reserve(64 * labels.size());
    for (std::size_t i = 0; i < labels.size(); ++i) {
      if (i) combined += "\\n";
      combined += "• ";
      combined += DotEscape(labels[i]);
    }

    dot += std::format("  \"{}\" -> \"{}\" [label=\"{}\"];\n",
                       DotEscape(from_str), DotEscape(to_str), combined);
  }

  if (opt.show_legend) {
    dot += "  subgraph cluster_legend {\n";
    dot += "    label = \"Legend\";\n";
    dot += "    style = \"rounded,dashed\";\n";
    dot += "    color = \"#B0BEC5\";\n";
    dot += "    fontcolor = \"#37474F\";\n";
    dot +=
        "    legend_accept [label=\"accepting (double circle)\", "
        "shape=doublecircle, color=\"" +
        opt.accept_color + "\", fillcolor=\"" + opt.accept_fill +
        "\", style=filled];\n";
    dot += "    legend_state [label=\"state\", shape=" + opt.node_shape +
           ", color=\"" + opt.node_color + "\", fillcolor=\"" + opt.node_fill +
           "\", style=filled];\n";
    dot +=
        "    legend_t [label=\"edge label: • r:(reads) | w:(writes) | "
        "m:(moves)\", shape=box, "
        "style=rounded, color=\"#B0BEC5\", fontcolor=\"#37474F\"];\n";
    dot += "    legend_state -> legend_state [style=invis];\n";
    dot += "  }\n";
  }

  dot += "}\n";
  return dot;
}

std::expected<void, Error> WriteGraphvizDot(const Definition& definition,
                                            const std::filesystem::path& path,
                                            const GraphvizOptions& opt) {
  std::error_code ec;
  if (path.has_parent_path()) {
    std::filesystem::create_directories(path.parent_path(), ec);
    if (ec) {
      return std::unexpected(
          Error{std::format("failed to create directories '{}': {}",
                            path.string(), ec.message())});
    }
  }
  std::ofstream ofs(path, std::ios::binary);
  if (!ofs) {
    return std::unexpected(
        Error{std::format("failed to open output '{}'", path.string())});
  }
  ofs << ToGraphvizDot(definition, opt);
  if (!ofs) {
    return std::unexpected(
        Error{std::format("failed to write DOT to '{}'", path.string())});
  }
  return {};
}

std::expected<void, Error> ExportGraphvizImage(
    const Definition& definition, const std::filesystem::path& output_path,
    std::string_view dot_exe, std::string_view format,
    const GraphvizOptions& opt) {
  std::error_code ec;
  const auto tmp_dir = std::filesystem::temp_directory_path(ec);
  if (ec) {
    return std::unexpected(
        Error{std::format("failed to get temp directory: {}", ec.message())});
  }
  std::string temporary_path = (tmp_dir / "npda-graphviz-XXXXXX").string();
  const int descriptor = mkstemp(temporary_path.data());
  if (descriptor == -1) {
    return std::unexpected(Error{std::format(
        "failed to create temporary DOT file: {}", std::strerror(errno))});
  }
  close(descriptor);

  struct TemporaryFile {
    std::filesystem::path path;

    ~TemporaryFile() {
      std::error_code ignored;
      std::filesystem::remove(path, ignored);
    }
  } temporary{temporary_path};

  if (auto write = WriteGraphvizDot(definition, temporary.path, opt); !write) {
    return std::unexpected(write.error());
  }
  if (output_path.has_parent_path()) {
    std::filesystem::create_directories(output_path.parent_path(), ec);
    if (ec) return std::unexpected(Error{ec.message()});
  }

  // Use an argument vector so file paths and executable names never enter a
  // shell.
  std::vector<std::string> arguments{
      std::string(dot_exe), "-T" + std::string(format), temporary_path,
      "-Goverlap=false",    "-Gmodel=subset",           "-o",
      output_path.string()};
  std::vector<char*> argv;
  for (auto& argument : arguments) argv.push_back(argument.data());
  argv.push_back(nullptr);
  // POSIX sys/types.h owns pid_t; Darwin stores its typedef in a private
  // header. NOLINTNEXTLINE(misc-include-cleaner)
  pid_t process = 0;
  const int spawn_error = posix_spawnp(&process, argv.front(), nullptr, nullptr,
                                       argv.data(), environ);
  if (spawn_error != 0) {
    return std::unexpected(
        Error{std::format("failed to start graphviz '{}': {}", dot_exe,
                          std::strerror(spawn_error))});
  }
  int status = 0;
  while (waitpid(process, &status, 0) == -1) {
    if (errno == EINTR) continue;
    return std::unexpected(Error{
        std::format("failed to wait for graphviz: {}", std::strerror(errno))});
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    return std::unexpected(Error{std::format(
        "graphviz '{}' failed (process status {})", dot_exe, status)});
  }
  return {};
}

}  // namespace turing
