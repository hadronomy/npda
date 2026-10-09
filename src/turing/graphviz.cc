#include "turing/graphviz.h"

#include <spawn.h>
#include <sys/types.h>  // IWYU pragma: keep
#include <sys/wait.h>
#include <unistd.h>

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <iterator>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

#include "automata/symbol.h"
#include "turing/config.h"
#include "turing/definition.h"
#include "turing/machine.h"
#include "turing/transition.h"
#include "turing/transition_text.h"

extern char** environ;

namespace turing {

namespace {

std::string Quote(std::string_view text) {
  std::string quoted = "\"";
  for (const char character : text) {
    if (character == '\n') {
      quoted += "\\n";
      continue;
    }
    if (character == '"' || character == '\\') quoted += '\\';
    quoted += character;
  }
  return quoted + '"';
}

std::string RuleLabel(const Transition& transition) {
  return std::format(
      "{} / {} / {}",
      JoinReads(transition.tapes),
      JoinWrites(transition.tapes),
      JoinMoves(transition.tapes)
  );
}

}  // namespace

std::string ToDot(const Definition& definition) {
  std::vector<Symbol> states{definition.start_state};
  const auto add_state = [&](Symbol state) {
    if (!std::ranges::contains(states, state)) states.push_back(state);
  };

  struct Edge {
    Symbol from;
    Symbol to;
    std::vector<std::string> labels;
  };

  std::vector<Edge> edges;
  for (const auto& transition : definition.transitions) {
    add_state(transition.from);
    add_state(transition.to);
    auto edge = std::ranges::find_if(edges, [&](const Edge& candidate) {
      return candidate.from == transition.from && candidate.to == transition.to;
    });
    if (edge == edges.end()) {
      edges.push_back({transition.from, transition.to, {}});
      edge = std::prev(edges.end());
    }
    edge->labels.push_back(RuleLabel(transition));
  }
  for (const auto state : definition.accepting_states) add_state(state);

  const auto& config = definition.config;
  std::string dot =
      "digraph TM {\n"
      "  graph [bgcolor=\"#FFFFFF\", splines=true, overlap=false, pad=0.15, "
      "nodesep=0.35, ranksep=0.6, dpi=300];\n"
      "  rankdir=LR;\n"
      "  node [fontname=\"Helvetica\", shape=circle, style=filled, "
      "color=\"#455A64\", fillcolor=\"#ECEFF1\", fontcolor=\"#263238\", "
      "penwidth=1.2];\n"
      "  edge [fontname=\"Helvetica\", color=\"#37474F\", "
      "fontcolor=\"#263238\", penwidth=1.4, arrowsize=0.9];\n"
      "  labelloc=t;\n"
      "  labeljust=l;\n";
  dot += std::format(
      "  label={};\n",
      Quote(
          std::format(
              "{} tape(s), {}, {}, stay {}, blank {}",
              config.num_tapes,
              config.tape_direction == TapeDirection::kBidirectional
                  ? "bidirectional"
                  : "right-only",
              config.operation_mode == OperationMode::kSimultaneous
                  ? "simultaneous"
                  : "independent",
              config.allow_stay ? "allowed" : "forbidden",
              definition.blank
          )
      )
  );
  dot +=
      "  __start [shape=point, width=0.15, label=\"\", color=\"#37474F\"];\n";
  dot +=
      std::format("  __start -> {};\n", Quote(definition.start_state.name()));
  for (const auto state : states) {
    if (std::ranges::contains(definition.accepting_states, state)) {
      dot += std::format(
          "  {} [shape=doublecircle, color=\"#2E7D32\", "
          "fillcolor=\"#E8F5E9\", penwidth=1.6];\n",
          Quote(state.name())
      );
    } else {
      dot += std::format("  {};\n", Quote(state.name()));
    }
  }
  for (const auto& edge : edges) {
    std::string label;
    for (const auto& rule : edge.labels) {
      if (!label.empty()) label += '\n';
      label += rule;
    }
    dot += std::format(
        "  {} -> {} [label={}];\n",
        Quote(edge.from.name()),
        Quote(edge.to.name()),
        Quote(label)
    );
  }
  return dot + "}\n";
}

std::expected<void, Error> WriteDot(
    const Definition& definition,
    const std::filesystem::path& path
) {
  std::ofstream file(path, std::ios::binary);
  if (!file) {
    return std::unexpected(
        Error{std::format("cannot open '{}' for writing", path.string())}
    );
  }
  file << ToDot(definition);
  if (!file) {
    return std::unexpected(
        Error{std::format("cannot write '{}'", path.string())}
    );
  }
  return {};
}

std::expected<void, Error> RenderGraph(
    const Definition& definition,
    const std::filesystem::path& output,
    std::string_view executable
) {
  std::error_code error;
  const auto directory = std::filesystem::temp_directory_path(error);
  if (error) {
    return std::unexpected(
        Error{std::format(
            "cannot find a temporary directory: {}",
            error.message()
        )}
    );
  }
  std::string dot_path = (directory / "npda-graphviz-XXXXXX").string();
  const int descriptor = mkstemp(dot_path.data());
  if (descriptor == -1) {
    return std::unexpected(
        Error{std::format(
            "cannot create a temporary DOT file: {}",
            std::strerror(errno)
        )}
    );
  }
  close(descriptor);

  struct TemporaryFile {
    std::filesystem::path path;

    ~TemporaryFile() {
      std::error_code ignored;
      std::filesystem::remove(path, ignored);
    }
  } temporary{dot_path};

  if (auto written = WriteDot(definition, temporary.path); !written) {
    return written;
  }

  std::string format = output.extension().string();
  format = format.empty() ? "png" : format.substr(1);
  std::vector<std::string> arguments{
      std::string(executable),
      "-T" + format,
      dot_path,
      "-Goverlap=false",
      "-Gmodel=subset",
      "-o",
      output.string()
  };
  std::vector<char*> argv;
  for (auto& argument : arguments) argv.push_back(argument.data());
  argv.push_back(nullptr);
  // NOLINTNEXTLINE(misc-include-cleaner): Darwin keeps pid_t in a private header.
  pid_t process = 0;
  if (const int spawn_error = posix_spawnp(
          &process,
          argv.front(),
          nullptr,
          nullptr,
          argv.data(),
          environ
      );
      spawn_error != 0) {
    return std::unexpected(
        Error{std::format(
            "cannot start '{}': {}",
            executable,
            std::strerror(spawn_error)
        )}
    );
  }
  int status = 0;
  while (waitpid(process, &status, 0) == -1) {
    if (errno == EINTR) continue;
    return std::unexpected(
        Error{std::format(
            "cannot wait for '{}': {}",
            executable,
            std::strerror(errno)
        )}
    );
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    return std::unexpected(
        Error{std::format(
            "'{}' failed with process status {}",
            executable,
            status
        )}
    );
  }
  return {};
}

}  // namespace turing
