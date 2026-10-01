#ifndef NPDA_SRC_NPDA_SEARCH_H_
#define NPDA_SRC_NPDA_SEARCH_H_

#include <cstddef>
#include <deque>
#include <expected>
#include <optional>
#include <span>
#include <unordered_set>
#include <vector>

#include "npda/machine.h"

namespace npda {

class Machine::Search {
 public:
  Search(const Machine& machine, std::span<const Symbol> input,
         RunOptions options);
  std::expected<RunResult, Error> Execute();

 private:
  struct ConfigurationKey {
    Symbol state;
    std::size_t input_position;
    std::vector<Symbol> stack;
    bool operator==(const ConfigurationKey&) const = default;
  };

  struct ConfigurationHash {
    std::size_t operator()(const ConfigurationKey& configuration) const;
  };

  std::optional<std::size_t> TakeNextNode();
  void ObserveVisit(std::size_t node_index);
  void Observe(EventKind kind, std::size_t node_index,
               bool complete = false) const;
  RunResult Accept(std::size_t node_index) const;
  std::span<const std::size_t> TransitionsFrom(
      Symbol state, std::optional<Symbol> input) const;
  std::expected<void, Error> ExpandNode(std::size_t node_index);
  std::expected<void, Error> FollowTransition(std::size_t node_index,
                                              std::size_t transition_index);
  void AddNode(SearchNode node);

  const Machine& machine_;
  std::span<const Symbol> input_;
  const RunOptions options_;
  std::vector<SearchNode> nodes_;
  std::deque<std::size_t> frontier_;
  std::unordered_set<ConfigurationKey, ConfigurationHash> visited_;
  std::vector<std::size_t> explored_nodes_;
  std::vector<std::size_t> deadend_nodes_;
  std::size_t expansions_ = 0;
  std::size_t furthest_node_index_ = 0;
};

}  // namespace npda

#endif  // NPDA_SRC_NPDA_SEARCH_H_
