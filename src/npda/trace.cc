#include "npda/trace.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <iterator>
#include <optional>
#include <ostream>
#include <print>
#include <ranges>
#include <span>
#include <string>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "automata/symbol.h"
#include "automata/text.h"
#include "npda/definition.h"
#include "npda/machine.h"
#include "npda/run.h"
#include "npda/transition.h"
#include "terminal/box.h"
#include "terminal/style.h"
#include "terminal/table.h"

namespace npda {

namespace {

using automata::RuleLayout;
using automata::RuleText;
using terminal::BoxCell;
using terminal::Style;
using terminal::Styled;
using terminal::Table;
using terminal::Tone;

/// The number of input cells that the detailed view draws on each side of the
/// next symbol.
constexpr std::size_t kInputRadius = 8;

/// The width of the labels of the detailed view, from `state` to `accept`.
constexpr std::size_t kLabelWidth = 6;

/// A copy of a configuration from a run, so that a view can draw it after the
/// run.
struct Snapshot {
  Symbol state;
  std::size_t consumed;
  /// The top symbol is first.
  std::vector<Symbol> stack;
};

/// A copy of a visit from a run.
struct Record {
  Snapshot configuration;
  std::optional<Origin> origin;
  std::vector<Move> moves;
  bool accepting;
};

Snapshot Successor(const Snapshot& from, const Transition& transition) {
  std::vector<Symbol> stack = transition.push;
  stack.insert(stack.end(), std::next(from.stack.begin()), from.stack.end());
  return {
      .state = transition.to,
      .consumed = from.consumed + (transition.input ? 1 : 0),
      .stack = std::move(stack),
  };
}

std::string Explain(const Transition& transition) {
  std::string text = transition.input
                         ? std::format("read {}", *transition.input)
                         : std::string("read nothing");
  if (transition.push.empty()) {
    text += std::format(", pop {}", transition.top);
  } else if (transition.push == std::vector{transition.top}) {
    text += std::format(", keep {}", transition.top);
  } else {
    text += std::format(
        ", replace {} with {}",
        transition.top,
        automata::Concat(transition.push)
    );
  }
  if (transition.to == transition.from) {
    return text + std::format(", stay in {}", transition.to);
  }
  return text + std::format(", go to {}", transition.to);
}

RuleText Split(const Transition& transition, const Style& style) {
  const auto& epsilon = style.glyphs().epsilon;
  const auto push = automata::Concat(transition.push);
  return {
      .condition = std::format(
          "({}, {}, {})",
          transition.from,
          transition.input ? transition.input->name() : epsilon,
          transition.top
      ),
      .result =
          std::format("({}, {})", transition.to, push.empty() ? epsilon : push),
  };
}

std::vector<RuleText> SplitAll(
    const Definition& definition,
    const Style& style
) {
  std::vector<RuleText> rules;
  for (const auto& transition : definition.transitions) {
    rules.push_back(Split(transition, style));
  }
  return rules;
}

/// Draws the recorded visits of one run in the view of the options.
class TraceWriter {
 public:
  TraceWriter(
      std::ostream& out,
      const Definition& definition,
      std::span<const Symbol> input,
      const Style& style,
      const TraceOptions& options,
      std::vector<Record> records,
      std::size_t visits
  )
      : out_(&out),
        definition_(&definition),
        input_(input),
        style_(style),
        options_(options),
        rules_(SplitAll(definition, style)),
        layout_(RuleLayout::For(rules_)),
        records_(std::move(records)),
        visits_(visits) {}

  void Write(const std::expected<RunResult, LimitReached>& result) const {
    const auto input = automata::Concat(input_);
    automata::WriteHeading(
        *out_,
        "trace",
        input.empty() ? style_.glyphs().epsilon : input,
        style_
    );
    switch (options_.view) {
      case TraceView::kDetailed:
        WriteDetailed();
        break;
      case TraceView::kCompact:
        WriteCompact();
        break;
      case TraceView::kTree:
        /// The ids on the accepting path, if the trace recorded the end of the
        /// path.
        WriteTree(AcceptingIds(result));
        break;
    }
    if (visits_ > records_.size()) {
      if (options_.view == TraceView::kDetailed) std::println(*out_);
      automata::WriteHiddenNotice(
          *out_,
          visits_ - records_.size(),
          "configuration",
          style_
      );
    }
    if (options_.view != TraceView::kTree && result && result->Accepted()) {
      const auto& path = *result->accepting_path;
      const bool searched_other_branches = visits_ > path.size() + 1;
      if (searched_other_branches) WritePath(path);
    }
    std::println(*out_);
  }

 private:
  [[nodiscard]] Styled Id(std::size_t id) const {
    return style_.Span(Tone::kMuted, std::format("#{}", id));
  }

  [[nodiscard]] Styled State(const Snapshot& configuration) const {
    return style_.Span(Tone::kStrong, configuration.state.name());
  }

  /// `ab▸ba`: the input that the automaton read, a cursor, and the input that
  /// remains.
  [[nodiscard]] Styled Input(const Snapshot& configuration) const {
    if (input_.empty())
      return style_.Span(Tone::kMuted, style_.glyphs().epsilon);
    const auto read = input_.first(configuration.consumed);
    const auto unread = input_.subspan(configuration.consumed);
    return style_.Span(Tone::kMuted, automata::Concat(read)) +
           style_.Span(Tone::kAccent, style_.glyphs().cursor) +
           style_.Span(Tone::kPlain, automata::Concat(unread));
  }

  /// `A A S`: the stack with the top first, and the top in bold.
  [[nodiscard]] Styled Stack(const Snapshot& configuration) const {
    const auto& stack = configuration.stack;
    if (stack.empty())
      return style_.Span(Tone::kMuted, style_.glyphs().epsilon);
    Styled text = style_.Span(Tone::kStrong, stack.front().name());
    for (const auto symbol : stack | std::views::drop(1)) {
      text += style_.Span(Tone::kPlain, std::format(" {}", symbol));
    }
    return text;
  }

  /// The rule with the widths of all the rules, so that it aligns with them.
  [[nodiscard]] Styled Rule(std::size_t transition) const {
    return automata::FormatRule(
        transition,
        rules_[transition],
        layout_,
        style_
    );
  }

  /// The rule without spaces for alignment, for use in a sentence.
  [[nodiscard]] Styled InlineRule(std::size_t transition) const {
    return automata::FormatRule(
        transition,
        rules_[transition],
        RuleLayout{},
        style_
    );
  }

  /// `⇒ #4`, with a note if the search found #4 before, or if the search did
  /// not explore #4.
  [[nodiscard]] Styled Target(const Move& move) const {
    Styled text =
        style_.Span(Tone::kMuted, std::format("{} ", style_.glyphs().leads_to));
    text += style_.Span(Tone::kAccent, std::format("#{}", move.target));
    if (move.repeated) {
      text += style_.Span(Tone::kMuted, " (seen)");
    } else if (move.target >= visits_) {
      text += style_.Span(Tone::kMuted, " (not explored)");
    }
    return text;
  }

  [[nodiscard]] Styled MoveLine(const Move& move) const {
    return style_.Span(
               Tone::kPlain,
               terminal::PadRight(Rule(move.transition), layout_.Width(style_))
           ) +
           style_.Span(Tone::kPlain, "  ") + Target(move);
  }

  [[nodiscard]] std::string AcceptReason(const Snapshot& configuration) const {
    if (std::holds_alternative<AcceptByEmptyStack>(definition_->acceptance)) {
      return "input read and stack empty";
    }
    return std::format("input read in final state {}", configuration.state);
  }

  void WriteDetailed() const {
    for (std::size_t id = 0; id < records_.size(); ++id) {
      const auto& record = records_[id];
      const auto& configuration = record.configuration;
      std::println(*out_);
      WriteBlockHeading(id, record);
      WriteLabeled("state", State(configuration).text);
      WriteInputCells(configuration);
      WriteStackCells(configuration);
      if (record.accepting) {
        WriteLabeled(
            "accept",
            style_.Paint(Tone::kGood, AcceptReason(configuration))
        );
      } else if (record.moves.empty()) {
        WriteLabeled("moves", style_.Paint(Tone::kMuted, "none (dead end)"));
      }
      for (std::size_t index = 0; index < record.moves.size(); ++index) {
        const auto& move = record.moves[index];
        WriteLabeled(index == 0 ? "moves" : "", MoveLine(move).text);
        if (options_.explain) {
          WriteLabeled(
              "",
              std::format(
                  "{}{}",
                  std::string(layout_.label + 2, ' '),
                  style_.Paint(
                      Tone::kMuted,
                      Explain(definition_->transitions[move.transition])
                  )
              )
          );
        }
      }
    }
  }

  void WriteBlockHeading(std::size_t id, const Record& record) const {
    const auto id_text = style_.Paint(Tone::kHeading, std::format("#{}", id));
    if (!record.origin) {
      std::println(
          *out_,
          "{}  {}",
          id_text,
          style_.Paint(Tone::kMuted, "start")
      );
      return;
    }
    std::println(
        *out_,
        "{}  {} {} {} {}",
        id_text,
        style_.Paint(Tone::kMuted, "from"),
        Id(record.origin->parent).text,
        style_.Paint(Tone::kMuted, "by"),
        InlineRule(record.origin->transition).text
    );
  }

  void WriteLabeled(std::string_view label, std::string_view value) const {
    std::println(
        *out_,
        "    {}  {}",
        style_.Paint(Tone::kMuted, terminal::PadRight(label, kLabelWidth)),
        value
    );
  }

  /// Draws a row of cells with `label` next to it. If `head` has a value, puts
  /// a mark below that cell.
  void WriteCells(
      std::string_view label,
      std::span<const BoxCell> cells,
      std::optional<std::size_t> head,
      std::string_view after
  ) const {
    const auto row = terminal::DrawBoxedRow(cells, style_);
    WriteLabeled("", row.top);
    WriteLabeled(label, std::format("{}{}", row.middle, after));
    WriteLabeled("", row.bottom);
    if (head) {
      WriteLabeled(
          "",
          std::string(row.centers[*head], ' ') +
              style_.Paint(Tone::kAccent, style_.glyphs().head)
      );
    }
  }

  void WriteInputCells(const Snapshot& configuration) const {
    if (input_.empty()) {
      WriteLabeled(
          "input",
          style_.Paint(Tone::kMuted, style_.glyphs().epsilon)
      );
      return;
    }
    const auto consumed = configuration.consumed;
    const auto begin = consumed > kInputRadius ? consumed - kInputRadius : 0;
    const auto end = std::min(input_.size(), consumed + kInputRadius + 1);
    std::vector<BoxCell> cells;
    if (begin > 0) {
      cells.push_back({std::string(style_.glyphs().ellipsis), Tone::kMuted});
    }
    std::optional<std::size_t> head;
    for (std::size_t index = begin; index < end; ++index) {
      if (index == consumed) head = cells.size();
      cells.push_back({
          std::string(input_[index].name()),
          index < consumed    ? Tone::kMuted
          : index == consumed ? Tone::kStrong
                              : Tone::kPlain,
      });
    }
    if (end < input_.size()) {
      cells.push_back({std::string(style_.glyphs().ellipsis), Tone::kMuted});
    }
    const auto after =
        head ? std::string() : style_.Paint(Tone::kMuted, "  all read");
    WriteCells("input", cells, head, after);
  }

  void WriteStackCells(const Snapshot& configuration) const {
    if (configuration.stack.empty()) {
      WriteLabeled(
          "stack",
          style_.Paint(
              Tone::kMuted,
              std::format("{}  empty", style_.glyphs().epsilon)
          )
      );
      return;
    }
    std::vector<BoxCell> cells;
    for (const auto symbol : configuration.stack) {
      cells.push_back({
          std::string(symbol.name()),
          cells.empty() ? Tone::kStrong : Tone::kPlain,
      });
    }
    WriteCells(
        "stack",
        cells,
        std::nullopt,
        style_.Paint(Tone::kMuted, "  top first")
    );
  }

  [[nodiscard]] Styled Status(const Record& record) const {
    if (record.accepting) return style_.Span(Tone::kGood, "accept");
    if (record.moves.empty()) return style_.Span(Tone::kMuted, "dead end");
    return {};
  }

  void WriteCompact() const {
    Table table;
    table.AddRow({
        style_.Span(Tone::kMuted, "#"),
        style_.Span(Tone::kMuted, "state"),
        style_.Span(Tone::kMuted, "input"),
        style_.Span(Tone::kMuted, "stack"),
    });
    const auto& glyphs = style_.glyphs();
    for (std::size_t id = 0; id < records_.size(); ++id) {
      const auto& record = records_[id];
      const auto& configuration = record.configuration;
      table.AddRow({
          Id(id),
          State(configuration),
          Input(configuration),
          Stack(configuration),
          Status(record),
      });
      for (std::size_t index = 0; index < record.moves.size(); ++index) {
        const bool last = index + 1 == record.moves.size();
        table.AddLine(
            style_.Span(Tone::kMuted, "  ") +
            style_.Span(
                Tone::kMuted,
                last ? glyphs.tree_last : glyphs.tree_branch
            ) +
            MoveLine(record.moves[index])
        );
      }
    }
    table.Write(*out_);
  }

  /// The ids on the accepting path, when the trace recorded its end.
  [[nodiscard]] std::unordered_set<std::size_t> AcceptingIds(
      const std::expected<RunResult, LimitReached>& result
  ) const {
    std::unordered_set<std::size_t> ids;
    if (!result || !result->Accepted() || records_.empty() ||
        !records_.back().accepting) {
      return ids;
    }
    for (std::optional<std::size_t> id = records_.size() - 1; id;
         id = records_[*id].origin ? std::optional(records_[*id].origin->parent)
                                   : std::nullopt) {
      ids.insert(*id);
    }
    return ids;
  }

  void WriteTree(const std::unordered_set<std::size_t>& path) const {
    if (records_.empty()) return;
    Table table;
    table.AddRow({
        Styled{},
        style_.Span(Tone::kMuted, "state"),
        style_.Span(Tone::kMuted, "input"),
        style_.Span(Tone::kMuted, "stack"),
    });
    const auto& root = records_.front();
    table.AddRow({
        style_.Span(Tone::kMuted, "start"),
        State(root.configuration),
        Input(root.configuration),
        Stack(root.configuration),
        Status(root),
    });
    AddChildren(table, 0, Styled{}, path);
    table.Write(*out_);
  }

  void AddChildren(
      Table& table,
      std::size_t id,
      const Styled& indent,
      const std::unordered_set<std::size_t>& path
  ) const {
    const auto& glyphs = style_.glyphs();
    const auto& record = records_[id];
    for (std::size_t index = 0; index < record.moves.size(); ++index) {
      const auto& move = record.moves[index];
      const bool last = index + 1 == record.moves.size();
      const bool recorded = !move.repeated && move.target < records_.size();
      const bool on_path = recorded && path.contains(move.target);
      const auto branch = style_.Span(
          Tone::kMuted,
          last ? glyphs.tree_last : glyphs.tree_branch
      );
      const auto label = style_.Span(
          on_path ? Tone::kGood : Tone::kAccent,
          automata::TransitionLabel(move.transition, style_)
      );
      const auto configuration = Successor(
          record.configuration,
          definition_->transitions[move.transition]
      );
      Styled status;
      if (recorded) {
        status = Status(records_[move.target]);
      } else if (move.repeated) {
        status = style_.Span(Tone::kMuted, "seen");
      } else if (move.target < visits_) {
        status = style_.Span(Tone::kMuted, "hidden");
      } else {
        status = style_.Span(Tone::kMuted, "not explored");
      }
      table.AddRow({
          indent + branch + label,
          State(configuration),
          Input(configuration),
          Stack(configuration),
          status,
      });
      if (recorded) {
        const auto pipe =
            style_.Span(Tone::kMuted, last ? "   " : glyphs.tree_pipe);
        AddChildren(table, move.target, indent + pipe, path);
      }
    }
  }

  void WritePath(std::span<const std::size_t> path) const {
    automata::WriteHeading(*out_, "accepting path", "", style_);
    Snapshot configuration{
        .state = definition_->start_state,
        .consumed = 0,
        .stack = {definition_->initial_stack_symbol},
    };
    const auto leads_to = style_.Span(Tone::kMuted, style_.glyphs().leads_to);
    Table table;
    table.AddRow({
        style_.Span(Tone::kMuted, "start"),
        Styled{},
        State(configuration),
        Input(configuration),
        Stack(configuration),
    });
    for (const auto transition : path) {
      configuration =
          Successor(configuration, definition_->transitions[transition]);
      table.AddRow({
          Rule(transition),
          leads_to,
          State(configuration),
          Input(configuration),
          Stack(configuration),
      });
    }
    table.Write(*out_);
  }

  std::ostream* out_;
  const Definition* definition_;
  std::span<const Symbol> input_;
  Style style_;
  TraceOptions options_;
  std::vector<RuleText> rules_;
  RuleLayout layout_;
  std::vector<Record> records_;
  std::size_t visits_;
};

}  // namespace

std::string Describe(const Definition& definition, const Style& style) {
  const auto separator = style.Separator();
  std::string text = "empty stack";
  if (const auto* final_state =
          std::get_if<AcceptByFinalState>(&definition.acceptance)) {
    text = "final state F = " + automata::FormatSet(final_state->states);
  }
  const auto count = definition.transitions.size();
  return std::format(
      "{}{}q0 = {}{}Z0 = {}{}{} {}",
      text,
      separator,
      definition.start_state,
      separator,
      definition.initial_stack_symbol,
      separator,
      count,
      count == 1 ? "transition" : "transitions"
  );
}

std::string FormatTransition(const Transition& transition, const Style& style) {
  const auto text = Split(transition, style);
  return std::format(
      "{} {} {}",
      text.condition,
      style.glyphs().arrow,
      text.result
  );
}

std::string FormatPath(std::span<const std::size_t> path, const Style& style) {
  std::string text;
  for (const auto transition : path) {
    if (!text.empty()) text += ' ';
    text += automata::TransitionLabel(transition, style);
  }
  return text;
}

void WriteTransitions(
    std::ostream& out,
    const Definition& definition,
    const Style& style,
    bool explain
) {
  std::vector<std::string> notes;
  if (explain) {
    for (const auto& transition : definition.transitions) {
      notes.push_back(Explain(transition));
    }
  }
  automata::WriteTransitionList(out, SplitAll(definition, style), notes, style);
}

std::expected<RunResult, LimitReached> TraceRun(
    const Machine& machine,
    std::span<const Symbol> input,
    std::size_t max_expansions,
    const TraceOptions& options,
    std::ostream& out,
    const Style& style
) {
  std::vector<Record> records;
  std::size_t visits = 0;
  const auto record = [&](const Visit& visit) {
    ++visits;
    if (visit.id >= options.limit) return;
    const auto& configuration = visit.configuration;
    records.push_back({
        .configuration =
            {
                .state = configuration.state,
                .consumed = configuration.consumed,
                .stack = std::ranges::to<std::vector>(configuration.stack),
            },
        .origin = visit.origin,
        .moves = std::ranges::to<std::vector>(visit.moves),
        .accepting = visit.accepting,
    });
  };
  const auto result = machine.Run(
      input,
      {.max_expansions = max_expansions, .observer = record}
  );
  const TraceWriter writer(
      out,
      machine.definition(),
      input,
      style,
      options,
      std::move(records),
      visits
  );
  writer.Write(result);
  return result;
}

}  // namespace npda
