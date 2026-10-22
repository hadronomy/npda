#include "turing/trace.h"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <ostream>
#include <print>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "automata/symbol.h"
#include "automata/text.h"
#include "terminal/box.h"
#include "terminal/style.h"
#include "terminal/table.h"
#include "turing/config.h"
#include "turing/definition.h"
#include "turing/machine.h"
#include "turing/run.h"
#include "turing/tape.h"
#include "turing/transition.h"
#include "turing/transition_text.h"

namespace turing {

namespace {

using automata::RuleLayout;
using automata::RuleText;
using terminal::BoxCell;
using terminal::Style;
using terminal::Styled;
using terminal::Table;
using terminal::Tone;

/// The number of cells that a trace draws on each side of a head. With this
/// number, a trace shows a sweep, and two tapes fit next to each other.
constexpr std::size_t kTraceRadius = 8;

/// The width of the labels of the detailed view, from `state` to `reject`.
constexpr std::size_t kLabelWidth = 6;

/// The cells of a tape that the trace shows: from `begin` to `end`, both
/// included.
struct Window {
  std::size_t begin;
  std::size_t end;
  bool cut_left;
  bool cut_right;
};

Window WindowOf(
    const Tape& tape,
    Symbol blank,
    std::optional<std::size_t> radius
) {
  std::size_t first = tape.head;
  std::size_t last = tape.head;
  for (std::size_t index = 0; index < tape.cells.size(); ++index) {
    if (tape.cells[index] == blank) continue;
    first = std::min(first, index);
    last = std::max(last, index);
  }
  if (!radius) {
    return {.begin = first, .end = last, .cut_left = false, .cut_right = false};
  }
  const auto begin =
      std::max(first, tape.head > *radius ? tape.head - *radius : 0);
  const auto end = std::min(last, tape.head + *radius);
  return {
      .begin = begin,
      .end = end,
      .cut_left = begin > first,
      .cut_right = end < last,
  };
}

std::string_view MoveName(HeadMove move) {
  switch (move) {
    case HeadMove::kLeft:
      return "move left";
    case HeadMove::kRight:
      return "move right";
    case HeadMove::kStay:
      return "stay";
  }
  return "stay";
}

std::string Explain(const Transition& transition, OperationMode mode) {
  std::string text;
  for (std::size_t tape = 0; tape < transition.tapes.size(); ++tape) {
    const auto& action = transition.tapes[tape];
    if (transition.tapes.size() > 1) {
      text += std::format("{}Tape {}: ", tape == 0 ? "" : ". ", tape + 1);
    }
    text += std::format("read {}", action.read);
    if (mode == OperationMode::kIndependent) {
      text += action.move == HeadMove::kStay
                  ? std::format(", write {}", action.write)
                  : std::format(", {}", MoveName(action.move));
      continue;
    }
    if (action.write != action.read) {
      text += std::format(", write {}", action.write);
    }
    text += std::format(", {}", MoveName(action.move));
  }
  if (transition.to == transition.from) {
    return text + std::format(", stay in {}", transition.to);
  }
  return text + std::format(", go to {}", transition.to);
}

RuleText Split(const Transition& transition, OperationMode mode) {
  std::string result(transition.to.name());
  for (const auto& action : transition.tapes) {
    result += mode == OperationMode::kIndependent
                  ? std::format(", {}", ActionText(action))
                  : std::format(", {}, {}", action.write, ToChar(action.move));
  }
  return {
      .condition = std::format(
          "({}, {})",
          transition.from,
          JoinReads(transition.tapes, ", ")
      ),
      .result = std::format("({})", result),
  };
}

std::vector<RuleText> SplitAll(const Definition& definition) {
  std::vector<RuleText> rules;
  for (const auto& transition : definition.transitions) {
    rules.push_back(Split(transition, definition.config.operation_mode));
  }
  return rules;
}

/// A copy of a step from a run, so that a view can draw it after the run.
struct Record {
  Configuration configuration;
  std::optional<std::size_t> transition;
};

/// Draws the recorded steps of one run in the view of the options.
class TraceWriter {
 public:
  TraceWriter(
      std::ostream& out,
      const Definition& definition,
      const Style& style,
      const TraceOptions& options,
      std::vector<Record> records,
      std::size_t steps
  )
      : out_(&out),
        definition_(&definition),
        style_(style),
        options_(options),
        rules_(SplitAll(definition)),
        layout_(RuleLayout::For(rules_)),
        records_(std::move(records)),
        steps_(steps) {}

  void Write(
      std::span<const Symbol> input,
      const std::expected<RunResult, LimitReached>& result
  ) const {
    const auto text = automata::Concat(input);
    automata::WriteHeading(
        *out_,
        "trace",
        text.empty() ? style_.glyphs().epsilon : text,
        style_
    );
    const auto halted = result && result->steps < records_.size()
                            ? std::optional(result->steps)
                            : std::nullopt;
    switch (options_.view) {
      case TraceView::kDetailed:
        WriteDetailed(halted, result);
        break;
      case TraceView::kCompact:
        WriteCompact(halted, result);
        break;
    }
    if (steps_ > records_.size()) {
      if (options_.view == TraceView::kDetailed) std::println(*out_);
      automata::WriteHiddenNotice(
          *out_,
          steps_ - records_.size(),
          "step",
          style_
      );
    }
    std::println(*out_);
  }

 private:
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

  [[nodiscard]] std::string TapeName(std::size_t tape) const {
    if (definition_->num_tapes == 1) return "tape";
    return std::format("tape {}", tape + 1);
  }

  /// The reason why the machine halted at `configuration`: no transition
  /// applies.
  [[nodiscard]] std::string HaltReason(
      const Configuration& configuration,
      bool accepted
  ) const {
    std::string reads;
    for (const auto& tape : configuration.tapes) {
      if (!reads.empty()) reads += ", ";
      reads += ReadHead(tape, definition_->blank).name();
    }
    return std::format(
        "no transition reads ({}) in {} {}",
        reads,
        accepted ? "final state" : "state",
        configuration.state
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

  void WriteDetailed(
      std::optional<std::size_t> halted,
      const std::expected<RunResult, LimitReached>& result
  ) const {
    for (std::size_t number = 0; number < records_.size(); ++number) {
      const auto& record = records_[number];
      std::println(*out_);
      const auto id = style_.Paint(Tone::kHeading, std::format("#{}", number));
      if (record.transition) {
        std::println(
            *out_,
            "{}  {} {}",
            id,
            style_.Paint(Tone::kMuted, "by"),
            InlineRule(*record.transition).text
        );
        if (options_.explain) {
          WriteLabeled(
              "",
              style_.Paint(
                  Tone::kMuted,
                  Explain(
                      definition_->transitions[*record.transition],
                      definition_->config.operation_mode
                  )
              )
          );
        }
      } else {
        std::println(*out_, "{}  {}", id, style_.Paint(Tone::kMuted, "start"));
      }
      const auto& configuration = record.configuration;
      WriteLabeled(
          "state",
          style_.Paint(Tone::kStrong, configuration.state.name())
      );
      for (std::size_t tape = 0; tape < configuration.tapes.size(); ++tape) {
        WriteTapeCells(TapeName(tape), configuration.tapes[tape]);
      }
      if (halted == number) {
        const bool accepted = result->accepted;
        WriteLabeled(
            accepted ? "accept" : "reject",
            style_.Paint(
                accepted ? Tone::kGood : Tone::kBad,
                HaltReason(configuration, accepted)
            )
        );
      }
    }
  }

  void WriteTapeCells(std::string_view label, const Tape& tape) const {
    const auto blank = definition_->blank;
    const auto window = WindowOf(tape, blank, kTraceRadius);
    const auto& ellipsis = style_.glyphs().ellipsis;
    std::vector<BoxCell> cells;
    if (window.cut_left) cells.push_back({std::string(ellipsis), Tone::kMuted});
    std::size_t head = 0;
    for (std::size_t index = window.begin; index <= window.end; ++index) {
      const auto symbol = index < tape.cells.size() ? tape.cells[index] : blank;
      if (index == tape.head) head = cells.size();
      cells.push_back({
          std::string(symbol.name()),
          index == tape.head ? Tone::kStrong
          : symbol == blank  ? Tone::kMuted
                             : Tone::kPlain,
      });
    }
    if (window.cut_right) {
      cells.push_back({std::string(ellipsis), Tone::kMuted});
    }
    const auto row = terminal::DrawBoxedRow(cells, style_);
    WriteLabeled("", row.top);
    WriteLabeled(label, row.middle);
    WriteLabeled("", row.bottom);
    WriteLabeled(
        "",
        std::string(row.centers[head], ' ') +
            style_.Paint(Tone::kAccent, style_.glyphs().head)
    );
  }

  void WriteCompact(
      std::optional<std::size_t> halted,
      const std::expected<RunResult, LimitReached>& result
  ) const {
    Table table;
    std::vector<Styled> header{
        style_.Span(Tone::kMuted, "#"),
        style_.Span(Tone::kMuted, "state"),
    };
    for (std::size_t tape = 0; tape < definition_->num_tapes; ++tape) {
      header.push_back(style_.Span(Tone::kMuted, TapeName(tape)));
    }
    header.push_back(style_.Span(Tone::kMuted, "by"));
    table.AddRow(std::move(header));
    for (std::size_t number = 0; number < records_.size(); ++number) {
      const auto& record = records_[number];
      std::vector<Styled> row{
          style_.Span(Tone::kMuted, std::format("#{}", number)),
          style_.Span(Tone::kStrong, record.configuration.state.name()),
      };
      for (const auto& tape : record.configuration.tapes) {
        row.push_back(style_.Span(
            Tone::kPlain,
            FormatTape(tape, definition_->blank, style_, kTraceRadius)
        ));
      }
      row.push_back(record.transition ? Rule(*record.transition) : Styled{});
      if (halted == number) {
        row.push_back(
            result->accepted ? style_.Span(Tone::kGood, "accept")
                             : style_.Span(Tone::kBad, "reject")
        );
      }
      table.AddRow(std::move(row));
    }
    table.Write(*out_);
  }

  std::ostream* out_;
  const Definition* definition_;
  Style style_;
  TraceOptions options_;
  std::vector<RuleText> rules_;
  RuleLayout layout_;
  std::vector<Record> records_;
  std::size_t steps_;
};

}  // namespace

std::string Describe(const Definition& definition, const Style& style) {
  const auto& config = definition.config;
  const auto count = definition.transitions.size();
  const std::vector<std::string> parts{
      std::format("q0 = {}", definition.start_state),
      std::format("b = {}", definition.blank),
      std::format("F = {}", automata::FormatSet(definition.accepting_states)),
      std::format(
          "{} {}",
          definition.num_tapes,
          definition.num_tapes == 1 ? "tape" : "tapes"
      ),
      config.tape_direction == TapeDirection::kBidirectional ? "bidirectional"
                                                             : "right-only",
      config.operation_mode == OperationMode::kSimultaneous ? "simultaneous"
                                                            : "independent",
      config.allow_stay ? "stay allowed" : "no stay",
      std::format("{} {}", count, count == 1 ? "transition" : "transitions"),
  };
  std::string text;
  for (const auto& part : parts) {
    if (!text.empty()) text += style.Separator();
    text += part;
  }
  return text;
}

std::string FormatTransition(
    const Transition& transition,
    OperationMode mode,
    const Style& style
) {
  const auto text = Split(transition, mode);
  return std::format(
      "{} {} {}",
      text.condition,
      style.glyphs().arrow,
      text.result
  );
}

std::string FormatTape(
    const Tape& tape,
    Symbol blank,
    const Style& style,
    std::optional<std::size_t> radius
) {
  const auto window = WindowOf(tape, blank, radius);
  const auto& ellipsis = style.glyphs().ellipsis;
  std::string text(window.cut_left ? ellipsis : "");
  for (std::size_t index = window.begin; index <= window.end; ++index) {
    const auto symbol = index < tape.cells.size() ? tape.cells[index] : blank;
    if (index == tape.head) {
      text += std::format("[{}]", symbol);
    } else {
      text += symbol.name();
    }
  }
  if (window.cut_right) text += ellipsis;
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
      notes.push_back(Explain(transition, definition.config.operation_mode));
    }
  }
  automata::WriteTransitionList(out, SplitAll(definition), notes, style);
}

std::expected<RunResult, LimitReached> TraceRun(
    const Machine& machine,
    std::span<const Symbol> input,
    std::size_t max_steps,
    const TraceOptions& options,
    std::ostream& out,
    const Style& style
) {
  std::vector<Record> records;
  std::size_t steps = 0;
  const auto record = [&](const Step& step) {
    ++steps;
    if (step.number >= options.limit) return;
    records.push_back({
        .configuration = step.configuration,
        .transition = step.transition,
    });
  };
  const auto result =
      machine.Run(input, {.max_steps = max_steps, .observer = record});
  const TraceWriter writer(
      out,
      machine.definition(),
      style,
      options,
      std::move(records),
      steps
  );
  writer.Write(input, result);
  return result;
}

}  // namespace turing
