#pragma once

#include <fmt/color.h>
#include <fmt/format.h>
#include <functional>
#include <optional>
#include <string>
#include <vector>
#include "config.h"
#include "rule.h"

namespace npda {

// Text wrapping utility
inline std::string wrap_text(const std::string& text, std::size_t width = 35) {
  std::string result;
  std::size_t start = 0;

  while (start < text.length()) {
    std::size_t end = start + width;
    if (end >= text.length()) {
      result += text.substr(start);
      break;
    }

    // Find the last space before the width limit
    std::size_t last_space = text.rfind(' ', end);
    if (last_space == std::string::npos || last_space <= start) {
      // No space found, break at width
      result += fmt::format("{}\n", text.substr(start, width));
      start += width;
    } else {
      // Break at the last space
      result += fmt::format("{}\n", text.substr(start, last_space - start));
      start = last_space + 1;
    }
  }

  return result;
}

// Rule explanation with colorization
template <Hashable State, Hashable Input, Hashable StackSym>
std::string explain_rule(const Rule<State, Input, StackSym>& r) {
  std::string explanation;

  // Build colored explanation parts
  std::string header =
    fmt::format(fmt::fg(config::colors::section_heading), "Explanation: Transition: ");
  explanation += header;

  // From state (colored as command name)
  std::string from_state_colored =
    fmt::format(fmt::fg(config::colors::command_name), "'{}'", r.from);
  explanation += fmt::format("From state {} ", from_state_colored);

  // Input condition
  if (r.input.has_value()) {
    std::string input_colored =
      fmt::format(fmt::fg(config::colors::warning), "'{}'", r.input.value());
    explanation += fmt::format("when reading {} ", input_colored);
  } else {
    explanation +=
      fmt::format(fmt::fg(config::colors::info), "without consuming input (ε-transition) ");
  }

  // Stack condition
  if (r.stack_top.has_value()) {
    std::string stack_colored =
      fmt::format(fmt::fg(config::colors::warning), "'{}'", r.stack_top.value());
    explanation += fmt::format("with {} on top of stack ", stack_colored);
  } else {
    explanation += fmt::format(fmt::fg(config::colors::info), "regardless of stack contents ");
  }

  // Action and destination (arrow colored as example, state as command)
  std::string to_state_colored = fmt::format(fmt::fg(config::colors::command_name), "'{}'", r.to);
  std::string arrow_colored = fmt::format(fmt::fg(config::colors::example), "→");
  explanation += arrow_colored;
  explanation += fmt::format(" move to state {} ", to_state_colored);

  // Stack operation with colorized symbols
  if (r.stack_top.has_value() && !r.push.empty()) {
    std::string stack_top_colored =
      fmt::format(fmt::fg(config::colors::warning), "'{}'", r.stack_top.value());
    explanation += fmt::format(" and replace {} with ", stack_top_colored);
    for (std::size_t i = 0; i < r.push.size(); ++i) {
      std::string push_sym_colored =
        fmt::format(fmt::fg(config::colors::warning), "'{}'", r.push[i]);
      explanation += push_sym_colored;
      if (i + 1 < r.push.size())
        explanation += ", ";
    }
  } else if (r.stack_top.has_value()) {
    std::string stack_top_colored =
      fmt::format(fmt::fg(config::colors::warning), "'{}'", r.stack_top.value());
    explanation += fmt::format(" and pop {} from stack", stack_top_colored);
  } else if (!r.push.empty()) {
    explanation += " and push ";
    if (r.push.size() == 1) {
      std::string push_sym_colored =
        fmt::format(fmt::fg(config::colors::warning), "'{}'", r.push[0]);
      explanation += fmt::format("{} onto stack", push_sym_colored);
    } else {
      explanation += "'";
      for (std::size_t i = 0; i < r.push.size(); ++i) {
        std::string push_sym_colored =
          fmt::format(fmt::fg(config::colors::warning), "'{}'", r.push[i]);
        explanation += push_sym_colored;
        if (i + 1 < r.push.size())
          explanation += ", ";
      }
      explanation += "' onto stack";
    }
  } else {
    explanation += fmt::format(fmt::fg(config::colors::info), " without changing stack");
  }

  explanation += ".";
  return wrap_text(explanation, 80);
}

}  // namespace npda