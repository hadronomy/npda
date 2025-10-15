#include <filesystem>
#include <fstream>
#include <iostream>
#include <string_view>

#include "cli.h"
#include "cli/turing.h"
#include "diag.h"
#include "fmt/color.h"
#include "turing/parser.h"
#include "turing/turing.h"

static std::vector<std::string> to_symbols(std::string_view s) {
  std::vector<std::string> v;
  v.reserve(s.size());
  for (char c : s)
    v.emplace_back(1, c);  // "a" from 'a'
  return v;
}

int TuringHandler::operator()(const CommandContext&) {
  std::filesystem::path filepath = this->file_path;
  std::ifstream file(filepath);

  auto result = turing::parse::parse_with_diagnostics(file, filepath.filename());

  // Show warnings/errors if any
  if (!result.diagnostics.items.empty()) {
    diag::render(
      std::cerr,
      result.source,
      result.diagnostics,
      diag::RenderOptions{.color = true, .context_lines = 0}
    );
    // Terminate if there are any errors
    if (result.diagnostics.has_errors()) {
      return 1;
    }
  }

  // Check for parsing errors (this should be redundant now but kept for safety)
  if (!result.value.has_value()) {
    diag::render(
      std::cerr,
      result.source,
      result.value.error(),
      diag::RenderOptions{.color = true, .context_lines = 0}
    );
    return 1;
  }

  if (auto& tm = result.value; result.value.has_value()) {
    auto run = [&](std::string_view s, bool trace = false) {
      // Create TM configuration from CLI options
      turing::TMConfig config;
      config.num_tapes = this->num_tapes;
      config.tape_direction = this->tape_direction;
      config.operation_mode = this->operation_mode;
      config.allow_stay = this->allow_stay;

      auto r = tm->run(
        to_symbols(s),
        turing::RunOptions{
          .max_steps = 100000,
          .track_witness = true,
          .trace = trace,
          .trace_colors = true,
          .trace_compact = false,
          .trace_explanations = this->explain,
          .show_config = true,
        }
      );
      if (!r) {
        std::cout << s << " -> error: " << r.error().message << "\n";
        return;
      }

      // Format input display - show empty string as "λ" (lambda) for clarity
      std::string input_display = s.empty() ? "λ" : std::string(s);

      // Colorize the result line
      std::string result_line;
      if (r->accepted) {
        result_line = fmt::format(
          fmt::fg(fmt::terminal_color::green),
          "{} -> accepted=true steps={}",
          input_display,
          r->steps
        );
      } else {
        result_line = fmt::format(
          fmt::fg(fmt::terminal_color::red),
          "{} -> accepted=false steps={}",
          input_display,
          r->steps
        );
      }

      std::cout << result_line;

      if (r->witness) {
        std::cout << fmt::format(fmt::fg(fmt::terminal_color::cyan), " witness_rules=[");
        for (std::size_t i = 0; i < r->witness->size(); ++i) {
          std::cout << fmt::format(fmt::fg(fmt::terminal_color::yellow), "{}", (*r->witness)[i]);
          if (i + 1 < r->witness->size()) {
            std::cout << fmt::format(fmt::fg(fmt::terminal_color::cyan), ",");
          }
        }
        std::cout << fmt::format(fmt::fg(fmt::terminal_color::cyan), "]");
      }

      // Show final tape configuration
      if (!r->final_tapes.empty() && !r->final_tapes[0].empty()) {
        std::cout << fmt::format(fmt::fg(fmt::terminal_color::cyan), " tape=\"");

        const auto& tape = r->final_tapes[0];
        std::size_t head_pos = r->final_head_positions[0];

        for (std::size_t i = 0; i < tape.size(); ++i) {
          if (i == head_pos) {
            std::cout << fmt::format(fmt::fg(fmt::terminal_color::yellow), "[{}]", tape[i]);
          } else {
            std::cout << tape[i];
          }
        }

        // Show head position if it's beyond the current tape
        if (head_pos >= tape.size()) {
          std::cout << fmt::format(fmt::fg(fmt::terminal_color::yellow), "[ ]");
        }

        std::cout << "\"";
      }

      std::cout << "\n";
    };

    for (const auto& input_string : input_strings) {
      std::cout << "---------------------------------------------------"
                << "\nShowing \""
                << fmt::format(fmt::fg(fmt::terminal_color::cyan), "{}", input_string)
                << "\" execution in "
                << fmt::format(fmt::fg(fmt::terminal_color::yellow), "{}", file_path.c_str())
                << "\n";
      run(input_string, this->trace_enabled);
    }
    return 0;
  }
  if (!result.value) {
    diag::render(
      std::cerr,
      result.source,
      result.value.error(),
      diag::RenderOptions{.color = true, .context_lines = 0}
    );
    return 1;
  }
  return 0;
}