#include <filesystem>
#include <fstream>
#include <iostream>
#include <string_view>

#include "cli.h"
#include "diag.h"
#include "fmt/color.h"
#include "npda.h"
#include "parser.h"

#include "cli/run.h"

static std::vector<std::string> to_symbols(std::string_view s) {
  std::vector<std::string> v;
  v.reserve(s.size());
  for (char c : s)
    v.emplace_back(1, c);  // "a" from 'a'
  return v;
}

int RunHandler::operator()(const CommandContext&) {
  std::filesystem::path filepath = this->file_path;
  std::ifstream file(filepath);

  auto result = npda::parse::parse_with_diagnostics(file, filepath.filename());
  if (auto dpa = result.value; result.value.has_value()) {
    auto run = [&](std::string_view s, bool trace = false) {
      auto r = dpa->run(
        to_symbols(s),
        npda::RunOptions{
          .bfs = true,
          .max_expansions = 100000,
          .track_witness = true,
          .trace = trace,
          .trace_colors = true,
          .trace_compact = false,
          .show_full_trace = true,
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
          "{} -> accepted=true expansions={}",
          input_display,
          r->expansions
        );
      } else {
        result_line = fmt::format(
          fmt::fg(fmt::terminal_color::red),
          "{} -> accepted=false expansions={}",
          input_display,
          r->expansions
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