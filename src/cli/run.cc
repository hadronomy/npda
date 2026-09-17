// Implement RunHandler in the cli module. Import only.
module cli;

// Import the modules below.
import std;
import ansi;
import diag;
import lex;
import npda;
import npda.parser;
import ui;

int RunHandler::operator()(const CommandContext& ctx) {
  std::filesystem::path filepath = this->file_path;
  std::ifstream file(filepath);

  diag::RenderOptions opt;
  opt.verbose = ctx.verbose;
  diag::Activity act(std::string("Checking ") + filepath.filename().string());

  auto result = npda::parse::parse_with_diagnostics(file, filepath.filename());
  if (auto dpa = result.value; result.value.has_value()) {
    bool failed = false;
    auto run = [&](std::string_view s, bool trace = false) {
      auto r = dpa->run(
        to_symbols(s),
        npda::RunOptions{
          .bfs = true,
          .max_expansions = 100000,
          .track_witness = true,
          .trace =
            {.enabled = trace,
             .colors = true,
             .compact = false,
             .explanations = this->explain,
             .show_full_trace = true},
        }
      );
      act.suspend();
      std::cout << "---------------------------------------------------"
                << "\nShowing \""
                << ansi::format(ansi::fg(ansi::terminal_color::cyan), "{}", ui::truncate_middle(s))
                << "\" execution in "
                << ansi::format(ansi::fg(ansi::terminal_color::yellow), "{}", file_path.c_str())
                << "\n";
      if (!r) {
        std::cout << ui::truncate_middle(s) << " -> error: " << r.error().message << "\n";
        failed = true;
        act.resume();
        return;
      }

      // Format input display - show empty string as "λ" (lambda) for clarity
      std::string input_display = s.empty() ? "λ" : ui::truncate_middle(s);

      // Colorize the result line
      std::string result_line;
      if (r->accepted) {
        result_line = ansi::format(ansi::fg(ansi::terminal_color::green), "{} -> accepted=true expansions={}", input_display, r->expansions);
      } else {
        result_line = ansi::format(ansi::fg(ansi::terminal_color::red), "{} -> accepted=false expansions={}", input_display, r->expansions);
      }

      std::cout << result_line;

      if (r->witness) {
        std::cout << ansi::format(ansi::fg(ansi::terminal_color::cyan), " witness_rules=[");
        for (std::size_t i = 0; i < r->witness->size(); ++i) {
          std::cout << ansi::format(ansi::fg(ansi::terminal_color::yellow), "{}", (*r->witness)[i]);
          if (i + 1 < r->witness->size()) {
            std::cout << ansi::format(ansi::fg(ansi::terminal_color::cyan), ",");
          }
        }
        std::cout << ansi::format(ansi::fg(ansi::terminal_color::cyan), "]");
      }
      std::cout << "\n";
      act.resume();
    };

    for (const auto& input_string : input_strings) {
      run(input_string, this->trace_enabled);
    }
    if (failed)
      act.fail(std::string("Failed ") + filepath.filename().string());
    else
      act.finish(std::string("Finished ") + filepath.filename().string());
    return 0;
  }
  if (!result.value) {
    act.dismiss();
    diag::render(
      std::cerr,
      result.source,
      result.value.error(),
      opt
    );
    return 1;
  }
  act.dismiss();
  return 0;
}