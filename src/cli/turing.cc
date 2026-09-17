// Implement TuringHandler in the cli module. Import only.
module cli;

// Import the modules below.
import std;
import ansi;
import diag;
import lex;
import turing;
import turing.parser;
import ui;

int TuringHandler::operator()(const CommandContext& ctx) {
  std::filesystem::path filepath = this->file_path;
  std::ifstream file(filepath);

  diag::RenderOptions opt;
  opt.verbose = ctx.verbose;
  diag::Activity act(std::string("Checking ") + filepath.filename().string());

  diag::SourceCache cache;
  auto result = turing::parse::parse_with_diagnostics(file, filepath.string());
  const diag::SourceFile& src = cache.insert(std::move(result.source));

  // Show warnings/errors if any
  if (!result.diagnostics.items.empty()) {
    act.dismiss();
    diag::render(
      std::cerr,
      cache,
      src.filename,
      result.diagnostics,
      opt
    );
    // Terminate if there are any errors
    if (result.diagnostics.has_errors()) {
      return 1;
    }
  }

  // Check for parsing errors (this should be redundant now but kept for safety)
  if (!result.value.has_value()) {
    act.dismiss();
    diag::render(
      std::cerr,
      cache,
      src.filename,
      result.value.error(),
      opt
    );
    return 1;
  }

  if (auto& tm = result.value; result.value.has_value()) {
    if (this->graphviz) {
      act.dismiss();
      auto new_path = this->file_path.filename().stem().replace_extension("png");
      ui::info(std::format("Writting graphviz image in {}", new_path.c_str()));
      auto exe = this->graphviz_exe;
      if (this->dot_only) {
        auto res = tm->write_graphviz_dot(new_path.replace_extension("dot"));
        if (!res) {
          ui::error(res.error().message);
          return -1;
        }
        return 0;
      }
      auto res = tm->export_graphviz_image(new_path, exe);
      if (!res) {
        ui::error(res.error().message);
        return -1;
      }
      return 0;
    }

    bool failed = false;
    std::size_t n_accepted = 0;
    std::size_t n_rejected = 0;
    std::size_t n_errors = 0;
    double total_secs = 0.0;
    auto run = [&](std::string_view s, bool trace = false) {
      // Create TM configuration from CLI options
      turing::TMConfig config;
      config.num_tapes = this->num_tapes;
      config.tape_direction = this->tape_direction;
      config.operation_mode = this->operation_mode;
      config.allow_stay = this->allow_stay;

      const auto t0 = std::chrono::steady_clock::now();
      auto r = tm->run(
        to_symbols(s),
        turing::RunOptions{
          .max_steps = 100000,
          .track_witness = true,
          .trace =
            {.enabled = trace,
             .colors = true,
             .explanations = this->explain,
             .step_limit = this->trace_limit},
          .show_config = false,
        }
      );
      const double secs = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
      total_secs += secs;
      act.suspend();
      std::cout << ui::rule(
        std::string(filepath.filename().string()) + " : " + ui::truncate_middle(s)
      ) << "\n";
      if (!r) {
        std::cout << ansi::format(
          ansi::fg(ansi::terminal_color::red),
          "FAIL [{:7.3f}s] {} -> error: {}",
          secs,
          ui::truncate_middle(s),
          ui::truncate_middle(r.error().message, 200)
        ) << "\n";
        ++n_errors;
        failed = true;
        act.resume();
        return;
      }

      // Format input display - show empty string as "λ" (lambda) for clarity
      std::string input_display = s.empty() ? "λ" : ui::truncate_middle(s);

      // Colorize the result line. PASS means the run worked;
      // the accepted word carries the verdict color.
      const auto verdict = r->accepted ? ansi::terminal_color::green : ansi::terminal_color::red;
      if (r->accepted)
        ++n_accepted;
      else
        ++n_rejected;
      std::cout << ansi::format(ansi::fg(ansi::terminal_color::green), "PASS [{:7.3f}s] ", secs)
                << ansi::format(
                     ansi::fg(verdict),
                     "{} -> accepted={} steps={}",
                     input_display,
                     r->accepted,
                     r->steps
                   )
                << "\n";

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

      // Show final tape configuration. Huge tapes print as a summary
      // so one run cannot flood the terminal.
      if (!r->final_tapes.empty() && !r->final_tapes[0].empty()) {
        std::cout << ansi::format(ansi::fg(ansi::terminal_color::cyan), " tape=\"");

        const auto& tape = r->final_tapes[0];
        std::size_t head_pos = r->final_head_positions[0];

        if (tape.size() > 200) {
          std::string plain;
          for (std::size_t i = 0; i < tape.size(); ++i) {
            if (i == head_pos)
              plain += "[";
            plain += tape[i];
            if (i == head_pos)
              plain += "]";
          }
          if (head_pos >= tape.size())
            plain += "[ ]";
          std::cout << ui::truncate_middle(plain, 80) << "\" cells=" << tape.size();
        } else {
          for (std::size_t i = 0; i < tape.size(); ++i) {
            if (i == head_pos) {
              std::cout << ansi::format(ansi::fg(ansi::terminal_color::yellow), "[{}]", tape[i]);
            } else {
              std::cout << tape[i];
            }
          }

          // Show head position if it's beyond the current tape
          if (head_pos >= tape.size()) {
            std::cout << ansi::format(ansi::fg(ansi::terminal_color::yellow), "[ ]");
          }

          std::cout << "\"";
        }
      }

      std::cout << "\n";
      act.resume();
    };

    act.set_message(std::string("Running ") + filepath.filename().string());
    act.suspend();
    tm->show_configuration(turing::RunOptions{.show_config = true});
    act.resume();
    for (const auto& input_string : input_strings) {
      run(input_string, this->trace_enabled);
    }
    std::cout << std::format(
      "Summary: {} inputs run: {} accepted, {} rejected, {} errors in {:.3f}s\n",
      input_strings.size(),
      n_accepted,
      n_rejected,
      n_errors,
      total_secs
    );
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
      cache,
      src.filename,
      result.value.error(),
      opt
    );
    return 1;
  }
  act.dismiss();
  return 0;
}