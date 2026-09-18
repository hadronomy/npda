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

  diag::SourceCache cache;
  auto result = npda::parse::parse_with_diagnostics(file, filepath.string());
  const diag::SourceFile& src = cache.insert(std::move(result.source));

  // Input strings: CLI args, then -in file lines, then keyboard.
  // Empty lines never count as inputs.
  std::vector<std::string> inputs = this->input_strings;
  if (!this->input_file.empty()) {
    std::ifstream inf(this->input_file);
    if (!inf) {
      ui::error(std::format("cannot open input file '{}'", this->input_file.string()));
      return 1;
    }
    std::string line;
    while (std::getline(inf, line)) {
      if (!line.empty() && line.back() == '\r')
        line.pop_back();
      if (!line.empty())
        inputs.push_back(std::move(line));
    }
  }
  if (inputs.empty()) {
    std::string line;
    while (std::getline(std::cin, line)) {
      if (!line.empty() && line.back() == '\r')
        line.pop_back();
      if (!line.empty())
        inputs.push_back(std::move(line));
    }
  }

  // Trace sink: -out file when given, screen otherwise.
  std::ofstream out;
  if (!this->output_file.empty() && this->trace_enabled) {
    out.open(this->output_file);
    if (!out) {
      ui::error(std::format("cannot open output file '{}'", this->output_file.string()));
      return 1;
    }
  }
  std::function<void(std::string_view)> sink;
  if (out.is_open())
    sink = [&](std::string_view s) { out << s; };

  if (auto dpa = result.value; result.value.has_value()) {
    bool failed = false;
    std::size_t n_accepted = 0;
    std::size_t n_rejected = 0;
    std::size_t n_errors = 0;
    double total_secs = 0.0;
    auto run = [&](std::string_view s, bool trace = false) {
      const auto t0 = std::chrono::steady_clock::now();
      auto r = dpa->run(
        to_symbols(s),
        npda::RunOptions{
          .bfs = true,
          .max_expansions = 100000,
          .track_witness = true,
          .trace =
            {.enabled = trace,
             .sink = sink,
             .colors = true,
             .compact = false,
             .explanations = this->explain,
             .show_full_trace = true,
             .box = this->trace_box,
             .tree = this->trace_tree,
             .step_limit = this->trace_limit},
        }
      );
      const double secs = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
      total_secs += secs;
      act.suspend();
      std::cout << ui::rule(
        std::string(filepath.filename().string()) + " : " + ui::truncate_middle(s)
      ) << "\n";
      std::cout << std::format(
        "config: accept={} start={} bottom={}\n", npda::accept_name(dpa->accept_policy()),
        std::format("{}", dpa->start_state()), std::format("{}", dpa->stack_bottom())
      );
      if (!r) {
        std::cout << ansi::format(
          ansi::fg(ansi::terminal_color::red),
          "FAIL [{:7.3f}s] {} {} error: {}",
          secs,
          ui::truncate_middle(s),
          ui::arrow(),
          ui::truncate_middle(r.error().message, 200)
        ) << "\n";
        ++n_errors;
        failed = true;
        act.resume();
        return;
      }

      // Format input display - show empty string as "λ" (lambda) for clarity
      std::string input_display = s.empty() ? "λ" : ui::truncate_middle(s);

      // PASS means the run worked; the accepted word carries the verdict color.
      const auto verdict = r->accepted ? ansi::terminal_color::green : ansi::terminal_color::red;
      if (r->accepted)
        ++n_accepted;
      else
        ++n_rejected;
      std::cout << ansi::format(ansi::fg(ansi::terminal_color::green), "PASS [{:7.3f}s] ", secs)
                << ansi::format(
                     ansi::fg(verdict),
                     "{} {} accepted={} expansions={}",
                     input_display,
                     ui::arrow(),
                     r->accepted,
                     r->expansions
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
      std::cout << "\n";
      // Verdict close. Counts make the run machine-readable.
      std::cout << ui::rule(
        std::string("result: ") + (r->accepted ? "ACCEPT" : "REJECT")
      ) << "\n";
      if (r->accepted) {
        std::cout << ansi::format(
          ansi::fg(ansi::terminal_color::green), "{} accept in {} steps ({} expansions, witness {}, depth {})\n",
          "✓",
          r->witness ? r->witness->size() : 0,
          r->expansions,
          r->witness ? r->witness->size() : 0,
          r->depth
        );
      } else {
        std::cout << ansi::format(
          ansi::fg(ansi::terminal_color::red), "{} reject after {} expansions\n", "✗",
          r->expansions
        );
      }
      std::cout << "\n";
      act.resume();
    };

    act.set_message(std::string("Running ") + filepath.filename().string());
    for (const auto& input_string : inputs) {
      run(input_string, this->trace_enabled);
    }
    std::cout << std::format(
      "Summary: {} inputs run: {} accepted, {} rejected, {} errors in {:.3f}s\n",
      inputs.size(),
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