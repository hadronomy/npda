// Own the CLI layer: registry, help formatter, and handlers.
// fmt stays in the global fragment above the module line.
// Import everything else. Change this file to change the API.
module;
#include <fmt/color.h>
#include <fmt/core.h>

export module cli;

// Import below. cli11 and config come from wrapper modules,
// the rest from the standard library.
import std;
import cli11;
import config;
import ui;
import turing;
import prf;

// Trim spaces from both ends. Internal use only.
inline std::string normalize_name(std::string_view name) {
  std::string n{name};
  // For brevity, just trim spaces; could also validate characters
  auto l = n.find_first_not_of(' ');
  auto r = n.find_last_not_of(' ');
  if (l == std::string::npos)
    return {};
  return n.substr(l, r - l + 1);
}

export namespace npda {

/**
 * @class ColorizedFormatter
 * @brief Custom formatter for CLI11 help text with enhanced colorization,
 *        including explicit support for CLI11 Option_group blocks.
 */
class ColorizedFormatter : public CLI::Formatter {
 public:
  ColorizedFormatter() : CLI::Formatter() {
    CLI::Formatter::column_width(35);
    label("OPTIONS", "");
    label("COMMANDS", "");
    // Ensure required marker shows as "(REQUIRED)"
    label("REQUIRED", "(REQUIRED)");
  }

  // Public API preserved
  [[nodiscard]] std::size_t column_width() const { return column_width_; }

  // Colorize the fully composed option line to preserve CLI11 alignment.
  std::string make_option(const CLI::Option* opt, bool is_positional) const override {
    std::string base = CLI::Formatter::make_option(opt, is_positional);
    if (base.empty())
      return base;

    // Split base line into "left" (names + opts) and "right" (description)
    const std::size_t split = find_help_split_pos(base);

    if (split != std::string::npos) {
      const std::string left = base.substr(0, split);
      const std::string right = base.substr(split);  // spaces + description
      // Do NOT add extra spaces here; base already has indentation/alignment
      return fmt::format(fg(config::colors::option_name), "{}", left) +
             fmt::format(fg(config::colors::info), "{}", right);
    }

    // Fallback: color everything uniformly if we couldn't find a split
    return fmt::format(fg(config::colors::info), "{}", base);
  }

  std::string make_subcommand(const CLI::App* app) const override {
    const std::string& name = app->get_name();
    const std::string& desc = app->get_description();
    // Simple, readable list for commands
    return fmt::format(fg(config::colors::command_name), "  {:<25}", name) + desc + "\n";
  }

  // Dedicated positionals section
  std::string make_positionals(const CLI::App* app) const override {
    const auto pos = positional_options(app);
    if (pos.empty())
      return {};

    std::string out;
    out += format_section_header("ARGUMENTS");
    for (const CLI::Option* opt : pos) {
      out += make_option(opt, /*is_positional=*/true);
    }
    return out;
  }

  std::string make_group(
    std::string group,
    bool is_positional,
    std::vector<const CLI::Option*> opts
  ) const override {
    if (group.empty()) {
      group = is_positional ? "Positional Arguments" : "Options";
    }

    std::string out;
    out.reserve(64 + opts.size() * 32);
    out += format_section_header(group);
    for (const CLI::Option* opt : opts) {
      out += make_option(opt, is_positional);
    }
    return out;
  }

  std::string make_subcommands(const CLI::App* app, CLI::AppFormatMode /*mode*/) const override {
    // Hide CLI11 Option_group "subcommands" from the COMMANDS list
    std::vector<const CLI::App*> subs = app->get_subcommands([&](const CLI::App* sub) {
      // Skip option groups (identified via RTTI) and also nameless entries
      if (is_option_group(sub))
        return false;
      if (sub->get_name().empty())
        return false;
      return true;
    });

    std::string out;
    if (!subs.empty()) {
      constexpr std::string_view group = "COMMANDS";
      out += format_section_header(group);
      for (const CLI::App* sub : subs) {
        out += make_subcommand(sub);
      }
    }
    return out;
  }

  std::string make_help(const CLI::App* app, std::string /*name*/, CLI::AppFormatMode mode)
    const override {
    std::string out;
    out.reserve(512);

    const auto title_style = fg(config::colors::banner_text) | fmt::emphasis::bold;
    const auto version_style = fg(config::colors::info);
    const auto usage_style = fg(config::colors::usage);

    // Header: "<root_name> is <description> (<version>)"
    out += "\n";
    out += fmt::format(title_style, "{}", root_name(app));
    out += " is " + root(app)->get_description();
    out += fmt::format(version_style, " ({})\n", config::app_version);

    // Usage
    const std::string command_path = full_command_path(app);
    std::string usage_tail_str = build_usage_tail(app);

    if (!command_path.empty()) {
      out += "\n";
      out += fmt::format(
        usage_style,
        "Usage: {}{}{}",
        command_path,
        usage_tail_str.empty() ? "" : " ",
        usage_tail_str
      );
      out += "\n";
    }

    // Order per docs: positionals, option groups, subcommands
    out += make_positionals(app);
    out += make_groups(app, mode);
    out += make_option_groups(app);
    out += make_subcommands(app, mode);

    // Examples (placeholder)
    if (!app->get_subcommands({}).empty()) {
      constexpr std::string_view group = "EXAMPLES";
      out += format_section_header(group);
      out += fmt::format(fg(config::colors::example), "  TODO \n");
    }

    // Footer
    out += "\nLearn more: ";
    out += fmt::format(fg(config::colors::banner_text), "{}\n", config::repo_url);

    return out;
  }

  // Keep these public to preserve the existing API
  [[nodiscard]] const CLI::App* root(const CLI::App* app) const {
    const CLI::App* current = app;
    while (current->get_parent()) {
      current = current->get_parent();
    }
    return current;
  }

  [[nodiscard]] const std::string root_name(const CLI::App* app) const {
    return root(app)->get_name();
  }

 private:
  // Find a visually good split point between the left column and description
  // AFTER base make_option has composed the line.
  [[nodiscard]] static std::size_t find_help_split_pos(std::string_view s) {
    // Prefer the large gap of spaces between columns (>= 3 spaces)
    std::vector<std::size_t> runs;
    runs.reserve(2);

    std::size_t i = 0;
    while (i < s.size()) {
      if (s[i] == ' ') {
        std::size_t j = i + 1;
        while (j < s.size() && s[j] == ' ')
          ++j;
        const std::size_t run_len = j - i;
        if (run_len >= 3) {
          runs.push_back(i);
          if (runs.size() >= 1)
            break;  // first wide gap is enough
        }
        i = j;
      } else {
        ++i;
      }
    }

    if (!runs.empty())
      return runs.front();
    // Fallback: try the common " - " delimiter used by some formatters
    if (const std::size_t pos = s.find(" - "); pos != std::string::npos) {
      return pos;
    }
    return std::string::npos;
  }

  // Format a colored section header with underline.
  [[nodiscard]] static std::string format_section_header(std::string_view title) {
    std::string out;
    out.reserve(title.size() * 2 + 16);

    const auto head_style = fg(config::colors::section_heading) | fmt::emphasis::bold;
    const auto line_style = fg(config::colors::section_heading);

    out += fmt::format(head_style, "\n{}\n", title);
    out += fmt::format(line_style, "{}\n\n", std::string(title.size(), '-'));
    return out;
  }

  // Build the full command path from the root to the given app node.
  // Fallback to "app" if any name is empty (preserving original behavior).
  [[nodiscard]] std::string full_command_path(const CLI::App* app) const {
    std::vector<const CLI::App*> chain;
    for (const CLI::App* cur = app; cur != nullptr; cur = cur->get_parent()) {
      chain.push_back(cur);
    }
    std::reverse(chain.begin(), chain.end());  // root -> ... -> app

    std::string path;
    path.reserve(64);

    bool first = true;
    for (const CLI::App* node : chain) {
      const std::string& nm = node->get_name();
      const std::string& piece = nm.empty() ? std::string("app") : nm;
      if (!first)
        path.push_back(' ');
      path += piece;
      first = false;
    }
    return path;
  }

  // Render CLI11 Option Groups (Option_group subcommands), separating
  // positionals from non-positional options within each group.
  [[nodiscard]] std::string make_option_groups(const CLI::App* app) const {
    // Collect only Option_group instances attached to this app
    std::vector<const CLI::App*> groups =
      app->get_subcommands([&](const CLI::App* sub) { return is_option_group(sub); });

    if (groups.empty())
      return {};

    std::string out;

    for (const CLI::App* grp : groups) {
      // Collect visible options in the group
      std::vector<const CLI::Option*> all_opts = grp->get_options([&](const CLI::Option* opt) {
        return opt != nullptr && !is_effectively_hidden(opt);
      });
      if (all_opts.empty())
        continue;

      // Partition into positionals and non-positionals
      std::vector<const CLI::Option*> pos_opts;
      std::vector<const CLI::Option*> nonpos_opts;
      pos_opts.reserve(all_opts.size());
      nonpos_opts.reserve(all_opts.size());
      for (const CLI::Option* o : all_opts) {
        (o->get_positional() ? pos_opts : nonpos_opts).push_back(o);
      }

      // Section header: "[Option Group: <group_name>]"
      // CLI11's Option_group uses App::group_ to store the display name
      const std::string header = fmt::format("[Option Group: {}]", grp->get_group());
      out += format_section_header(header);

      // Requirement constraints (min/max) if any
      const std::size_t min_req = grp->get_require_option_min();
      const std::size_t max_req = grp->get_require_option_max();
      if (min_req > 0 || max_req != 0) {
        out += make_requirement_line(min_req, max_req);
        out += "\n";
      }

      // Render positionals (if any)
      if (!pos_opts.empty()) {
        out += format_section_header("POSITIONALS");
        for (const CLI::Option* opt : pos_opts) {
          out += make_option(opt, /*is_positional=*/true);
        }
      }

      // Render non-positional options (if any)
      if (!nonpos_opts.empty()) {
        out += format_section_header("OPTIONS");
        for (const CLI::Option* opt : nonpos_opts) {
          out += make_option(opt, /*is_positional=*/false);
        }
      }
    }

    return out;
  }

  // Build a human-readable requirement line for an option group.
  [[nodiscard]] std::string make_requirement_line(std::size_t min_req, std::size_t max_req) const {
    const auto info = fg(config::colors::info) | fmt::emphasis::bold;

    auto push = [&](const std::string& s) {
      return fmt::format(info, "  [{}]\n", s);
    };

    if (min_req == 0 && max_req == 1) {
      return push("At most 1 of the following options may be provided");
    } else if (min_req == 1 && max_req == 0) {
      return push("At least 1 of the following options is required");
    } else if (min_req == 1 && max_req == 1) {
      return push("Exactly 1 of the following options are required");
    } else if (min_req == 0 && max_req == 0) {
      // No constraints
      return std::string{};
    } else if (max_req == 0) {
      return push(fmt::format("At least {} of the following options are required", min_req));
    } else if (min_req == 0) {
      return push(fmt::format("At most {} of the following options may be provided", max_req));
    } else {
      return push(
        fmt::format("Between {} and {} of the following options are required", min_req, max_req)
      );
    }
  }

  // Compose a concise, clean usage tail.
  [[nodiscard]] std::string build_usage_tail(const CLI::App* app) const {
    std::string tail;
    if (has_non_positional_options(app)) {
      tail = "[OPTIONS]";
    }
    for (const CLI::Option* opt : positional_options(app)) {
      const std::string placeholder = inferred_placeholder(opt);
      if (!tail.empty())
        tail.push_back(' ');
      tail += placeholder;
    }
    return tail;
  }

  [[nodiscard]] static std::vector<const CLI::Option*> positional_options(const CLI::App* app) {
    std::vector<const CLI::Option*> pos;
    pos.reserve(app->get_options().size());
    for (const CLI::Option* opt : app->get_options()) {
      if (opt->get_positional() && !is_effectively_hidden(opt)) {
        pos.push_back(opt);
      }
    }
    return pos;
  }

  [[nodiscard]] static bool is_effectively_hidden(const CLI::Option* opt) {
    // Common pattern: hidden options live in a "Hidden" or "Internal" group
    const std::string grp = opt->get_group();
    if (grp.empty())
      return false;

    std::string upper;
    upper.reserve(grp.size());
    for (char c : grp)
      upper.push_back(static_cast<char>(std::toupper(c)));

    return (upper == "HIDDEN" || upper == "INTERNAL");
  }

  [[nodiscard]] static bool has_non_positional_options(const CLI::App* app) {
    for (const CLI::Option* opt : app->get_options()) {
      if (!opt->get_positional() && !is_effectively_hidden(opt)) {
        return true;
      }
    }
    return false;
  }

  // Try to infer a reasonable placeholder for a positional argument in usage.
  [[nodiscard]] static std::string inferred_placeholder(const CLI::Option* opt) {
    std::string tn = opt->get_type_name();
    if (!tn.empty())
      return tn;
    std::string n = opt->get_name();
    if (!n.empty())
      return n;
    return "<arg>";
  }

  // Pinned CLI11 v2.5.0 sets CLI11_USE_STATIC_RTTI to 0, so keep the
  // dynamic_cast branch only. Macros do not cross module boundaries.
  [[nodiscard]] static bool is_option_group(const CLI::App* app) {
    return dynamic_cast<const CLI::Option_group*>(app) != nullptr;
  }
};

}  // namespace npda
// CommandContext carries global state you might want to share across commands.
export struct CommandContext {
  bool verbose = false;
};

// Base interface for a command handler. You can extend this for richer lifecycle.
export struct CommandHandler {
  virtual ~CommandHandler() = default;
  virtual int operator()(const CommandContext& ctx) = 0;
};

// CommandRegistry: wraps CLI11 app and simplifies subcommand registration.
export class CommandRegistry {
 public:
  explicit CommandRegistry(std::string app_name, std::string desc = {})
      : app_(desc, std::move(app_name)), description_(std::move(desc)) {
    app_.description(description_);
    app_.require_subcommand(1);  // require at least one subcommand
    app_.set_help_all_flag("--help-all", "Show help for all subcommands");
    app_.allow_extras(false);
    app_.formatter(std::make_shared<npda::ColorizedFormatter>());

    // Global options
    app_.add_flag("-v,--verbose", ctx_.verbose, "Enable verbose output");
  }

  // Register a command with a factory that returns a unique_ptr<ICommandHandler>.
  // The builder function is given a CLI::App& to define its options/args.
  template <typename Factory>
  CLI::App* register_command(std::string name, std::string description, Factory&& factory) {
    auto cmd_name = normalize_name(name);
    if (cmd_name.empty()) {
      throw std::invalid_argument("Command name cannot be empty");
    }
    if (subcommands_.contains(cmd_name)) {
      throw std::invalid_argument("Duplicate command: " + cmd_name);
    }

    auto* sub = app_.add_subcommand(cmd_name, std::move(description));
    sub->fallthrough(false);
    sub->allow_extras(false);

    // Create the handler via the factory and keep it alive.
    auto handler = factory(*sub);
    if (!handler) {
      throw std::runtime_error("Factory returned null handler for " + cmd_name);
    }
    // Bind callback to the subcommand
    sub->callback([this, h = handler.get()]() { exit_code_ = (*h)(ctx_); });

    subcommands_.emplace(cmd_name, Subcommand{.app = sub, .handler = std::move(handler)});
    return sub;
  }

  // Convenience: register a simple alias that forwards to an existing command.
  CLI::App* register_alias(std::string alias, std::string target) {
    auto a = normalize_name(alias);
    auto t = normalize_name(target);
    if (!subcommands_.contains(t)) {
      throw std::invalid_argument("Target command not found: " + t);
    }
    auto* sub = app_.add_subcommand(a, "Alias for '" + t + "'");
    sub->callback([this, t]() {
      // Reconstruct to run the target handler. In practice you might
      // prefer to parse argv differently, but here we simply call it.
      auto it = subcommands_.find(t);
      if (it != subcommands_.end()) {
        exit_code_ = (*(it->second.handler))(ctx_);
      } else {
        throw std::runtime_error("Alias target missing at runtime");
      }
    });
    aliases_.push_back({a, t});
    return sub;
  }

  // Register a meta command that lists available commands.
  void register_list_command() {
    register_command(
      "list",
      "List available commands",
      [this](CLI::App& sub) -> std::unique_ptr<CommandHandler> {
        struct ListHandler : CommandHandler {
          const CommandRegistry* reg{};
          explicit ListHandler(const CommandRegistry* r) : reg(r) {}
          int operator()(const CommandContext&) override {
            std::cout << "Available commands:\n";
            for (const auto& [name, sc] : reg->subcommands_) {
              std::cout << "  " << name << "  - " << sc.app->get_description() << "\n";
            }
            if (!reg->aliases_.empty()) {
              std::cout << "\nAliases:\n";
              for (const auto& [a, t] : reg->aliases_) {
                std::cout << "  " << a << " -> " << t << "\n";
              }
            }
            return 0;
          }
        };
        (void)sub;
        return std::make_unique<ListHandler>(this);
      }
    );
  }

  // Parse and execute. Returns the command's exit code or non-zero on errors.
  int run(int argc, char** argv) {
    try {
      app_.parse(argc, argv);
    } catch (const CLI::ParseError& e) {
      if (e.get_name() == "RuntimeError")
        return 1;
      if (e.get_name() == "CallForHelp") {
        std::cout << app_.help();
        return 1;
      }
      if (e.get_name() == "CallForAllHelp") {
        std::cout << app_.help("", CLI::AppFormatMode::All);
        return 1;
      }
      if (e.get_name() == "CallForVersion") {
        std::cout << e.what() << '\n';
        return e.get_exit_code();
      }
      std::stringstream sstream;
      sstream << e.what() << "\n"
              << "\x1b[0m"
              << "Run with " << fmt::format(fmt::fg(fmt::terminal_color::cyan), "--help")
              << " to see more information\n";
      ui::error(sstream.str());
      return 1;
    }
    return exit_code_;
  }

  CLI::App& app() { return app_; }
  const CLI::App& app() const { return app_; }

 private:
  struct Subcommand {
    CLI::App* app{};
    std::unique_ptr<CommandHandler> handler{};
  };

  CLI::App app_;
  std::string description_;
  CommandContext ctx_{};
  std::map<std::string, Subcommand> subcommands_{};
  std::vector<std::pair<std::string, std::string>> aliases_{};
  int exit_code_{0};
};

// NPDA command handler. Owns its options and runs the machine.
export class RunHandler final : public CommandHandler {
 public:
  std::filesystem::path file_path;
  std::vector<std::string> input_strings;
  bool trace_enabled;
  bool explain;

  int operator()(const CommandContext& ctx) override;
};

export std::unique_ptr<CommandHandler> make_npda(CLI::App& sub) {
  auto handler = std::make_unique<RunHandler>();
  sub.add_option("file_path", handler->file_path, "the NPDA description file path")
    ->required()
    ->check(cli11::ExistingFile);
  sub.add_option("input_string", handler->input_strings, "the string to accept")
    ->multi_option_policy(CLI::MultiOptionPolicy::TakeAll)
    ->required();
  sub.add_flag("--trace,!--no-trace", handler->trace_enabled, "Disable trace mode");
  sub.add_flag("--explain", handler->explain, "Enable explanations of the transitions");
  return handler;
}

// Turing command handler. Owns its options and runs the machine.
export class TuringHandler final : public CommandHandler {
 public:
  std::filesystem::path file_path;
  std::vector<std::string> input_strings;
  bool trace_enabled = false;
  bool explain = false;
  bool graphviz = false;
  std::string graphviz_exe = "dot";
  bool dot_only = false;

  // Configuration options
  std::size_t num_tapes = 1;
  turing::TapeDirection tape_direction = turing::TapeDirection::Bidirectional;
  turing::OperationMode operation_mode = turing::OperationMode::Simultaneous;
  bool allow_stay = true;

  int operator()(const CommandContext& ctx) override;
};

export std::unique_ptr<CommandHandler> make_turing(CLI::App& sub) {
  auto handler = std::make_unique<TuringHandler>();
  auto grp = sub.add_option_group("mode");
  sub.add_option("file_path", handler->file_path, "the Turing Machine description file path")
    ->required()
    ->check(cli11::ExistingFile);
  grp->add_option("input_string", handler->input_strings, "the string to process")
    ->multi_option_policy(CLI::MultiOptionPolicy::TakeAll);
  sub.add_flag("--trace,!--no-trace", handler->trace_enabled, "Enable trace mode");
  sub.add_flag("--explain", handler->explain, "Enable explanations of transitions");

  // Configuration options
  sub.add_option("--num-tapes", handler->num_tapes, "Number of tapes (default: 1)")
    ->check(cli11::PositiveNumber);
  sub.add_option(
    "--tape-direction", handler->tape_direction, "Tape direction: bidirectional or right-only"
  );
  sub.add_option(
    "--operation-mode", handler->operation_mode, "Operation mode: Simultaneous or Independent"
  );
  sub.add_flag(
    "--allow-stay,!--no-stay", handler->allow_stay, "Allow stay movement (S) or only L/R"
  );
  grp->add_flag("--graphviz,-g", handler->graphviz, "Export graphviz image");
  sub.add_option("--exe,-e", handler->graphviz_exe, "The graphviz executable to use")
    ->needs("--graphviz");
  sub
    .add_flag(
      "--dot-only", handler->dot_only, "Whether to only output the graphviz file and no image"
    )
    ->needs("--graphviz");
  grp->require_option(1, 1);
  return handler;
}

// PRF command handler. Owns its options and runs pow over the inputs.
export class PRFHandler final : public CommandHandler {
 public:
  std::vector<std::uint64_t> params;
  prf::Trace::Mode mode = prf::Trace::Mode::CountsOnly;

  int operator()(const CommandContext& ctx) override;
};

export std::unique_ptr<CommandHandler> make_prf(CLI::App& sub) {
  auto handler = std::make_unique<PRFHandler>();
  sub.add_option("params", handler->params, "the prf parameters for execution")
    ->expected(2, 2)
    ->required();
  const std::map<std::string, prf::Trace::Mode> mode_map{
    {"off", prf::Trace::Mode::Off},
    {"full", prf::Trace::Mode::Full},
    {"counts-only", prf::Trace::Mode::CountsOnly},
  };

  sub.add_option("--mode", handler->mode, "Trace mode")
    ->transform(
      CLI::CheckedTransformer(mode_map, CLI::ignore_case).description("off|full|counts-only")
    );
  return handler;
}
