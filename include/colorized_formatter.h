#pragma once

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/color.h>
#include <fmt/core.h>
#include <CLI/CLI.hpp>

#include "config.h"

namespace npda {

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

  // Identify CLI11 Option_group subcommands (via RTTI)
  [[nodiscard]] static bool is_option_group(const CLI::App* app) {
#if CLI11_USE_STATIC_RTTI == 0
    return dynamic_cast<const CLI::Option_group*>(app) != nullptr;
#else
    // If static RTTI only, fall back to a heuristic:
    // Option_group subcommands typically have empty name and hold options.
    return app->get_name().empty() && !app->get_options().empty();
#endif
  }
};

}  // namespace npda