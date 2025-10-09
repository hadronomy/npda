#pragma once

#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <CLI/CLI.hpp>

#include "colorized_formatter.h"
#include "fmt/color.h"
#include "ui.h"

// A small utility to trim and normalize command names
static std::string normalize_name(std::string_view name) {
  std::string n{name};
  // For brevity, just trim spaces; could also validate characters
  auto l = n.find_first_not_of(' ');
  auto r = n.find_last_not_of(' ');
  if (l == std::string::npos)
    return {};
  return n.substr(l, r - l + 1);
}

// CommandContext carries global state you might want to share across commands.
struct CommandContext {
  bool verbose = false;
};

// Base interface for a command handler. You can extend this for richer lifecycle.
struct CommandHandler {
  virtual ~CommandHandler() = default;
  virtual int operator()(const CommandContext& ctx) = 0;
};

// CommandRegistry: wraps CLI11 app and simplifies subcommand registration.
class CommandRegistry {
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
