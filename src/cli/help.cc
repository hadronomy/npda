#include "cli/help.h"

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <format>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <CLI/CLI.hpp>

#include "terminal/capabilities.h"
#include "terminal/style.h"

namespace cli {

namespace {

using terminal::Style;
using terminal::Styled;
using terminal::Tone;

constexpr std::string_view kVersion = "0.2.0";
constexpr std::string_view kRepository = "https://github.com/hadronomy/npda";

struct Entry {
  Styled name;
  std::string description;
  std::string note;
};

std::string CommandPath(const CLI::App* app) {
  std::vector<std::string> names;
  for (const auto* node = app; node != nullptr; node = node->get_parent()) {
    names.push_back(node->get_name());
  }
  std::ranges::reverse(names);
  std::string path;
  for (const auto& name : names) {
    if (!path.empty()) path += ' ';
    path += name;
  }
  return path;
}

std::string OptionNames(const CLI::Option* option) {
  std::string names;
  for (const auto& name : option->get_snames()) {
    names += std::format("{}-{}", names.empty() ? "" : ", ", name);
  }
  for (const auto& name : option->get_lnames()) {
    names += std::format("{}--{}", names.empty() ? "" : ", ", name);
  }
  return names;
}

/// Returns `INPUT...` for a repeatable argument with the name `inputs`, and
/// `CODE` for `code`.
std::string ArgumentName(const CLI::Option* option) {
  std::string name = option->get_name(/*positional=*/true);
  std::ranges::transform(name, name.begin(), [](unsigned char character) {
    return static_cast<char>(std::toupper(character));
  });
  if (option->get_expected_max() <= 1) return name;
  if (name.ends_with('S')) name.pop_back();
  return name + "...";
}

bool IsFlag(const CLI::Option* option) {
  return option->get_items_expected_max() == 0;
}

class HelpFormatter : public CLI::FormatterBase {
 public:
  std::string make_help(
      const CLI::App* app,
      std::string /*name*/,
      CLI::AppFormatMode /*mode*/
  ) const override {
    const auto style = Style::For(terminal::OutputStream::kStdout);
    std::ostringstream page;
    page << Title(app, style);
    page << Usage(app, style);
    page << Section("ARGUMENTS", Arguments(app, style), style);
    page << Section("OPTIONS", Options(app, style), style);
    page << Section("COMMANDS", Commands(app, style), style);
    page << Examples(app, style);
    page << std::format(
        "\nLearn more: {}\n",
        style.Paint(Tone::kBrand, kRepository)
    );
    return page.str();
  }

 private:
  static std::string Title(const CLI::App* app, const Style& style) {
    return std::format(
        "\n{} {}\n{}\n",
        style.Paint(Tone::kBrand, CommandPath(app)),
        style.Paint(Tone::kMuted, kVersion),
        app->get_description()
    );
  }

  static std::string Usage(const CLI::App* app, const Style& style) {
    std::string usage = CommandPath(app);
    bool optional = false;
    for (const auto* option : app->get_options()) {
      if (option->get_positional()) continue;
      if (!option->get_required()) {
        optional = true;
        continue;
      }
      usage +=
          std::format(" {} {}", OptionNames(option), option->get_type_name());
    }
    if (optional) usage += " [OPTIONS]";
    for (const auto* option : app->get_options()) {
      if (!option->get_positional()) continue;
      const auto name = ArgumentName(option);
      usage += option->get_required() ? " " + name : " [" + name + "]";
    }
    if (!app->get_subcommands({}).empty()) usage += " COMMAND";
    return std::format(
        "\n{} {}\n",
        style.Paint(Tone::kHeading, "Usage:"),
        usage
    );
  }

  static std::vector<Entry> Arguments(const CLI::App* app, const Style& style) {
    std::vector<Entry> entries;
    for (const auto* option : app->get_options()) {
      if (!option->get_positional()) continue;
      entries.push_back({
          .name = style.Span(Tone::kAccent, ArgumentName(option)),
          .description = option->get_description(),
          .note = option->get_required() ? "required" : "",
      });
    }
    return entries;
  }

  static std::vector<Entry> Options(const CLI::App* app, const Style& style) {
    std::vector<const CLI::Option*> options;
    for (const auto* option : app->get_options()) {
      if (!option->get_positional() && option != app->get_help_ptr()) {
        options.push_back(option);
      }
    }
    if (app->get_help_ptr() != nullptr) options.push_back(app->get_help_ptr());
    std::vector<Entry> entries;
    for (const auto* option : options) {
      Styled name = style.Span(Tone::kAccent, OptionNames(option));
      if (!IsFlag(option)) {
        name += style.Span(Tone::kPlain, " " + option->get_type_name());
      }
      std::string note;
      if (option->get_required()) {
        note = "required";
      } else if (!option->get_default_str().empty()) {
        note = "default: " + option->get_default_str();
      }
      entries.push_back({
          .name = std::move(name),
          .description = option->get_description(),
          .note = std::move(note),
      });
    }
    return entries;
  }

  static std::vector<Entry> Commands(const CLI::App* app, const Style& style) {
    std::vector<Entry> entries;
    for (const auto* command : app->get_subcommands({})) {
      entries.push_back({
          .name = style.Span(Tone::kAccent, command->get_name()),
          .description = command->get_description(),
          .note = "",
      });
    }
    return entries;
  }

  static std::string Heading(std::string_view title, const Style& style) {
    return std::format(
        "\n{}\n{}\n",
        style.Paint(Tone::kHeading, title),
        style.Paint(Tone::kHeading, std::string(title.size(), '-'))
    );
  }

  static std::string Section(
      std::string_view title,
      const std::vector<Entry>& entries,
      const Style& style
  ) {
    if (entries.empty()) return {};
    std::size_t width = 0;
    for (const auto& entry : entries) width = std::max(width, entry.name.width);
    std::string text = Heading(title, style);
    for (const auto& entry : entries) {
      text += std::format(
          "  {}    {}",
          terminal::PadRight(entry.name, width),
          entry.description
      );
      if (!entry.note.empty()) {
        text +=
            "  " + style.Paint(Tone::kMuted, std::format("[{}]", entry.note));
      }
      text += '\n';
    }
    return text;
  }

  static std::string Examples(const CLI::App* app, const Style& style) {
    const auto& footer = app->get_footer();
    if (footer.empty()) return {};
    std::string text = Heading("EXAMPLES", style);
    for (const auto line : std::views::split(footer, '\n')) {
      text += std::format(
          "  {} {}\n",
          style.Paint(Tone::kMuted, "$"),
          std::string_view(line.begin(), line.end())
      );
    }
    return text;
  }
};

}  // namespace

std::shared_ptr<CLI::FormatterBase> MakeHelpFormatter() {
  return std::make_shared<HelpFormatter>();
}

}  // namespace cli
