#ifndef NPDA_SRC_TERMINAL_STYLE_H_
#define NPDA_SRC_TERMINAL_STYLE_H_

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>

#include "terminal/capabilities.h"

namespace terminal {

/// The meaning of a piece of text.
///
/// Each tone uses one of the terminal's sixteen colors, so the output follows
/// the user's theme on all backgrounds.
enum class Tone {
  kPlain,
  /// The values that the reader looks for first.
  kStrong,
  /// Labels, separators, and less important details.
  kMuted,
  kHeading,
  /// Names that the reader types or follows: commands, options, and
  /// transitions.
  kAccent,
  /// Names of a second kind beside `kAccent`, in a hue of their own, such as
  /// the projections beside the other basic functions.
  kSecondary,
  /// A name that the text defines, wherever it appears, such as `pow` on both
  /// sides of its own equations.
  kDefined,
  /// The name of the program.
  kBrand,
  kGood,
  kBad,
  kWarn,
};

/// Symbols, each with an ASCII form for terminals that cannot show Unicode.
///
/// The ASCII form of ε is `.`, as in machine files.
struct Glyphs {
  std::string_view epsilon;
  std::string_view delta;
  std::string_view arrow;
  /// Points from a move to the configuration that the move gives.
  std::string_view leads_to;
  /// Divides the input that the machine read from the input that remains.
  std::string_view cursor;
  /// Points up to the cell below a head.
  std::string_view head;
  std::string_view separator;
  std::string_view digit_group;
  std::string_view ellipsis;
  std::string_view tree_branch;
  std::string_view tree_last;
  std::string_view tree_pipe;
  std::string_view box_horizontal;
  std::string_view box_vertical;
  std::string_view box_top_left;
  std::string_view box_top_join;
  std::string_view box_top_right;
  std::string_view box_bottom_left;
  std::string_view box_bottom_join;
  std::string_view box_bottom_right;
};

/// Painted text and the number of columns that it uses on the screen.
///
/// Because escape sequences use no columns, `Styled` measures text before
/// applying color and preserves that width through concatenation.
struct Styled {
  std::string text;
  std::size_t width = 0;

  Styled& operator+=(const Styled& other);
};

[[nodiscard]] Styled operator+(Styled left, const Styled& right);

class Style {
 public:
  /// Uses color only if `stream` is a terminal without `NO_COLOR` or
  /// `TERM=dumb`, and Unicode only if its locale is UTF-8.
  [[nodiscard]] static Style For(OutputStream stream);

  /// Returns a style for files: UTF-8 text without escape sequences.
  [[nodiscard]] static Style Plain();

  [[nodiscard]] std::string Paint(Tone tone, std::string_view text) const;

  /// Paints `text` as `Paint` does, and keeps its width for alignment.
  [[nodiscard]] Styled Span(Tone tone, std::string_view text) const;

  /// Returns the muted ` · ` that divides items on one line.
  [[nodiscard]] std::string Separator() const;

  /// Returns `value` with its digits in groups of three, `805 306 852`.
  ///
  /// The groups use a narrow space, as SI and the RAE recommend, because a
  /// comma would blur with the commas between arguments in `pow(10 000, 2)`.
  /// Without Unicode, they use an ordinary space, and numbers of four digits or
  /// fewer stay whole in either form: `1000`.
  [[nodiscard]] std::string Number(std::uint64_t value) const;

  // NOLINTBEGIN(readability-identifier-naming): Google style accessors.
  [[nodiscard]] const Glyphs& glyphs() const { return *glyphs_; }

  /// Whether the destination supports Unicode, consistent with its glyphs.
  [[nodiscard]] bool unicode() const { return unicode_; }

  // NOLINTEND(readability-identifier-naming)

 private:
  Style(bool color, bool unicode);

  bool color_;
  bool unicode_;
  const Glyphs* glyphs_;
};

/// Returns the number of columns of `text`, one for each code point.
///
/// Because the count includes escape sequences, measure text before applying
/// color.
[[nodiscard]] std::size_t DisplayWidth(std::string_view text);

/// Adds spaces after `text` to reach `width` columns, leaving wider text
/// intact.
[[nodiscard]] std::string PadRight(std::string_view text, std::size_t width);

/// Adds spaces before `text` until it has `width` columns. Use it for numbers.
[[nodiscard]] std::string PadLeft(std::string_view text, std::size_t width);

[[nodiscard]] std::string PadRight(const Styled& text, std::size_t width);

/// Cuts `text` to `width` columns, ending it with `ellipsis` only if it was
/// cut.
[[nodiscard]] std::string Truncate(
    std::string_view text,
    std::size_t width,
    std::string_view ellipsis
);

/// Like `Truncate`, cuts `text` to `width` columns, but keeps both ends and
/// puts the ellipsis in the middle.
[[nodiscard]] std::string TruncateMiddle(
    std::string_view text,
    std::size_t width,
    std::string_view ellipsis
);

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_STYLE_H_
