#ifndef NPDA_SRC_TERMINAL_UI_H_
#define NPDA_SRC_TERMINAL_UI_H_

#include <cstddef>
#include <string>
#include <string_view>

namespace ui {

void Error(std::string_view message);
void Info(std::string_view message);
[[nodiscard]] std::string TruncateMiddle(std::string_view text,
                                         std::size_t width = 60);
[[nodiscard]] std::string Rule(std::string_view title);
[[nodiscard]] std::string Arrow();
[[nodiscard]] std::string Rail(std::string_view title);

}  // namespace ui

#endif  // NPDA_SRC_TERMINAL_UI_H_
