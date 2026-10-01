#include "terminal/activity.h"

#include <cctype>
#include <chrono>
#include <cstdlib>
#include <mutex>
#include <ostream>
#include <string>
#include <string_view>
#include <utility>

#include "terminal/capabilities.h"

namespace terminal {

namespace {

bool ColorEnabled() {
  return terminal::CapabilitiesFor(terminal::OutputStream::kStderr).color;
}

// Resolve Auto against the terminal. Explicit modes pass through.
const terminal::Capabilities& ProbeTerminal() {
  return terminal::CapabilitiesFor(terminal::OutputStream::kStderr);
}

namespace ansi {

[[nodiscard]] constexpr std::string_view Red(bool c) {
  return c ? "\x1b[31m" : "";
}

[[nodiscard]] constexpr std::string_view Green(bool c) {
  return c ? "\x1b[32m" : "";
}

[[nodiscard]] constexpr std::string_view Dim(bool c) {
  return c ? "\x1b[2m" : "";
}

[[nodiscard]] constexpr std::string_view Reset(bool c) {
  return c ? "\x1b[0m" : "";
}

}  // namespace ansi

}  // namespace

Activity::Activity(std::string message, std::ostream& os)
    : os_(os), message_(std::move(message)), color_(ColorEnabled()) {
  const terminal::Capabilities& t = ProbeTerminal();
  if (!t.tty || !t.utf8) return;
  enabled_ = true;
  worker_ = std::jthread([this] { Run(); });
}

Activity::~Activity() { Dismiss(); }

void Activity::SetMessage(std::string message) {
  std::lock_guard lock(mutex_);
  message_ = std::move(message);
}

void Activity::Dismiss() { Suspend(); }

void Activity::Suspend() {
  stop_requested_.store(true);
  cv_.notify_all();
  if (worker_.joinable()) {
    worker_.request_stop();
    worker_.join();
  }
  Clear();
}

void Activity::Resume() {
  if (!enabled_ || worker_.joinable()) return;
  stop_requested_.store(false);
  worker_ = std::jthread([this] { Run(); });
}

void Activity::Finish(const std::string& done) {
  Dismiss();
  // Silent unless a frame painted: fast and piped runs stay clean.
  if (finished_ || !painted_) return;
  finished_ = true;
  os_ << ansi::Green(color_) << "✓" << ansi::Reset(color_) << " " << done
      << "\n"
      << std::flush;
}

void Activity::Fail(const std::string& msg) {
  Dismiss();
  if (finished_ || !painted_) return;
  finished_ = true;
  os_ << ansi::Red(color_) << "✗" << ansi::Reset(color_) << " " << msg << "\n"
      << std::flush;
}

void Activity::Clear() {
  std::lock_guard lock(mutex_);
  if (!active_) return;
  active_ = false;
  os_ << "\r\x1b[2K\x1b[?25h" << std::flush;
}

void Activity::Run() {
  // Grace period: fast work prints nothing at all.
  {
    std::unique_lock lock(mutex_);
    if (cv_.wait_for(lock, std::chrono::milliseconds(150),
                     [&] { return stop_requested_.load(); }))
      return;
  }
  {
    std::lock_guard lock(mutex_);
    active_ = true;
    painted_ = true;
    os_ << "\x1b[?25l" << std::flush;
  }
  static constexpr const char* kFrames[] = {"⠋", "⠙", "⠹", "⠸", "⠼",
                                            "⠴", "⠦", "⠧", "⠇", "⠏"};
  std::size_t frame = 0;
  while (true) {
    std::string msg;
    {
      std::lock_guard lock(mutex_);
      msg = message_;
    }
    os_ << "\x1b[?2026h\r\x1b[2K" << ansi::Dim(color_) << kFrames[frame % 10]
        << ansi::Reset(color_) << " " << msg << "\x1b[?2026l" << std::flush;
    frame++;
    std::unique_lock lock(mutex_);
    if (cv_.wait_for(lock, std::chrono::milliseconds(80),
                     [&] { return stop_requested_.load(); }))
      return;
  }
}

}  // namespace terminal
