#ifndef NPDA_SRC_TERMINAL_ACTIVITY_H_
#define NPDA_SRC_TERMINAL_ACTIVITY_H_

#include <atomic>
#include <condition_variable>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>

namespace terminal {

class Activity {
 public:
  explicit Activity(std::string message, std::ostream& os = std::cerr);

  ~Activity();
  Activity(const Activity&) = delete;
  Activity& operator=(const Activity&) = delete;
  Activity(Activity&&) = delete;
  Activity& operator=(Activity&&) = delete;

  void SetMessage(std::string message);

  void Dismiss();

  // Stop frames and clear the line. The worker restarts on resume().
  // Use around stdout blocks so frames never interleave with output.
  void Suspend();

  void Resume();

  void Finish(const std::string& done);

  // Error twin of finish(). Prints a red cross instead of the check.
  void Fail(const std::string& msg);

 private:
  void Clear();

  void Run();

  std::ostream& os_;
  std::string message_;
  const bool color_ = false;
  std::mutex mutex_;
  std::condition_variable_any cv_;
  std::atomic<bool> stop_requested_{false};
  bool enabled_ = false;
  bool active_ = false;
  bool painted_ = false;
  bool finished_ = false;
  std::jthread worker_;
};

}  // namespace terminal

#endif  // NPDA_SRC_TERMINAL_ACTIVITY_H_
