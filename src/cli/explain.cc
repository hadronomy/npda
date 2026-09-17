// Implement ExplainHandler in the cli module.
module cli;

// Import the modules below.
import std;
import diag;
import ui;

int ExplainHandler::operator()(const CommandContext&) {
  const auto entries = diag::explain(code);
  if (entries.empty()) {
    ui::error("unknown error code '" + code + "'");
    return 1;
  }
  for (const auto& e : entries) {
    std::cout << e.code << " [" << e.machine << "]: " << e.title << '\n' << e.hint << '\n';
  }
  return 0;
}
