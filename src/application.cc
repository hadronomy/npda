#include "application.h"

#include "cli.h"
#include "cli/run.h"
#include "ui.h"

int Application::run(int argc, char** argv) const {
  CommandRegistry registry("npda", "a simple cli for running a NPDA and see all the traces");
  try {
    registry.register_command("run", "execute a given NPDA with a given string", make_run);
    registry.run(argc, argv);
  } catch (const std::exception& e) {
    ui::error(e.what());
    return 1;
  }
  return 0;
}
