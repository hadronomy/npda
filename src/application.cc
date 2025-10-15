#include "application.h"

#include "cli.h"
#include "cli/npda.h"
#include "cli/turing.h"
#include "ui.h"

int Application::run(int argc, char** argv) const {
  CommandRegistry registry(
    "cc", "a simple cli for running a NPDA or turing machine and see all the traces"
  );
  try {
    registry.register_command("npda", "execute a given NPDA with a given string", make_npda);
    registry.register_command(
      "turing", "execute a given Turing Machine with a given string", make_turing
    );
    registry.run(argc, argv);
  } catch (const std::exception& e) {
    ui::error(e.what());
    return 1;
  }
  return 0;
}
