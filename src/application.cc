// Implement Application in the app module.
module app;

// Import the modules below.
import std;
import cli;
import ui;

int Application::run(int argc, char** argv) const {
  CommandRegistry registry(
    "cc", "a simple cli for running a NPDA or turing machine and see all the traces"
  );
  try {
    registry.register_command("npda", "execute a given NPDA with a given string", make_npda);
    registry.register_command(
      "turing", "execute a given Turing Machine with a given string", make_turing
    );
    registry.register_command("prf", "execute primitive recursive functions", make_prf);
    registry.run(argc, argv);
  } catch (const std::exception& e) {
    ui::error(e.what());
    return 1;
  }
  return 0;
}
