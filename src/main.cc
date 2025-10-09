#include "cli.h"
#include "cli/run.h"

int main(int argc, char** argv) {
  CommandRegistry registry("npda", "a simple cli for running a NPDA and see all the traces");
  registry.register_command("run", "execute a given NPDA with a given string", make_run);
  registry.run(argc, argv);
}