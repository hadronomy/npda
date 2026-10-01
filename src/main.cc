#include <exception>

#include "application.h"
#include "terminal/ui.h"

int main(int argc, char** argv) {
  try {
    return cli::RunApplication(argc, argv);
  } catch (const std::exception& e) {
    ui::Error(e.what());
    return 1;
  }
}
