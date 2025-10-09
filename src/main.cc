#include <exception>

#include "application.h"
#include "ui.h"

int main(int argc, char** argv) {
  try {
    const auto app = Application();
    app.run(argc, argv);
  } catch (std::exception& e) {
    ui::error(e.what());
  }
}