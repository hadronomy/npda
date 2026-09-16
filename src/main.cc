// Run the application. Import only.
import std;
import app;
import ui;

int main(int argc, char** argv) {
  try {
    const auto app = Application();
    return app.run(argc, argv);
  } catch (const std::exception& e) {
    ui::error(e.what());
    return 1;
  }
}