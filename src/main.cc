// Run the application. Import only.
import std;
import app;
import ui;

int main(int argc, char** argv) {
  try {
    const auto app = Application();
    app.run(argc, argv);
  } catch (std::exception& e) {
    ui::error(e.what());
  }
}