import gleam/io
import glimr/config
import glimr/console/command.{type Args, type Command}
import glimr/console/console
import glimr/internal/actions/auto_compile
import glimr/internal/actions/run_hooks
import glimr/internal/actions/run_with_watch

/// The console command description.
const description = "Run the application with hot reload"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(_args: Args) -> Nil {
  case auto_compile.run() {
    Ok(_) -> {
      case run_hooks.run(config.get_string_list("glimr.hooks.run.pre")) {
        Ok(_) -> run_with_watch.run()
        Error(msg) -> {
          io.println(console.error(msg))
        }
      }
    }
    Error(msg) -> {
      io.println(console.error(msg))
    }
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
