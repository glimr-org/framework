import gleam/io
import glimr/console/command.{type Args, type Command}
import glimr/console/console
import glimr/internal/actions/auto_compile
import glimr/internal/actions/run_hooks
import glimr/internal/actions/run_with_watch
import glimr/internal/config

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
  let cfg = config.load()

  case auto_compile.run(cfg) {
    Ok(_) -> {
      case run_hooks.run(cfg.hooks.run_pre) {
        Ok(_) -> run_with_watch.run(cfg)
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
