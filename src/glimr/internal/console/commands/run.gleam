import gleam/io
import glimr/console/command.{type Command, type Args}
import glimr/console/console
import glimr/internal/actions/run_hooks
import glimr/internal/actions/run_with_watch
import glimr/internal/config

/// The name of the console command.
const name = "run"

/// The console command description.
const description = "Run the application with hot reload"

/// Define the console command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(_args: Args) -> Nil {
  let cfg = config.load()

  case run_hooks.run(cfg.hooks.run_pre) {
    Ok(_) -> run_with_watch.run(cfg.hooks)
    Error(msg) -> {
      io.println(console.error(msg))
    }
  }
}
