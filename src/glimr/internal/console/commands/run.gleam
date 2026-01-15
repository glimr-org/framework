import gleam/io
import gleam/list
import glimr/console/command.{type Command, type ParsedArgs}
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
  |> command.handler(fn(args) { run(args) })
}

/// Execute the console command.
///
fn run(_args: ParsedArgs) -> Nil {
  let cfg = config.load()

  case list.is_empty(cfg.hooks.run_pre) {
    True -> Nil
    False -> {
      console.output()
      |> console.unpadded()
      |> console.blank_line(1)
      |> console.line_warning("Running pre-run hooks...")
      |> console.print()
    }
  }

  case run_hooks.run(cfg.hooks.run_pre) {
    Ok(_) -> run_with_watch.run(cfg.hooks)
    Error(msg) -> {
      io.println(console.error(msg))
    }
  }
}
