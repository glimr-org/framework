import gleam/io
import gleam/list
import glimr/console/command.{type Command, type ParsedArgs}
import glimr/console/console
import glimr/internal/actions/run_hooks
import glimr/internal/actions/run_with_watch
import glimr/internal/config
import glimr/routing/router.{type RouteGroupConfig}

/// The name of the console command.
const name = "run"

/// The console command description.
const description = "Run the application with hot reload"

/// Define the console command and its properties.
///
pub fn command(route_groups: List(RouteGroupConfig)) -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.handler(fn(args) { run(args, route_groups) })
}

/// Execute the console command.
///
fn run(_args: ParsedArgs, route_groups: List(RouteGroupConfig)) -> Nil {
  let cfg = config.load()

  case list.is_empty(cfg.hooks.run_pre) {
    True -> Nil
    False -> {
      io.println("")
    }
  }

  case run_hooks.run(cfg.hooks.run_pre) {
    Ok(_) -> run_with_watch.run(cfg.hooks, route_groups)
    Error(msg) -> {
      io.println(console.error(msg))
    }
  }
}
