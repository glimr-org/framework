import gleam/io
import glimr/console/command.{type Command, type ParsedArgs, Flag}
import glimr/console/console
import glimr/internal/actions/compile_routes
import glimr/routing/router.{type RouteGroupConfig}

/// The name of the console command.
const name = "route:compile"

/// The console command description.
const description = "Compile controller routes to optimized pattern matching"

/// Define the console command and its properties.
///
pub fn command(route_groups: List(RouteGroupConfig)) -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.args([
    Flag("verbose", "v", "Display information about compiled routes"),
  ])
  |> command.handler(fn(args) { run(args, route_groups) })
}

/// Execute the console command.
///
fn run(args: ParsedArgs, route_groups: List(RouteGroupConfig)) -> Nil {
  let verbose = command.has_flag(args, "verbose")

  case compile_routes.run(verbose, route_groups) {
    Ok(_) -> Nil
    Error(msg) -> io.println(console.error(msg))
  }
}
