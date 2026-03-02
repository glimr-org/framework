//// Build Command
////
//// The `./glimr build` command. Runs auto-compilation for
//// routes, templates, and queries, then fires pre-build hooks,
//// invokes `gleam build`, and finally runs post-build hooks.
//// Any failure halts the process with exit code 1.

import gleam/io
import gleam/result
import glimr/console/command.{type Args, type Command}
import glimr/console/console
import glimr/internal/actions/auto_compile
import glimr/internal/actions/run_build
import glimr/internal/actions/run_hooks
import glimr/internal/config

const description = "Build the application"

/// Registers this as a CLI command so `./glimr build` finds it
/// in the command registry. The handler delegates to run()
/// which orchestrates the full build pipeline.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.handler(run)
}

/// Runs the full build pipeline in order: auto-compile (routes,
/// templates, queries), pre-build hooks, gleam build, then
/// post-build hooks. Halts with exit code 1 on any failure so
/// CI pipelines get a clear signal.
///
fn run(_args: Args) -> Nil {
  let cfg = config.load()

  let result = {
    use _ <- result.try(auto_compile.run(cfg))
    use _ <- result.try(run_hooks.run(cfg.hooks.build_pre))
    use _ <- result.try(run_build.run())
    run_hooks.run(cfg.hooks.build_post)
  }

  case result {
    Ok(_) -> Nil
    Error(msg) -> {
      io.println(console.error(msg))
      console.halt(1)
    }
  }
}

/// Gleam requires a main() when this module is compiled as an
/// escript target. It just wires up the command and hands off
/// to the argument parser.
///
pub fn main() {
  command.run(command())
}
