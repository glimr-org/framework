import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import glimr/console/command.{type Args, type Command}
import glimr/console/console
import glimr/internal/actions/compile_commands.{type CommandInfo}

/// The console command description.
const description = "List all available commands"

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
  case compile_commands.read_registry() {
    Ok(registry) -> print_commands(registry)
    Error(_) -> {
      io.println(console.error("Command registry not found."))
      io.println("Run ./glimr command_compile first.")
    }
  }
}

/// Prints the command list from the registry.
///
fn print_commands(registry: dict.Dict(String, CommandInfo)) -> Nil {
  command.print_glimr_version()
  io.println("Build scalable web apps AI can understand, and you can trust.")
  io.println("")

  io.println(console.warning("Usage:"))
  io.println("  ./glimr <command> [arguments] [options]")
  io.println("")

  let commands =
    registry
    |> dict.to_list
    |> list.map(fn(entry) {
      let #(cmd_name, info) = entry
      #(cmd_name, info.description)
    })

  // Find the longest command name for alignment
  let max_length =
    list.fold(commands, string.length("-h, --help"), fn(acc, cmd) {
      int.max(acc, string.length(cmd.0))
    })

  io.println(console.warning("Options:"))
  let help_label = string.pad_end("-h, --help", max_length + 2, " ")
  io.println(
    "  " <> console.success(help_label) <> "Display help for the given command",
  )
  io.println("")

  // Split commands into ungrouped and grouped (has underscore)
  let #(ungrouped, grouped) =
    list.partition(commands, fn(cmd) { !string.contains(cmd.0, "_") })

  // Sort ungrouped commands alphabetically
  let sorted_ungrouped =
    list.sort(ungrouped, fn(a, b) { string.compare(a.0, b.0) })

  // Group commands by prefix (first part before underscore)
  let groups = group_by_prefix(grouped)

  // Sort groups alphabetically
  let sorted_groups = list.sort(groups, fn(a, b) { string.compare(a.0, b.0) })

  // Print ungrouped commands
  io.println(console.warning("Available commands:"))
  list.each(sorted_ungrouped, fn(cmd) {
    let padded_name = string.pad_end(cmd.0, max_length + 2, " ")
    io.println("  " <> console.success(padded_name) <> cmd.1)
  })

  // Print grouped commands
  list.each(sorted_groups, fn(group) {
    let #(prefix, cmds) = group
    io.println(" " <> console.warning(prefix))
    list.each(cmds, fn(cmd) {
      let padded_name = string.pad_end(cmd.0, max_length + 2, " ")
      io.println("  " <> console.success(padded_name) <> cmd.1)
    })
  })
}

/// Groups commands by their prefix (first part before
/// underscore).
///
fn group_by_prefix(
  commands: List(#(String, String)),
) -> List(#(String, List(#(String, String)))) {
  let sorted = list.sort(commands, fn(a, b) { string.compare(a.0, b.0) })

  list.fold(sorted, [], fn(acc, cmd) {
    let prefix = case string.split(cmd.0, "_") {
      [p, ..] -> p
      _ -> ""
    }
    case list.key_find(acc, prefix) {
      Ok(existing) -> list.key_set(acc, prefix, list.append(existing, [cmd]))
      Error(_) -> list.append(acc, [#(prefix, [cmd])])
    }
  })
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
