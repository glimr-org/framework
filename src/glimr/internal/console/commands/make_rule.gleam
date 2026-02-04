import gleam/string
import glimr/console/command.{type Command, type Args, Argument, Flag}
import glimr/console/console
import glimr/filesystem/filesystem

/// The name of the console command.
const name = "make:rule"

/// The console command description.
const description = "Create a new validation rule"

/// Define the Command and it's properties.
///
pub fn command() -> Command {
  command.new()
  |> command.name(name)
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the validation rule"),
    Flag(
      name: "file",
      short: "f",
      description: "Generate a validation rule for an uploaded file",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let file = command.has_flag(args, "file")

  let module_name = string.lowercase(name)
  let file_path = "src/app/http/rules/" <> module_name <> ".gleam"

  let stub_name = case file {
    True -> "rule_file.stub"
    False -> "rule.stub"
  }

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Rule already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) = {
        filesystem.write_from_stub("http/" <> stub_name, file_path)
      }

      console.output()
      |> console.line_success("Rule created successfully!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
  }
}
