import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Flag}
import glimr/console/console
import glimr/filesystem/filesystem
import glimr/utils/string as glimr_string

/// The console command description.
const description = "Create a new controller"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the controller"),
    Flag(
      name: "resource",
      short: "r",
      description: "Generate a resource controller",
    ),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let name = command.get_arg(args, "name")
  let resource = command.has_flag(args, "resource")

  let module_name = string.lowercase(name)
  let file_path = "src/app/http/controllers/" <> module_name <> ".gleam"

  let stub_name = case resource {
    True -> "controller_resource.stub"
    False -> "controller.stub"
  }

  let model_name = case string.ends_with(module_name, "_controller") {
    True -> string.drop_end(module_name, string.length("_controller"))
    False -> module_name
  }

  let plural = glimr_string.pluralize(model_name)
  let param = ":" <> model_name

  let variables = [
    #("model", model_name),
    #("route", plural),
    #("index_route", plural),
    #("show_route", plural <> "/" <> param),
    #("store_route", plural),
    #("edit_route", plural <> "/" <> param <> "/edit"),
    #("update_route", plural <> "/" <> param),
    #("destroy_route", plural <> "/" <> param),
  ]

  let assert Ok(file_exists) = filesystem.file_exists(file_path)

  case file_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Controller already exists!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
    False -> {
      let assert Ok(_) =
        filesystem.write_from_stub_with_variables(
          package: "glimr",
          stub_path: "http/" <> stub_name,
          dest_path: file_path,
          variables: variables,
        )

      console.output()
      |> console.line_success("Controller created successfully!")
      |> console.line("[" <> file_path <> "]")
      |> console.print()
    }
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
