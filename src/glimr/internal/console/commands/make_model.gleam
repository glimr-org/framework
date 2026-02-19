import gleam/list
import gleam/string
import glimr/config/database
import glimr/console/command.{type Args, type Command, Argument}
import glimr/console/console
import glimr/db/driver
import glimr/filesystem/filesystem
import glimr/utils/string as glimr_string

/// The console command description.
const description = "Create a new model"

/// Define the Command and its properties.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The name of the model"),
    command.db_option(),
  ])
  |> command.handler(run)
}

/// Execute the console command.
///
fn run(args: Args) -> Nil {
  let model_name_input = command.get_arg(args, "name")
  let connection = command.get_option(args, "database")

  let connection_name = case connection {
    "_default" -> {
      case list.first(database.load()) {
        Ok(conn) -> driver.connection_name(conn)
        Error(_) -> "_default"
      }
    }
    _ -> connection
  }

  let model_name = string.lowercase(model_name_input)
  let table_name = glimr_string.pluralize(model_name)
  let model_dir = "src/data/" <> connection_name <> "/models/" <> model_name
  let queries_dir = model_dir <> "/queries"

  let assert Ok(dir_exists) = filesystem.directory_exists(model_dir)

  case dir_exists {
    True -> {
      console.output()
      |> console.line_error("Error: Model already exists!")
      |> console.line("[" <> model_dir <> "]")
      |> console.print()
    }
    False -> {
      let schema_path = model_dir <> "/" <> model_name <> "_schema.gleam"
      let assert Ok(_) =
        filesystem.write_from_stub_with_variables(
          stub_path: "data/schema.stub",
          dest_path: schema_path,
          variables: [#("table_name", table_name)],
        )

      let query_stubs = ["create", "delete", "find", "list_all", "update"]
      list.each(query_stubs, fn(query_name) {
        let query_path = queries_dir <> "/" <> query_name <> ".sql"
        let assert Ok(_) =
          filesystem.write_from_stub_with_variables(
            stub_path: "data/queries/" <> query_name <> ".stub",
            dest_path: query_path,
            variables: [#("table_name", table_name)],
          )
      })

      console.output()
      |> console.line_success("Model created successfully!")
      |> console.line("[" <> model_dir <> "]")
      |> console.print()
    }
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
