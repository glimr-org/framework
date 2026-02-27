import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glimr/console/command.{type Args, type Command, Flag, Option as CmdOption}
import glimr/console/console
import glimr/db/gen as db_gen
import glimr/db/gen/migrate as gen_migrate
import glimr/db/pool_connection.{type DbPool}
import glimr/internal/actions/run_migrate
import simplifile

/// The console command description.
const description = "Generate repository and migration code"

/// Creates the db_gen command.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    CmdOption(
      name: "model",
      description: "Comma-separated list of models to generate",
      default: "",
    ),
    Flag(
      name: "migrate",
      short: "m",
      description: "Run migrations after generating",
    ),
  ])
  |> command.db_handler(run)
}

/// Executes the gen command.
///
fn run(args: Args, pool: DbPool) -> Nil {
  let database = command.get_option(args, "database")
  let model_option = command.get_option(args, "model")
  let should_migrate = command.has_flag(args, "migrate")

  let model_filter = parse_model_filter(model_option)

  let models_path = "src/data/" <> database <> "/models"
  case validate_models(models_path, model_filter) {
    Error(invalid) -> {
      console.output()
      |> console.line_error(
        "Model(s) not found in "
        <> models_path
        <> ": "
        <> string.join(invalid, ", "),
      )
      |> console.print()
    }
    Ok(validated_filter) -> {
      gen_migrate.run(database, validated_filter)

      console.output()
      |> console.blank_line(1)
      |> console.print()

      db_gen.run(database, validated_filter)

      case should_migrate {
        True -> {
          console.output()
          |> console.blank_line(1)
          |> console.print()

          run_migrate.run(pool, database)
        }
        False -> Nil
      }
    }
  }
}

fn parse_model_filter(model_option: String) -> Option(List(String)) {
  case string.trim(model_option) {
    "" -> None
    value -> {
      let models =
        value
        |> string.split(",")
        |> list.map(string.trim)
        |> list.filter(fn(s) { s != "" })

      case models {
        [] -> None
        _ -> Some(models)
      }
    }
  }
}

fn validate_models(
  models_path: String,
  models: Option(List(String)),
) -> Result(Option(List(String)), List(String)) {
  case models {
    None -> Ok(None)
    Some(model_list) -> {
      let #(valid, invalid) =
        list.partition(model_list, fn(model) {
          case simplifile.is_directory(models_path <> "/" <> model) {
            Ok(True) -> True
            _ -> False
          }
        })
      case invalid {
        [] -> Ok(Some(valid))
        _ -> Error(invalid)
      }
    }
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
