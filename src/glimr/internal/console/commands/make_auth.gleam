import gleam/option.{Some}
import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Flag, Option}
import glimr/console/console
import glimr/db/db.{type DbPool}
import glimr/db/gen as db_gen
import glimr/db/gen/migrate as gen_migrate
import glimr/internal/actions/run_migrate
import glimr/internal/services/make_auth_service

/// The console command description.
const description = "Set up authentication for a model"

/// Creates the make_auth command.
///
pub fn command() -> Command {
  command.new()
  |> command.description(description)
  |> command.args([
    Argument(name: "name", description: "The authenticatable model name"),
    Flag(
      name: "migrate",
      short: "m",
      description: "Run migrations after generating",
    ),
    Flag(
      name: "scoped",
      short: "s",
      description: "Scope middleware and routes to the model name",
    ),
    Option(
      name: "ctx-db-name",
      description: "Context field name for the database pool",
      default: "db",
    ),
  ])
  |> command.db_handler(run)
}

/// Execute the console command.
///
fn run(args: Args, pool: DbPool) -> Nil {
  let model_name = command.get_arg(args, "name") |> string.lowercase()
  let connection = command.get_option(args, "database")
  let ctx_db_name = command.get_option(args, "ctx-db-name")
  let should_migrate = command.has_flag(args, "migrate")
  let scoped = command.has_flag(args, "scoped")

  // 1. Check for existing unscoped auth (if not scoped)
  case scoped {
    False -> {
      case make_auth_service.check_existing_unscoped_auth(model_name) {
        Error(existing_model) -> {
          console.output()
          |> console.line_warning(
            "Warning: Unscoped auth already exists for \""
            <> existing_model
            <> "\".",
          )
          |> console.line_warning(
            "This will overwrite it with \"" <> model_name <> "\".",
          )
          |> console.blank_line(1)
          |> console.line("To add a second auth model, use --scoped:")
          |> console.line("  ./glimr make_auth " <> model_name <> " --scoped")
          |> console.print()

          Nil
        }
        Ok(_) ->
          run_steps(
            model_name,
            connection,
            ctx_db_name,
            scoped,
            should_migrate,
            pool,
          )
      }
    }
    True ->
      run_steps(
        model_name,
        connection,
        ctx_db_name,
        scoped,
        should_migrate,
        pool,
      )
  }
}

/// The actual scaffolding sequence — separated from `run` so
/// the unscoped-auth safety check can bail out early without
/// nesting the entire 15-step pipeline inside a case branch.
///
fn run_steps(
  model_name: String,
  connection: String,
  ctx_db_name: String,
  scoped: Bool,
  should_migrate: Bool,
  pool: DbPool,
) -> Nil {
  // 2. Generate model files (schema + queries)
  make_auth_service.create_model(model_name, connection)

  // 3. Generate migration from schema
  gen_migrate.run(connection, Some([model_name]), False)

  // 4. Generate repository from schema + queries
  db_gen.run(connection, Some([model_name]), False)

  // 5. Generate load middleware (queries DB for full model)
  make_auth_service.create_load_middleware(model_name, connection, ctx_db_name)

  // 6. Generate auth guard middleware
  make_auth_service.create_auth_middleware(model_name, scoped)

  // 7. Generate guest guard middleware
  make_auth_service.create_guest_middleware(model_name, scoped)

  // 8. Generate login validator
  make_auth_service.create_login_validator(scoped, model_name)

  // 9. Generate register validator
  make_auth_service.create_register_validator(scoped, model_name)

  // 10. Generate login controller
  make_auth_service.create_login_controller(
    model_name,
    connection,
    ctx_db_name,
    scoped,
  )

  // 11. Generate logout controller
  make_auth_service.create_logout_controller(model_name, scoped)

  // 12. Generate register controller
  make_auth_service.create_register_controller(
    model_name,
    connection,
    ctx_db_name,
    scoped,
  )

  // 13. Generate dashboard controller
  make_auth_service.create_dashboard_controller(model_name, scoped)

  // 14. Generate loom views (login, register, dashboard, components)
  make_auth_service.create_loom_views(scoped, model_name, connection)

  // 15. Patch kernel with load middleware
  make_auth_service.register_in_kernel(model_name)

  // 16. Patch App type with typed model field
  make_auth_service.register_in_app(model_name, connection)

  // 17. Patch App constructor with option.None initializer
  make_auth_service.register_in_app_start(model_name)

  // 18. Optionally run migrations
  case should_migrate {
    True -> run_migrate.run(pool, connection)
    False -> Nil
  }
}

/// Console command's entry point
///
pub fn main() {
  command.run(command())
}
