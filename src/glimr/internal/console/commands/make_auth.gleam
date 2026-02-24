import gleam/option.{Some}
import gleam/string
import glimr/console/command.{type Args, type Command, Argument, Flag}
import glimr/db/gen as db_gen
import glimr/db/gen/migrate as gen_migrate
import glimr/db/pool_connection.{type Pool}
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
  ])
  |> command.db_handler(run)
}

/// Execute the console command.
///
fn run(args: Args, pool: Pool) -> Nil {
  let model_name = command.get_arg(args, "name") |> string.lowercase()
  let connection = command.get_option(args, "database")
  let should_migrate = command.has_flag(args, "migrate")

  // 1. Generate model files (schema + queries)
  make_auth_service.create_model(model_name, connection)

  // 2. Generate migration from schema
  gen_migrate.run(connection, Some([model_name]))

  // 3. Generate repository from schema + queries
  db_gen.run(connection, Some([model_name]))

  // 4. Generate load middleware (queries DB for full model)
  make_auth_service.create_load_middleware(model_name, connection)

  // 5. Generate auth guard middleware
  make_auth_service.create_auth_middleware(model_name)

  // 6. Generate guest guard middleware
  make_auth_service.create_guest_middleware(model_name)

  // 7. Patch kernel with load middleware
  make_auth_service.register_in_kernel(model_name)

  // 8. Patch context with typed model field
  make_auth_service.register_in_context(model_name, connection)

  // 9. Patch ctx_provider with option.None initializer
  make_auth_service.register_in_ctx_provider(model_name)

  // 10. Optionally run migrations
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
