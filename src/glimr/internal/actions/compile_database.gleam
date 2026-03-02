//// Database Compiler Action
////
//// Regenerates migrations and model code for a given database
//// connection. Called by the watch loop when schema or query
//// files change, avoiding the need to manually re-run db_gen
//// during development.

import gleam/option.{None}
import glimr/db/gen as db_gen
import glimr/db/gen/migrate as gen_migrate

// ------------------------------------------------------------- Public Functions

/// Migrations first, then model code — that order matters
/// because the model generator reads the current schema, and
/// migrations may have just added columns that the generated
/// types need to include. Both run in quiet mode (verbose:
/// False) since the watch loop handles its own output.
///
pub fn run(database_name: String) -> Result(Nil, String) {
  gen_migrate.run(database_name, None, False)
  db_gen.run(database_name, None, False)

  Ok(Nil)
}
