import gleeunit/should
import glimr/internal/actions/compile_routes
import simplifile

/// When a controller is deleted, the compiled route file that
/// referenced it must be cleaned up. Otherwise `gleam build`
/// fails with "Unknown module" because the stale file still
/// imports the deleted controller.
///
pub fn stale_route_files_are_cleaned_up_test() {
  let routes_dir = "src/compiled/routes"

  // Ensure directory exists
  let _ = simplifile.create_directory_all(routes_dir)

  // Plant a stale compiled route file
  let stale_path = routes_dir <> "/_test_stale.gleam"
  let assert Ok(_) = simplifile.write(stale_path, "// stale route file")

  // Verify it exists
  simplifile.is_file(stale_path)
  |> should.equal(Ok(True))

  // Run route compilation (no controllers reference this group)
  let _ = compile_routes.run(False)

  // The stale file should have been removed
  simplifile.is_file(stale_path)
  |> should.equal(Ok(False))
}
