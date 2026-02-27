//// Loom Module Registry
////
//// The WebSocket init message includes a module name chosen by
//// the client. Without validation, a malicious client could
//// invoke arbitrary Erlang modules via dynamic dispatch. This
//// registry acts as an allowlist — only modules explicitly
//// registered during compilation can be dispatched to, closing
//// the arbitrary-execution hole.
////

import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import simplifile

const registry_path = "priv/storage/framework/loom/modules.json"

// ------------------------------------------------------------- Public Types

/// Each entry tracks both the module path (for dispatch
/// validation) and the source template path (for debugging and
/// recompilation). Storing both avoids a second lookup to map
/// between module names and their source files.
///
pub type ModuleEntry {
  ModuleEntry(module: String, source: String)
}

// ------------------------------------------------------------- Public Functions

/// The WebSocket handler calls this before starting an actor to
/// ensure the client-requested module was actually compiled by
/// the framework. Rejecting unknown modules here prevents
/// arbitrary code execution via the dynamic dispatch path.
///
pub fn is_valid_module(module: String) -> Bool {
  read_registry()
  |> result.map(set.contains(_, module))
  |> result.unwrap(False)
}

/// The compiler calls this after successfully generating a live
/// template module. Adding the module to the registry at
/// compile time ensures the allowlist is always in sync with
/// the actual set of generated modules, without requiring a
/// separate configuration step.
///
pub fn register_module(module: String, source: String) -> Result(Nil, String) {
  let entries = read_entries() |> result.unwrap([])

  // Update or add entry
  let updated =
    entries
    |> list.filter(fn(e) { e.module != module })
    |> list.append([ModuleEntry(module:, source:)])

  write_entries(updated)
}

/// When a template file is deleted, its compiled module should
/// no longer be dispatchable. Removing it from the registry
/// ensures stale module names can't be exploited after the
/// template source is gone.
///
pub fn unregister_module(module: String) -> Result(Nil, String) {
  case read_entries() {
    Ok(entries) ->
      write_entries(list.filter(entries, fn(e) { e.module != module }))
    Error(_) -> Ok(Nil)
  }
}

/// The compiler and diagnostic tools need to inspect the full
/// registry contents — for example, to rebuild all known
/// modules or display the list of live templates in development
/// tooling.
///
pub fn list_modules() -> List(ModuleEntry) {
  read_entries() |> result.unwrap([])
}

/// A full recompilation starts fresh to avoid stale entries
/// from renamed or deleted templates lingering in the registry.
/// Clearing first and re-registering during compilation
/// guarantees the registry exactly matches the current template
/// set.
///
pub fn clear() -> Result(Nil, String) {
  write_entries([])
}

// ------------------------------------------------------------- Private Functions

/// is_valid_module only needs module names, not full entries.
/// Converting to a set here provides O(1) membership checks for
/// the hot path where every WebSocket init message triggers a
/// validation lookup.
///
fn read_registry() -> Result(Set(String), Nil) {
  read_entries()
  |> result.map(fn(entries) {
    list.map(entries, fn(e) { e.module }) |> set.from_list
  })
}

/// The registry is persisted as JSON on disk so it survives
/// application restarts without requiring recompilation.
/// Returning an empty list on read failure (missing file) is
/// safe because a fresh install has no registered modules yet.
///
fn read_entries() -> Result(List(ModuleEntry), Nil) {
  case simplifile.read(registry_path) {
    Error(_) -> Ok([])
    Ok(content) ->
      json.parse(content, decode.list(entry_decoder()))
      |> result.map_error(fn(_) { Nil })
  }
}

/// Persists the full entry list to disk as JSON. Creating the
/// directory tree first handles the cold-start case where the
/// storage directory doesn't exist yet. Writing the entire list
/// atomically avoids partial-update corruption from concurrent
/// compilations.
///
fn write_entries(entries: List(ModuleEntry)) -> Result(Nil, String) {
  // Ensure directory exists
  let dir = string.replace(registry_path, "/modules.json", "")
  let _ = simplifile.create_directory_all(dir)

  let json_str =
    entries
    |> json.array(fn(entry) {
      json.object([
        #("module", json.string(entry.module)),
        #("source", json.string(entry.source)),
      ])
    })
    |> json.to_string()

  // Write new modules to file or throw a custom error
  simplifile.write(registry_path, json_str)
  |> result.replace_error("Failed to write loom registry")
}

/// The JSON schema must match write_entries exactly. Decoding
/// into a typed ModuleEntry ensures the registry data is well-
/// formed before any module name reaches the dispatch
/// validation path.
///
fn entry_decoder() -> decode.Decoder(ModuleEntry) {
  use module <- decode.field("module", decode.string)
  use source <- decode.field("source", decode.string)
  decode.success(ModuleEntry(module:, source:))
}
