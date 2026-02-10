//// Loom Module Registry
////
//// Maintains a registry of valid live template modules for security
//// validation. Prevents clients from requesting arbitrary modules
//// via WebSocket.
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

/// Registry entry for a live module
pub type ModuleEntry {
  ModuleEntry(module: String, source: String)
}

// ------------------------------------------------------------- Public Functions

/// Checks if a module is registered as a valid live module.
/// Returns True if the module is in the registry.
///
pub fn is_valid_module(module: String) -> Bool {
  case read_registry() {
    Ok(modules) -> set.contains(modules, module)
    Error(_) -> False
  }
}

/// Registers a live module in the registry.
/// Called during template compilation.
///
pub fn register_module(module: String, source: String) -> Result(Nil, String) {
  let entries = case read_entries() {
    Ok(e) -> e
    Error(_) -> []
  }

  // Update or add entry
  let updated =
    entries
    |> list.filter(fn(e) { e.module != module })
    |> list.append([ModuleEntry(module:, source:)])

  write_entries(updated)
}

/// Unregisters a module from the registry.
/// Called when a template is deleted.
///
pub fn unregister_module(module: String) -> Result(Nil, String) {
  case read_entries() {
    Ok(entries) -> {
      let filtered = list.filter(entries, fn(e) { e.module != module })
      write_entries(filtered)
    }
    Error(_) -> Ok(Nil)
  }
}

/// Returns all registered modules.
///
pub fn list_modules() -> List(ModuleEntry) {
  case read_entries() {
    Ok(entries) -> entries
    Error(_) -> []
  }
}

/// Clears the registry and rebuilds from scratch.
/// Called during full recompilation.
///
pub fn clear() -> Result(Nil, String) {
  write_entries([])
}

// ------------------------------------------------------------- Private Functions

/// Reads the registry and returns a set of valid module names.
///
fn read_registry() -> Result(Set(String), Nil) {
  case read_entries() {
    Ok(entries) -> {
      entries
      |> list.map(fn(e) { e.module })
      |> set.from_list
      |> Ok
    }
    Error(_) -> Error(Nil)
  }
}

/// Reads the registry entries from disk.
///
fn read_entries() -> Result(List(ModuleEntry), Nil) {
  case simplifile.read(registry_path) {
    Error(_) -> Ok([])
    Ok(content) -> {
      case json.parse(content, entries_decoder()) {
        Ok(entries) -> Ok(entries)
        Error(_) -> Error(Nil)
      }
    }
  }
}

/// Writes registry entries to disk.
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

/// Decoder for registry entries.
///
fn entries_decoder() -> decode.Decoder(List(ModuleEntry)) {
  decode.list(entry_decoder())
}

/// Decoder for a single registry entry.
///
fn entry_decoder() -> decode.Decoder(ModuleEntry) {
  use module <- decode.field("module", decode.string)
  use source <- decode.field("source", decode.string)
  decode.success(ModuleEntry(module:, source:))
}
