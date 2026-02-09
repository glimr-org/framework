//// Loom Live Dispatch
////
//// Erlang FFI for dynamic module dispatch. Uses Erlang's apply/3 to call
//// functions on modules determined at runtime, enabling the framework to
//// call generated template functions without compile-time dependencies.
////

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/string

/// Converts a module path string to an Erlang atom.
/// Gleam modules use @ as path separator in Erlang.
/// e.g., "compiled/loom/counter" -> :'compiled@loom@counter'
///
pub fn module_to_atom(module: String) -> Atom {
  module
  |> string.replace("/", "@")
  |> atom.create
}

/// Calls handle_json on a template module dynamically.
/// Returns the updated props as a JSON string.
///
pub fn call_handle_json(
  module: String,
  handler_id: String,
  props_json: String,
  value: String,
  checked: String,
  key: String,
) -> Result(String, String) {
  let module_atom = module_to_atom(module)
  let args = [
    dynamic.string(handler_id),
    dynamic.string(props_json),
    dynamic.string(value),
    dynamic.string(checked),
    dynamic.string(key),
  ]

  case erlang_apply(module_atom, atom.create("handle_json"), args) {
    Ok(result) -> {
      case decode.run(result, decode.string) {
        Ok(json) -> Ok(json)
        Error(_) -> Error("handle_json did not return a string")
      }
    }
    Error(_) -> Error("Failed to call handle_json on module: " <> module)
  }
}

/// Calls render_json on a template module dynamically.
/// Returns the rendered HTML string.
///
pub fn call_render_json(
  module: String,
  props_json: String,
) -> Result(String, String) {
  let module_atom = module_to_atom(module)
  let args = [dynamic.string(props_json)]

  case erlang_apply(module_atom, atom.create("render_json"), args) {
    Ok(result) -> {
      case decode.run(result, decode.string) {
        Ok(html) -> Ok(html)
        Error(_) -> Error("render_json did not return a string")
      }
    }
    Error(_) -> Error("Failed to call render_json on module: " <> module)
  }
}

/// Checks if a module exports the is_live function and returns True.
///
pub fn is_live_module(module: String) -> Bool {
  let module_atom = module_to_atom(module)
  case erlang_apply(module_atom, atom.create("is_live"), []) {
    Ok(result) -> {
      case decode.run(result, decode.bool) {
        Ok(True) -> True
        _ -> False
      }
    }
    Error(_) -> False
  }
}

/// Erlang's apply/3 function for dynamic dispatch.
/// Calls Module:Function(Args) and returns the result.
///
@external(erlang, "glimr_loom_ffi", "safe_apply")
fn erlang_apply(
  module: Atom,
  function: Atom,
  args: List(Dynamic),
) -> Result(Dynamic, Dynamic)
