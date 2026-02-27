//// Loom Live Dispatch
////
//// Generated template modules are determined at runtime by the
//// client's init message, so the live_socket actor can't call
//// them directly — Gleam requires compile-time module
//// references. This module uses Erlang's apply/3 via FFI to
//// bridge that gap, letting the runtime invoke handle_json and
//// render_json on any generated module by name.
////

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/result
import gleam/string

// ------------------------------------------------------------- Public Functions

/// Erlang's apply/3 expects module names as atoms, but Gleam
/// uses forward-slash paths (compiled/loom/counter). The BEAM
/// convention replaces slashes with @ for module atoms, so this
/// conversion is needed before every dynamic call.
///
pub fn module_to_atom(module: String) -> Atom {
  module
  |> string.replace("/", "@")
  |> atom.create
}

/// The live_socket actor needs to invoke the generated
/// handle_json function to process events, but the module is
/// only known at runtime. This wrapper marshals the handler ID,
/// current props, and special variables into dynamic args for
/// the FFI call.
///
pub fn call_handle_json(
  module: String,
  handler_id: String,
  props_json: String,
  value: String,
  checked: String,
  key: String,
) -> Result(String, String) {
  call_module_fn(module, "handle_json", [
    dynamic.string(handler_id),
    dynamic.string(props_json),
    dynamic.string(value),
    dynamic.string(checked),
    dynamic.string(key),
  ])
}

/// After handle_json updates the props, the actor needs fresh
/// HTML to send as a patch. Calling render_json dynamically
/// lets the runtime re-render any template module without
/// compile-time coupling to it.
///
pub fn call_render_json(
  module: String,
  props_json: String,
) -> Result(String, String) {
  call_module_fn(module, "render_json", [dynamic.string(props_json)])
}

/// After handle_json updates the props, the actor needs a fresh
/// tree JSON to diff against the previous tree. Calling
/// render_tree_json dynamically produces the statics/dynamics
/// JSON for any template module.
///
pub fn call_render_tree_json(
  module: String,
  props_json: String,
) -> Result(String, String) {
  call_module_fn(module, "render_tree_json", [dynamic.string(props_json)])
}

/// The registry needs to distinguish live modules from static
/// ones at startup. Probing for the is_live export via apply
/// avoids maintaining a separate manifest — the generated
/// module itself declares its liveness through the presence (or
/// absence) of this function.
///
pub fn is_live_module(module: String) -> Bool {
  let module_atom = module_to_atom(module)
  case erlang_apply(module_atom, atom.create("is_live"), []) {
    Ok(r) -> decode.run(r, decode.bool) == Ok(True)
    Error(_) -> False
  }
}

// ------------------------------------------------------------- Private Functions

/// Shared wrapper for all dynamic module calls. Converts the
/// module path to an atom, invokes the function via FFI, and
/// decodes the result as a string. Centralizing the error
/// handling here keeps the public call_* functions focused on
/// argument marshaling.
///
fn call_module_fn(
  module: String,
  function: String,
  args: List(Dynamic),
) -> Result(String, String) {
  use raw <- result.try(
    erlang_apply(module_to_atom(module), atom.create(function), args)
    |> result.map_error(fn(_) {
      "Failed to call " <> function <> " on module: " <> module
    }),
  )
  decode.run(raw, decode.string)
  |> result.map_error(fn(_) { function <> " did not return a string" })
}

// ------------------------------------------------------------- FFI Bindings

/// Wraps Erlang's apply/3 in a try/catch so dynamic dispatch
/// errors (undefined modules, missing functions) return a
/// Result instead of crashing the actor. The FFI lives in
/// glimr_loom_ffi.erl because Gleam has no native try/catch.
///
@external(erlang, "glimr_loom_ffi", "safe_apply")
fn erlang_apply(
  module: Atom,
  function: Atom,
  args: List(Dynamic),
) -> Result(Dynamic, Dynamic)
