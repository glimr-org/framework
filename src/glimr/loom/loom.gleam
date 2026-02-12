//// Loom
////
//// Shared types and path conventions for the Loom template
//// system. Both the compiler and the live runtime need to
//// agree on file locations, event structures, and actor
//// messages — centralizing them here prevents drift between
//// compile-time and runtime assumptions.
////

import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import glimr/response/response
import simplifile

// ------------------------------------------------------------- Public Constants

/// The compiler, file watcher, and response module all need to 
/// locate template source files. Deriving from 
/// response.views_path keeps this single-sourced so changing 
/// the views directory doesn't require updating multiple 
/// locations.
///
pub const views_path = response.views_path

/// Generated Gleam modules go here so they're compiled by the 
/// Gleam build system alongside hand-written code. A dedicated 
/// output directory keeps generated files separate and easy to 
/// gitignore or clean.
///
pub const output_path = "src/compiled/loom/"

/// Template authors can place Gleam modules alongside their
/// templates (e.g., for live event handlers). The file watcher 
/// monitors this path to trigger recompilation when backing 
/// Gleam code changes.
///
pub const app_path = "src/app/loom/"

// ------------------------------------------------------------- Public Types

/// The WebSocket runtime receives events as JSON from the
/// client and needs a typed representation to dispatch them. 
/// Bundling handler ID, event name, and special variables into 
/// one record lets the dispatch function pattern match without 
/// unpacking raw JSON fields.
///
pub type ClientEvent {
  ClientEvent(handler: String, event: String, special_vars: SpecialVars)
}

/// Browser events carry context (input value, checkbox state, 
/// key pressed) that handlers may reference via $value/$checked
/// /$key. Wrapping them as Options reflects that each is only 
/// present for certain event types — an input has $value, a 
/// checkbox has $checked.
///
pub type SpecialVars {
  SpecialVars(value: Option(String), checked: Option(Bool), key: Option(String))
}

/// A template split into static HTML fragments and dynamic
/// expressions. Statics never change between renders; only
/// dynamics are diffed and sent over the WebSocket.
///
pub type LiveTree {
  LiveTree(statics: List(String), dynamics: List(Dynamic))
}

/// A single dynamic slot in a LiveTree. Can be a leaf string,
/// a nested subtree (conditional/component), or a list of
/// subtrees (loop output).
///
pub type Dynamic {
  DynString(String)
  DynTree(LiveTree)
  DynList(List(LiveTree))
}

/// The live socket actor communicates with the WebSocket
/// handler via typed messages. Separating event receipt, HTML
/// patches, redirects, and shutdown into variants gives the
/// actor a clear protocol without ad-hoc string-based message
/// passing.
///
pub type SocketMessage {
  /// Event received from the client via WebSocket
  Event(ClientEvent)
  /// Send initial statics + dynamics tree to client
  SendTrees(id: String, json: String)
  /// Send changed dynamics only to client
  SendPatch(id: String, diff: String)
  /// Send a redirect back to the client
  SendRedirect(url: String)
  /// Stop the actor
  Stop
}

// ------------------------------------------------------------- Public Functions

/// The file watcher triggers on any file change, but only
/// .loom.html files under views_path should cause a
/// recompilation. This guard prevents wasted compile cycles 
/// when non-template files are modified in the same directory 
/// tree.
///
pub fn is_views_path(path: String) -> Bool {
  string.starts_with(path, views_path)
}

/// Changes to backing Gleam files (event handlers, helpers)
/// require recompiling the corresponding template so the
/// generated code stays in sync. This guard identifies relevant 
/// Gleam file changes without reacting to unrelated source 
/// modifications.
///
pub fn is_app_path(path: String) -> Bool {
  string.starts_with(path, app_path)
}

/// The compiler needs a complete list of template files to
/// process during a full build. Filtering by .loom.html
/// extension ensures only template files are collected,
/// excluding any non-template files that may live in the views 
/// directory.
///
pub fn find_files() -> List(String) {
  simplifile.get_files(views_path)
  |> result.unwrap([])
  |> list.filter(string.ends_with(_, ".loom.html"))
}

/// Components must be compiled before pages because pages
/// reference component slot and prop information during code 
/// generation. Separating component files lets the compiler 
/// process them first to build the required metadata maps.
///
pub fn find_components(files: List(String)) -> List(String) {
  list.filter(files, string.contains(_, "components/"))
}

/// Pages are compiled after components, once all component
/// metadata (props, slots) is available. Filtering out
/// component files from the full list gives the compiler just 
/// the page templates for the second compilation pass.
///
pub fn find_non_components(files: List(String)) -> List(String) {
  use file <- list.filter(files)
  !string.contains(file, "components/")
}
