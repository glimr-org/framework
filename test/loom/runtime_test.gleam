import dot_env/env
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import gleeunit/should
import glimr/loom/loom.{DynList, DynString, DynTree, LiveTree}
import glimr/loom/runtime

// ------------------------------------------------------------- inject_live_wrapper Tests

fn test_html() -> String {
  "<html><head></head><body><p>Hello</p></body></html>"
}

pub fn inject_live_wrapper_emits_data_l_token_test() {
  let assert Ok(_) = env.set("APP_KEY", "test-secret-key")
  let result =
    runtime.inject_live_wrapper(test_html(), "counter", "{\"count\":0}")

  string.contains(result, "data-l-token=\"")
  |> should.be_true

  string.contains(result, "data-l-props")
  |> should.be_false
}

pub fn inject_live_wrapper_token_is_verifiable_test() {
  let secret = "test-secret-key"
  let assert Ok(_) = env.set("APP_KEY", secret)
  let result =
    runtime.inject_live_wrapper(test_html(), "counter", "{\"count\":0}")

  // Extract the token from data-l-token="..."
  let assert Ok(#(_, after_token)) =
    string.split_once(result, "data-l-token=\"")
  let assert Ok(#(token, _)) = string.split_once(after_token, "\"")

  // Verify the token with the same secret
  let assert Ok(payload_bits) =
    crypto.verify_signed_message(token, <<secret:utf8>>)
  let assert Ok(payload) = bit_array.to_string(payload_bits)

  payload
  |> should.equal("counter:{\"count\":0}")
}

pub fn inject_live_wrapper_token_binds_module_test() {
  let secret = "test-secret-key"
  let assert Ok(_) = env.set("APP_KEY", secret)
  let result = runtime.inject_live_wrapper(test_html(), "my_module", "{}")

  let assert Ok(#(_, after_token)) =
    string.split_once(result, "data-l-token=\"")
  let assert Ok(#(token, _)) = string.split_once(after_token, "\"")

  let assert Ok(payload_bits) =
    crypto.verify_signed_message(token, <<secret:utf8>>)
  let assert Ok(payload) = bit_array.to_string(payload_bits)

  // Payload starts with the module name
  string.starts_with(payload, "my_module:")
  |> should.be_true
}

pub fn inject_live_wrapper_token_rejected_with_wrong_key_test() {
  let assert Ok(_) = env.set("APP_KEY", "correct-key")
  let result =
    runtime.inject_live_wrapper(test_html(), "counter", "{\"count\":0}")

  let assert Ok(#(_, after_token)) =
    string.split_once(result, "data-l-token=\"")
  let assert Ok(#(token, _)) = string.split_once(after_token, "\"")

  // Verification with wrong key should fail
  crypto.verify_signed_message(token, <<"wrong-key":utf8>>)
  |> should.be_error
}

// ------------------------------------------------------------- build_classes Tests

pub fn build_classes_all_true_test() {
  runtime.build_classes([
    #("p-4", True),
    #("font-bold", True),
    #("text-red", True),
  ])
  |> should.equal("p-4 font-bold text-red")
}

pub fn build_classes_some_false_test() {
  runtime.build_classes([
    #("p-4", True),
    #("font-bold", False),
    #("text-red", True),
  ])
  |> should.equal("p-4 text-red")
}

pub fn build_classes_all_false_test() {
  runtime.build_classes([#("p-4", False), #("font-bold", False)])
  |> should.equal("")
}

pub fn build_classes_empty_list_test() {
  runtime.build_classes([])
  |> should.equal("")
}

pub fn build_classes_single_true_test() {
  runtime.build_classes([#("active", True)])
  |> should.equal("active")
}

pub fn build_classes_single_false_test() {
  runtime.build_classes([#("active", False)])
  |> should.equal("")
}

pub fn build_classes_preserves_spaces_in_classes_test() {
  // Multiple classes in a single string are preserved
  runtime.build_classes([#("hover:bg-blue-500 focus:outline-none", True)])
  |> should.equal("hover:bg-blue-500 focus:outline-none")
}

// ------------------------------------------------------------- class() Helper Tests

pub fn class_helper_returns_always_true_tuple_test() {
  runtime.class("btn primary")
  |> should.equal(#("btn primary", True))
}

pub fn class_helper_works_with_build_classes_test() {
  runtime.build_classes([
    runtime.class("btn"),
    runtime.class("primary"),
    #("active", True),
    #("disabled", False),
  ])
  |> should.equal("btn primary active")
}

pub fn class_helper_with_empty_string_test() {
  runtime.class("")
  |> should.equal(#("", True))
}

pub fn class_helper_mixed_static_and_conditional_test() {
  // The new syntax: :class="[class('static classes'), #('conditional', cond)]"
  let is_active = True
  let is_disabled = False
  runtime.build_classes([
    runtime.class("btn lg rounded"),
    #("active", is_active),
    #("disabled", is_disabled),
  ])
  |> should.equal("btn lg rounded active")
}

// ------------------------------------------------------------- build_styles Tests

pub fn build_styles_all_true_test() {
  runtime.build_styles([
    #("color: red", True),
    #("font-size: 16px", True),
    #("margin: 0", True),
  ])
  |> should.equal("color: red font-size: 16px margin: 0")
}

pub fn build_styles_some_false_test() {
  runtime.build_styles([
    #("color: red", True),
    #("font-size: 16px", False),
    #("margin: 0", True),
  ])
  |> should.equal("color: red margin: 0")
}

pub fn build_styles_all_false_test() {
  runtime.build_styles([#("color: red", False), #("margin: 0", False)])
  |> should.equal("")
}

pub fn build_styles_empty_list_test() {
  runtime.build_styles([])
  |> should.equal("")
}

pub fn build_styles_single_true_test() {
  runtime.build_styles([#("display: none", True)])
  |> should.equal("display: none")
}

pub fn build_styles_single_false_test() {
  runtime.build_styles([#("display: none", False)])
  |> should.equal("")
}

pub fn build_styles_with_semicolons_test() {
  runtime.build_styles([#("color: red;", True), #("margin: 0;", True)])
  |> should.equal("color: red; margin: 0;")
}

// ------------------------------------------------------------- style() Helper Tests

pub fn style_helper_returns_always_true_tuple_test() {
  runtime.style("color: red; margin: 0")
  |> should.equal(#("color: red; margin: 0", True))
}

pub fn style_helper_works_with_build_styles_test() {
  runtime.build_styles([
    runtime.style("color: red"),
    runtime.style("font-size: 16px"),
    #("display: none", True),
    #("opacity: 0", False),
  ])
  |> should.equal("color: red font-size: 16px display: none")
}

pub fn style_helper_with_empty_string_test() {
  runtime.style("")
  |> should.equal(#("", True))
}

pub fn style_helper_mixed_static_and_conditional_test() {
  // The new syntax: :style="[style('color: red; margin: 0'), #('display: none', is_hidden)]"
  let is_hidden = True
  let is_faded = False
  runtime.build_styles([
    runtime.style("color: red; padding: 10px"),
    #("display: none", is_hidden),
    #("opacity: 0.5", is_faded),
  ])
  |> should.equal("color: red; padding: 10px display: none")
}

// ------------------------------------------------------------- flatten_tree Tests

pub fn flatten_tree_simple_test() {
  let tree =
    LiveTree(statics: ["<p>Count: ", "</p>"], dynamics: [DynString("42")])
  runtime.flatten_tree(tree)
  |> should.equal("<p>Count: 42</p>")
}

pub fn flatten_tree_multiple_dynamics_test() {
  let tree =
    LiveTree(statics: ["<p>", " and ", "</p>"], dynamics: [
      DynString("hello"),
      DynString("world"),
    ])
  runtime.flatten_tree(tree)
  |> should.equal("<p>hello and world</p>")
}

pub fn flatten_tree_no_dynamics_test() {
  let tree = LiveTree(statics: ["<p>Static only</p>"], dynamics: [])
  runtime.flatten_tree(tree)
  |> should.equal("<p>Static only</p>")
}

pub fn flatten_tree_nested_subtree_test() {
  let inner = LiveTree(statics: ["<b>", "</b>"], dynamics: [DynString("bold")])
  let tree = LiveTree(statics: ["<p>", "</p>"], dynamics: [DynTree(inner)])
  runtime.flatten_tree(tree)
  |> should.equal("<p><b>bold</b></p>")
}

pub fn flatten_tree_list_dynamics_test() {
  let items = [
    LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("a")]),
    LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("b")]),
  ]
  let tree = LiveTree(statics: ["<ul>", "</ul>"], dynamics: [DynList(items)])
  runtime.flatten_tree(tree)
  |> should.equal("<ul><li>a</li><li>b</li></ul>")
}

// ------------------------------------------------------------- tree_to_json Tests

pub fn tree_to_json_simple_test() {
  let tree = LiveTree(statics: ["<p>", "</p>"], dynamics: [DynString("42")])
  let result = runtime.tree_to_json(tree)
  // Parse and verify structure
  let assert Ok(parsed) =
    json.parse(result, {
      use s <- decode.field("s", decode.list(decode.string))
      use d <- decode.field("d", decode.list(decode.string))
      decode.success(#(s, d))
    })
  parsed.0 |> should.equal(["<p>", "</p>"])
  parsed.1 |> should.equal(["42"])
}

pub fn tree_to_json_nested_test() {
  let inner = LiveTree(statics: ["<b>", "</b>"], dynamics: [DynString("x")])
  let tree = LiveTree(statics: ["<p>", "</p>"], dynamics: [DynTree(inner)])
  let result = runtime.tree_to_json(tree)
  // Should contain nested object with s and d keys
  string.contains(result, "\"s\"")
  |> should.be_true
  string.contains(result, "\"d\"")
  |> should.be_true
}

// ------------------------------------------------------------- diff_tree_json Tests

pub fn diff_tree_json_no_changes_test() {
  let tree = LiveTree(statics: ["<p>", "</p>"], dynamics: [DynString("42")])
  let tree_json = runtime.tree_to_json(tree)
  runtime.diff_tree_json(tree_json, tree_json)
  |> should.equal("{}")
}

pub fn diff_tree_json_single_value_changed_test() {
  let old = LiveTree(statics: ["<p>", "</p>"], dynamics: [DynString("0")])
  let new = LiveTree(statics: ["<p>", "</p>"], dynamics: [DynString("1")])
  let diff =
    runtime.diff_tree_json(runtime.tree_to_json(old), runtime.tree_to_json(new))
  // Diff should contain index "0" with value "1"
  string.contains(diff, "\"0\"")
  |> should.be_true
  string.contains(diff, "\"1\"")
  |> should.be_true
}

pub fn diff_tree_json_multiple_values_only_changed_test() {
  let old =
    LiveTree(statics: ["<p>", " ", "</p>"], dynamics: [
      DynString("a"),
      DynString("b"),
    ])
  let new =
    LiveTree(statics: ["<p>", " ", "</p>"], dynamics: [
      DynString("a"),
      DynString("c"),
    ])
  let diff =
    runtime.diff_tree_json(runtime.tree_to_json(old), runtime.tree_to_json(new))
  // Only index "1" should be in the diff (index "0" unchanged)
  string.contains(diff, "\"1\"")
  |> should.be_true
  string.contains(diff, "\"0\"")
  |> should.be_false
}

pub fn diff_tree_json_branch_flip_sends_full_subtree_test() {
  let old =
    LiveTree(statics: ["", ""], dynamics: [
      DynTree(LiveTree(statics: ["<p>yes</p>"], dynamics: [])),
    ])
  let new =
    LiveTree(statics: ["", ""], dynamics: [
      DynTree(LiveTree(statics: ["<p>no</p>"], dynamics: [])),
    ])
  let diff =
    runtime.diff_tree_json(runtime.tree_to_json(old), runtime.tree_to_json(new))
  // Full subtree with "s" key should be in diff
  string.contains(diff, "\"s\"")
  |> should.be_true
  string.contains(diff, "no")
  |> should.be_true
}

pub fn diff_tree_json_list_length_change_sends_full_list_test() {
  let old =
    LiveTree(statics: ["<ul>", "</ul>"], dynamics: [
      DynList([
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("a")]),
      ]),
    ])
  let new =
    LiveTree(statics: ["<ul>", "</ul>"], dynamics: [
      DynList([
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("a")]),
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("b")]),
      ]),
    ])
  let diff =
    runtime.diff_tree_json(runtime.tree_to_json(old), runtime.tree_to_json(new))
  // Should contain the full list (array)
  diff |> should.not_equal("{}")
  string.contains(diff, "\"0\"")
  |> should.be_true
}

pub fn diff_tree_json_same_length_list_per_item_diff_test() {
  let old =
    LiveTree(statics: ["<ul>", "</ul>"], dynamics: [
      DynList([
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("a")]),
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("b")]),
      ]),
    ])
  let new =
    LiveTree(statics: ["<ul>", "</ul>"], dynamics: [
      DynList([
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("a")]),
        LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("c")]),
      ]),
    ])
  let diff =
    runtime.diff_tree_json(runtime.tree_to_json(old), runtime.tree_to_json(new))
  // Only item 1 changed, item 0 unchanged
  diff |> should.not_equal("{}")
  string.contains(diff, "\"c\"")
  |> should.be_true
}

// ------------------------------------------------------------- map_each Tests

pub fn map_each_returns_dyn_list_test() {
  let result =
    runtime.map_each(["a", "b", "c"], fn(item) {
      LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString(item)])
    })
  case result {
    DynList(trees) -> {
      let html =
        trees
        |> list.map(runtime.flatten_tree)
        |> string.join("")
      html |> should.equal("<li>a</li><li>b</li><li>c</li>")
    }
    _ -> should.fail()
  }
}

pub fn map_each_empty_list_test() {
  let result =
    runtime.map_each([], fn(_item: String) {
      LiveTree(statics: ["<li>", "</li>"], dynamics: [DynString("x")])
    })
  case result {
    DynList(trees) -> list.length(trees) |> should.equal(0)
    _ -> should.fail()
  }
}

pub fn map_each_with_loop_test() {
  let result =
    runtime.map_each_with_loop(["x", "y"], fn(item, loop) {
      let idx = int.to_string(loop.index)
      LiveTree(statics: ["<li>", ": ", "</li>"], dynamics: [
        DynString(idx),
        DynString(item),
      ])
    })
  case result {
    DynList(trees) -> {
      let html =
        trees
        |> list.map(runtime.flatten_tree)
        |> string.join("")
      html |> should.equal("<li>0: x</li><li>1: y</li>")
    }
    _ -> should.fail()
  }
}
