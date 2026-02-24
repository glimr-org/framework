//// Loom Template Compiler
////
//// Compiles Loom template files (.loom.html) to executable
//// Gleam code. Handles dependency tracking between components
//// and their consumers for incremental compilation.
////

import gleam/bool
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import glimr/console/console
import glimr/filesystem/filesystem
import glimr/loom/generator.{type ComponentDataMap, type ComponentSlotMap}
import glimr/loom/lexer
import glimr/loom/loom
import glimr/loom/parser
import glimr/loom/registry
import shellout
import simplifile

// ------------------------------------------------------------- Public Functions

/// Entry point for full compilation. Components must be
/// compiled before pages so that component data/slot maps are
/// available during page generation.
///
pub fn run(verbose: Bool) -> Result(Nil, String) {
  let loom_files = loom.find_files()

  // Early return if there aren't any loom files.
  use <- bool.lazy_guard(list.is_empty(loom_files), fn() {
    io.println(console.warning(""))
    io.println(console.warning("No loom templates found"))

    Ok(Nil)
  })

  case verbose {
    True -> {
      console.output()
      |> console.line_warning("Compiling loom views...")
      |> console.print()
    }
    False -> Nil
  }

  // Stale entries would cause false positives during
  // module validation, so we rebuild from scratch.
  let _ = registry.clear()

  // Split files into components and pages
  let component_files = loom.find_components(loom_files)
  let page_files = loom.find_non_components(loom_files)

  // Build component data and slot maps in a single pass
  let #(data, slots) = build_component_maps(component_files)

  // Compile components
  use _ <- result.try(compile_files(component_files, data, slots, verbose))

  // Compile pages
  use _ <- result.try(compile_files(page_files, data, slots, verbose))

  // Run `gleam format` on the generated files
  format_generated_files()

  case verbose {
    True -> Nil
    False -> {
      console.output()
      |> console.line_success(
        "Compiled "
        <> int.to_string(list.length(loom_files))
        <> " Loom templates",
      )
      |> console.print()
    }
  }

  Ok(Nil)
}

// ------------------------------------------------------------- Private Functions

/// Iterates over files and stops on the first error so that the
/// user sees the most relevant failure instead of a cascade of
/// downstream errors.
///
fn compile_files(
  files: List(String),
  component_data: ComponentDataMap,
  component_slots: ComponentSlotMap,
  verbose: Bool,
) -> Result(Nil, String) {
  list.try_each(files, compile_file(_, component_data, component_slots, verbose))
}

/// Single-file entry point used by the file watcher. Validates
/// the path ends in .loom.html before attempting compilation to
/// prevent non-template files from entering the pipeline.
///
pub fn run_path(path: String, verbose: Bool) -> Result(Nil, String) {
  case string.ends_with(path, ".loom.html") {
    True -> compile_loom_path(path, verbose)
    False -> Error("Not a loom file: path must end with '.loom.html'")
  }
}

/// Handles both existing and deleted templates so the file
/// watcher can pass any path change without needing to
/// distinguish deletions from modifications.
///
fn compile_loom_path(path: String, verbose: Bool) -> Result(Nil, String) {
  // Check if file was deleted - if so, clean up generated file
  case simplifile.is_file(path) {
    Ok(True) -> compile_existing_loom_path(path, verbose)
    _ -> {
      cleanup_deleted_loom_file(path, verbose)
      Ok(Nil)
    }
  }
}

/// Routes to the correct compilation strategy because layouts
/// affect all templates, components cascade to dependents, and
/// regular templates stand alone.
///
fn compile_existing_loom_path(
  path: String,
  verbose: Bool,
) -> Result(Nil, String) {
  // Layout changes affect all templates
  use <- bool.lazy_guard(string.contains(path, "layouts/"), fn() {
    run(verbose)
  })

  // Component changes recompile dependents
  use <- bool.lazy_guard(string.contains(path, "components/"), fn() {
    compile_component_and_dependents(path, verbose)
  })

  case verbose {
    True ->
      console.output()
      |> console.line_warning("Compiling loom views...")
      |> console.print()
    False -> Nil
  }

  let #(component_data, component_slots) =
    build_component_maps(loom.find_components(loom.find_files()))

  use _ <- result.try(compile_file(
    path,
    component_data,
    component_slots,
    verbose,
  ))

  format_generated_files()

  case verbose {
    True -> Nil
    False -> io.println(console.success("Compiled 1 Loom template"))
  }

  Ok(Nil)
}

/// Prevents stale generated files from lingering after a source
/// template is deleted, which would cause compile errors or
/// serve outdated content.
///
fn cleanup_deleted_loom_file(path: String, verbose: Bool) -> Nil {
  let output_file = path_to_output_path(path)
  let module_name = path_to_module_name(path)

  // Unregister the module from the live registry
  let _ = registry.unregister_module(module_name)

  let file_exists = simplifile.is_file(output_file) |> result.unwrap(False)
  use <- bool.lazy_guard(!file_exists, fn() { Nil })

  case simplifile.delete(output_file), verbose {
    Ok(_), True -> {
      console.output()
      |> console.line_success("Cleaned up generated file for deleted template:")
      |> console.line("  " <> output_file)
      |> console.print()
    }
    Ok(_), False -> io.println(console.success("Cleaned up 1 generated file"))
    Error(_), _ -> {
      console.output()
      |> console.line_error("Failed to delete: " <> output_file)
      |> console.print()
    }
  }
}

/// Component changes cascade to consumers because the generated
/// code for each consumer depends on the component's props and
/// slot definitions.
///
fn compile_component_and_dependents(
  component_template_path: String,
  verbose: Bool,
) -> Result(Nil, String) {
  // Extract component name from template path
  // e.g., src/resources/views/components/forms/input.loom.html -> "forms.input"
  let component_name =
    component_name_from_template_path(component_template_path)
  let component_tag = "<x-" <> component_name

  // Find all templates that use this component
  let all_templates = loom.find_files()
  let dependent_templates =
    all_templates
    |> list.filter(fn(template_path) {
      template_path != component_template_path
      && {
        case simplifile.read(template_path) {
          Ok(content) -> string.contains(content, component_tag)
          Error(_) -> False
        }
      }
    })

  // Build list: component first, then dependents
  let templates_to_compile = [component_template_path, ..dependent_templates]

  case verbose {
    True ->
      console.output()
      |> console.line_warning(
        "Compiling "
        <> component_name
        <> " and "
        <> int.to_string(list.length(dependent_templates))
        <> " dependent template(s)...",
      )
      |> console.print()
    False -> Nil
  }

  let #(component_data, component_slots) =
    build_component_maps(loom.find_components(loom.find_files()))

  use _ <- result.try(compile_files(
    templates_to_compile,
    component_data,
    component_slots,
    verbose,
  ))

  format_generated_files()

  case verbose {
    True -> Nil
    False ->
      io.println(
        console.success("Compiled ")
        <> int.to_string(list.length(templates_to_compile))
        <> " Loom template(s)",
      )
  }

  Ok(Nil)
}

/// Derives the component name used in template tags from the
/// file path. Subdirectories become colon-separated prefixes
/// (e.g., forms/input.loom.html -> "forms:input").
///
fn component_name_from_template_path(path: String) -> String {
  path
  |> string.replace(loom.views_path <> "components/", "")
  |> string.replace(".loom.html", "")
  |> string.replace("/", ":")
}

/// Generated code is machine-written and unformatted, so we run
/// gleam format to keep it readable for debugging and to match
/// the project's code style.
///
fn format_generated_files() -> Nil {
  let _ = shellout.command("gleam", ["format", loom.output_path], ".", [])
  Nil
}

/// Runs the full compilation pipeline for a single file.
/// Validation happens before generation so invalid templates
/// fail fast without producing broken output.
///
fn compile_file(
  path: String,
  component_data: generator.ComponentDataMap,
  component_slots: generator.ComponentSlotMap,
  verbose: Bool,
) -> Result(Nil, String) {
  let is_component = string.contains(path, "components/")

  use content <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(_) { "Failed to read file: " <> path }),
  )
  use tokens <- result.try(
    lexer.tokenize(content)
    |> result.map_error(fn(err) {
      "Lexer error in " <> path <> ": " <> lexer_error_to_string(err)
    }),
  )
  use template <- result.try(
    parser.parse(tokens)
    |> result.map_error(fn(err) {
      "Parser error in " <> path <> ": " <> parser_error_to_string(err)
    }),
  )
  use _ <- result.try(generator.validate_template(template, path))

  let module_name = path_to_module_name(path)

  let generated = {
    generator.generate(
      template,
      module_name,
      is_component,
      component_data,
      component_slots,
    )
  }

  let output_file = path_to_output_path(path)
  let _ = filesystem.ensure_directory_exists(output_file)

  use _ <- result.try(
    simplifile.write(output_file, generated.code)
    |> result.map_error(fn(_) { "Failed to write: " <> output_file }),
  )

  // Register live modules in the registry (pages and components alike,
  // since live components now get their own data-l-live containers)
  case template.is_live {
    True -> {
      let _ = registry.register_module(module_name, path)
      Nil
    }
    False -> Nil
  }

  case verbose {
    True -> {
      console.output()
      |> console.line("  " <> path <> " -> " <> console.success(output_file))
      |> console.print()
    }
    False -> Nil
  }

  Ok(Nil)
}

/// Builds both the component data map and slot map in a single
/// pass over the component files. Avoids reading and parsing
/// each component template twice.
///
fn build_component_maps(
  component_files: List(String),
) -> #(ComponentDataMap, ComponentSlotMap) {
  let entries = {
    list.filter_map(component_files, fn(path) {
      // Read the file contents from disk
      use content <- result.try(
        simplifile.read(path) |> result.replace_error(Nil),
      )

      // Tokenize the raw content into lexer tokens
      use tokens <- result.try(
        lexer.tokenize(content) |> result.replace_error(Nil),
      )

      // Parse the tokens into a template AST
      use template <- result.try(
        parser.parse(tokens) |> result.replace_error(Nil),
      )

      let name = component_name_from_template_path(path)
      let slot_info = generator.extract_slot_info(template)

      Ok(#(name, template.props, template.is_live, slot_info))
    })
  }

  let #(data_map, slot_map) = {
    entries
    |> list.fold(#(dict.new(), dict.new()), fn(acc, e) {
      let #(data, slots) = acc
      let #(name, props, is_live, slot_info) = e

      #(
        dict.insert(data, name, generator.ComponentData(props:, is_live:)),
        dict.insert(slots, name, slot_info),
      )
    })
  }

  #(data_map, slot_map)
}

/// The module name must match the output file location so that
/// Gleam's import system can find the compiled template (e.g.,
/// "compiled/loom/pages/home").
///
fn path_to_module_name(path: String) -> String {
  let relative =
    path
    |> string.replace(loom.views_path, "")
    |> string.replace(".loom.html", "")

  // Remove leading slash if present
  let relative = case string.starts_with(relative, "/") {
    True -> string.drop_start(relative, 1)
    False -> relative
  }

  "compiled/loom/" <> relative
}

/// Maps source template paths to their compiled output location
/// so generated .gleam files mirror the same directory
/// structure as the source templates.
///
fn path_to_output_path(path: String) -> String {
  let relative =
    path
    |> string.replace(loom.views_path, "")
    |> string.replace(".loom.html", ".gleam")

  loom.output_path <> relative
}

/// Translates internal lexer errors into user-facing messages
/// with position info so developers can locate the problem in
/// their template source.
///
fn lexer_error_to_string(err: lexer.LexerError) -> String {
  case err {
    lexer.UnterminatedExpression(pos) ->
      "Unterminated expression at position " <> int.to_string(pos)
    lexer.EmptyExpression(pos) ->
      "Empty expression at position " <> int.to_string(pos)
    lexer.UnterminatedDirective(pos) ->
      "Unterminated directive at position " <> int.to_string(pos)
    lexer.InvalidDirective(dir, pos) ->
      "Invalid directive '" <> dir <> "' at position " <> int.to_string(pos)
    lexer.InvalidLmForSyntax(content, pos) ->
      "Invalid l-for syntax '"
      <> content
      <> "' at position "
      <> int.to_string(pos)
      <> " (expected: l-for=\"item in items\")"
    lexer.UnterminatedComponent(pos) ->
      "Unterminated component tag at position " <> int.to_string(pos)
    lexer.InvalidComponentSyntax(content, pos) ->
      "Invalid component syntax '"
      <> content
      <> "' at position "
      <> int.to_string(pos)
    lexer.InvalidPropsDirective(content, line) ->
      "Invalid @props directive '"
      <> content
      <> "' at line "
      <> int.to_string(line)
      <> " (expected: @props(name: Type, ...))"
    lexer.InvalidImportDirective(content, line) ->
      "Invalid @import directive '"
      <> content
      <> "' at line "
      <> int.to_string(line)
    lexer.UnterminatedPropsDirective(line) ->
      "Unterminated @props directive at line " <> int.to_string(line)
    lexer.UnterminatedImportDirective(line) ->
      "Unterminated @import directive at line " <> int.to_string(line)
  }
}

/// Translates internal parser errors into user-facing messages
/// with context so developers can understand and fix structural
/// issues in their templates.
///
fn parser_error_to_string(err: parser.ParserError) -> String {
  case err {
    parser.UnexpectedLmElse -> "Unexpected l-else without preceding l-if"
    parser.UnexpectedLmElseIf -> "Unexpected l-else-if without preceding l-if"
    parser.LmElseAfterLmElse -> "Cannot have l-else after another l-else"
    parser.LmElseIfAfterLmElse -> "Cannot have l-else-if after l-else"
    parser.UnexpectedToken(_) -> "Unexpected token"
    parser.UnexpectedComponentEnd(name) ->
      "Unexpected </x-" <> name <> "> without matching opening tag"
    parser.UnexpectedElementEnd(tag) ->
      "Unexpected </" <> tag <> "> without matching opening tag"
    parser.UnclosedComponent(name) ->
      "Unclosed <x-" <> name <> "> - missing closing tag"
    parser.UnclosedElement(tag) ->
      "Unclosed <" <> tag <> "> - missing closing tag"
    parser.UnexpectedSlotDefEnd -> "Unexpected </slot> without matching <slot>"
    parser.UnclosedSlot(name) ->
      case name {
        option.Some(n) ->
          "Unclosed <slot name=\"" <> n <> "\"> - missing </slot>"
        option.None -> "Unclosed <slot> - missing </slot>"
      }
    parser.DirectiveAfterContent(directive, line) ->
      directive
      <> " directive at line "
      <> int.to_string(line)
      <> " must appear before template content"
    parser.DuplicatePropsDirective(line) ->
      "Duplicate @props directive at line "
      <> int.to_string(line)
      <> " - combine into single @props"
  }
}
