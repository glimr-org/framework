//// Loom Template Compiler
////
//// Compiles Loom template files (.loom.html) to executable Gleam
//// code. Handles dependency tracking between components and
//// their consumers for incremental compilation.
////

import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import glimr/console/console
import glimr/filesystem/filesystem
import glimr/loom/generator
import glimr/loom/lexer
import glimr/loom/parser
import glimr/loom/registry
import shellout
import simplifile

const views_path = "src/resources/views/"

const output_path = "src/compiled/loom/"

/// Compiles all loom template files. Discovers templates in
/// the views directory, compiles components first, then pages,
/// and validates the generated code.
///
pub fn run(verbose: Bool) -> Result(Nil, String) {
  let all_files = find_loom_files(views_path)

  case all_files {
    [] -> {
      io.println(console.warning(""))
      io.println(console.warning("No loom templates found"))
      Ok(Nil)
    }
    _ -> {
      case verbose {
        True ->
          console.output()
          |> console.line_warning("Compiling loom views...")
          |> console.print()
        False -> Nil
      }

      // Clear the live module registry before full recompilation
      let _ = registry.clear_registry()

      // Build component data map first
      let component_data = build_component_data_map()

      // Compile component files (they may depend on other components)
      let component_files =
        list.filter(all_files, fn(f) { string.contains(f, "components/") })
      let page_files =
        list.filter(all_files, fn(f) { !string.contains(f, "components/") })

      // Build component slot map (component name -> slot info)
      let component_slots = build_component_slot_map(component_files)

      // Compile components first, stop on first error
      case
        compile_files(component_files, component_data, component_slots, verbose)
      {
        Error(err) -> Error(err)
        Ok(_) -> {
          // Then compile page files
          case
            compile_files(page_files, component_data, component_slots, verbose)
          {
            Error(err) -> Error(err)
            Ok(_) -> {
              format_generated_files()
              case verbose {
                True -> Nil
                False ->
                  io.println(console.success("Compiled loom templates..."))
              }
              Ok(Nil)
            }
          }
        }
      }
    }
  }
}

/// Compiles a list of files, stopping on the first error.
///
fn compile_files(
  files: List(String),
  component_data: generator.ComponentDataMap,
  component_slots: generator.ComponentSlotMap,
  verbose: Bool,
) -> Result(Nil, String) {
  case files {
    [] -> Ok(Nil)
    [file, ..rest] -> {
      case compile_file(file, component_data, component_slots, verbose) {
        Error(err) -> Error(err)
        Ok(_) -> compile_files(rest, component_data, component_slots, verbose)
      }
    }
  }
}

/// Compiles a single loom template by path.
///
pub fn run_path(path: String, verbose: Bool) -> Result(Nil, String) {
  case string.ends_with(path, ".loom.html") {
    True -> compile_loom_path(path, verbose)
    False -> Error("Not a loom file: path must end with '.loom.html'")
  }
}

/// Compiles a loom template path. Checks if the file exists
/// and either compiles it or cleans up the generated file
/// if the source was deleted.
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

/// Compiles an existing loom template. Routes layout changes
/// to full recompilation, component changes to dependency
/// recompilation, and regular templates to single compilation.
///
fn compile_existing_loom_path(
  path: String,
  verbose: Bool,
) -> Result(Nil, String) {
  case string.contains(path, "layouts/") {
    // Layout changes affect all templates
    True -> run(verbose)
    False -> {
      case string.contains(path, "components/") {
        // Component template changed - recompile it + all templates using it
        True -> compile_component_template_and_dependents(path, verbose)
        // Regular template - just compile it
        False -> {
          case verbose {
            True ->
              console.output()
              |> console.line_warning("Compiling loom views...")
              |> console.print()
            False -> Nil
          }

          let component_data = build_component_data_map()
          let component_slots = build_component_slot_map_all()
          case compile_file(path, component_data, component_slots, verbose) {
            Ok(_) -> {
              format_generated_files()
              case verbose {
                True -> Nil
                False -> io.println(console.success("Compiled 1 loom template"))
              }
              Ok(Nil)
            }
            Error(err) -> Error(err)
          }
        }
      }
    }
  }
}

/// Cleans up the generated file when a source template is
/// deleted. Removes the corresponding .gleam file from the
/// compiled/loom directory.
///
fn cleanup_deleted_loom_file(path: String, verbose: Bool) -> Nil {
  let output_file = path_to_output_path(path)
  let module_name = path_to_module_name(path)

  // Unregister the module from the live registry
  let _ = registry.unregister_module(module_name)

  case simplifile.is_file(output_file) {
    Ok(True) -> {
      case simplifile.delete(output_file) {
        Ok(_) -> {
          case verbose {
            True ->
              console.output()
              |> console.line_success(
                "Cleaned up generated file for deleted template:",
              )
              |> console.line("  " <> output_file)
              |> console.print()
            False -> io.println(console.success("Cleaned up 1 generated file"))
          }
        }
        Error(_) -> {
          io.println(console.error("Failed to delete: " <> output_file))
        }
      }
    }
    _ -> Nil
  }
}

/// Compiles a component template and all dependents. Finds
/// all templates that use the component and recompiles them
/// to ensure consistent generated code.
///
fn compile_component_template_and_dependents(
  component_template_path: String,
  verbose: Bool,
) -> Result(Nil, String) {
  // Extract component name from template path
  // e.g., src/resources/views/components/forms/input.loom.html -> "forms.input"
  let component_name =
    component_name_from_template_path(component_template_path)
  let component_tag = "<x-" <> component_name

  // Find all templates that use this component
  let all_templates = find_loom_files(views_path)
  let dependent_templates =
    all_templates
    |> list.filter(fn(template_path) {
      // Don't include the component itself
      case template_path == component_template_path {
        True -> False
        False -> {
          case simplifile.read(template_path) {
            Ok(content) -> string.contains(content, component_tag)
            Error(_) -> False
          }
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

  let component_data = build_component_data_map()
  let component_slots = build_component_slot_map_all()
  case
    compile_files(
      templates_to_compile,
      component_data,
      component_slots,
      verbose,
    )
  {
    Error(err) -> Error(err)
    Ok(_) -> {
      format_generated_files()
      case verbose {
        True -> Nil
        False ->
          io.println(
            console.success("Compiled ")
            <> int.to_string(list.length(templates_to_compile))
            <> " loom template(s)",
          )
      }
      Ok(Nil)
    }
  }
}

/// Extracts component name from a template path. Converts
/// path like src/resources/views/components/forms/input.loom.html
/// to component name "forms:input".
///
fn component_name_from_template_path(path: String) -> String {
  path
  |> string.replace(views_path <> "components/", "")
  |> string.replace(".loom.html", "")
  |> string.replace("/", ":")
}

/// Formats all generated files in the output directory.
/// Runs gleam format to ensure consistent code style in
/// the compiled template output.
///
fn format_generated_files() -> Nil {
  let _ = shellout.command("gleam", ["format", output_path], ".", [])
  Nil
}

/// Validates generated files by running gleam check. Reverts
/// Compiles a single template file to Gleam code. Validates
/// the template first, then runs lexer, parser, and generator.
/// Only writes output if validation passes.
///
fn compile_file(
  path: String,
  component_data: generator.ComponentDataMap,
  component_slots: generator.ComponentSlotMap,
  verbose: Bool,
) -> Result(Nil, String) {
  let is_component = string.contains(path, "components/")

  case simplifile.read(path) {
    Error(_) -> Error("Failed to read file: " <> path)
    Ok(content) -> {
      case lexer.tokenize(content) {
        Error(err) ->
          Error("Lexer error in " <> path <> ": " <> lexer_error_to_string(err))
        Ok(tokens) -> {
          case parser.parse(tokens) {
            Error(err) ->
              Error(
                "Parser error in "
                <> path
                <> ": "
                <> parser_error_to_string(err),
              )
            Ok(template) -> {
              // Validate template before generating
              case generator.validate_template(template, path) {
                Error(err) -> Error(err)
                Ok(_) -> {
                  let module_name = path_to_module_name(path)
                  let generated =
                    generator.generate(
                      template,
                      module_name,
                      is_component,
                      component_data,
                      component_slots,
                    )
                  let output_file = path_to_output_path(path)

                  let _ = filesystem.ensure_directory_exists(output_file)

                  case simplifile.write(output_file, generated.code) {
                    Ok(_) -> {
                      // Register live modules (non-components) in the registry
                      case template.is_live && !is_component {
                        True -> {
                          let _ = registry.register_module(module_name, path)
                          Nil
                        }
                        False -> Nil
                      }

                      case verbose {
                        True ->
                          console.output()
                          |> console.line(
                            "  "
                            <> path
                            <> " -> "
                            <> console.success(output_file),
                          )
                          |> console.print()
                        False -> Nil
                      }
                      Ok(Nil)
                    }
                    Error(_) -> Error("Failed to write: " <> output_file)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Finds all loom template files in a directory. Recursively
/// searches for files ending in .loom.html and returns their
/// full paths.
///
fn find_loom_files(dir: String) -> List(String) {
  case simplifile.get_files(dir) {
    Error(_) -> []
    Ok(files) -> list.filter(files, fn(f) { string.ends_with(f, ".loom.html") })
  }
}

/// Builds a map of component names to their props.
/// Parses all component templates to extract their @props
/// directives for prop generation.
///
fn build_component_data_map() -> generator.ComponentDataMap {
  // Find all component templates
  let component_files =
    find_loom_files(views_path)
    |> list.filter(fn(f) { string.contains(f, "components/") })

  // Parse each one and extract props
  component_files
  |> list.filter_map(fn(path) {
    case simplifile.read(path) {
      Error(_) -> Error(Nil)
      Ok(content) -> {
        case lexer.tokenize(content) {
          Error(_) -> Error(Nil)
          Ok(tokens) -> {
            case parser.parse(tokens) {
              Error(_) -> Error(Nil)
              Ok(template) -> {
                // Extract component name from template path
                let name = component_name_from_template_path(path)
                Ok(#(name, template.props))
              }
            }
          }
        }
      }
    }
  })
  |> dict.from_list
}

/// Builds a map of component names to their slot info. Parses
/// component templates to determine which slots each component
/// defines for proper slot argument generation.
///
fn build_component_slot_map(
  component_files: List(String),
) -> generator.ComponentSlotMap {
  component_files
  |> list.filter_map(fn(path) {
    case simplifile.read(path) {
      Error(_) -> Error(Nil)
      Ok(content) -> {
        case lexer.tokenize(content) {
          Error(_) -> Error(Nil)
          Ok(tokens) -> {
            case parser.parse(tokens) {
              Error(_) -> Error(Nil)
              Ok(template) -> {
                // Extract component name from path
                // e.g., src/resources/views/components/button.loom.html -> "button"
                let name = component_name_from_template_path(path)
                let slot_info = generator.extract_slot_info(template)
                Ok(#(name, slot_info))
              }
            }
          }
        }
      }
    }
  })
  |> dict.from_list
}

/// Builds component slot map for all components. Convenience
/// function that discovers all component files and builds
/// the complete slot map.
///
fn build_component_slot_map_all() -> generator.ComponentSlotMap {
  let component_files =
    find_loom_files(views_path)
    |> list.filter(fn(f) { string.contains(f, "components/") })
  build_component_slot_map(component_files)
}

/// Converts a template path to a module name. Returns the full
/// Gleam module path (e.g., "compiled/loom/loom_test") matching
/// where the compiled output will be placed.
///
fn path_to_module_name(path: String) -> String {
  let relative =
    path
    |> string.replace(views_path, "")
    |> string.replace(".loom.html", "")

  // Remove leading slash if present
  let relative = case string.starts_with(relative, "/") {
    True -> string.drop_start(relative, 1)
    False -> relative
  }

  "compiled/loom/" <> relative
}

/// Converts a template path to its output path. Maps from
/// the views directory to the compiled/loom directory
/// with .gleam extension.
///
fn path_to_output_path(path: String) -> String {
  let relative =
    path
    |> string.replace(views_path, "")
    |> string.replace(".loom.html", ".gleam")

  output_path <> relative
}

/// Converts a lexer error to a human-readable string. Formats
/// the error type and position information for display in
/// error messages.
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

/// Converts a parser error to a human-readable string. Formats
/// the error type and context information for display in
/// error messages.
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
