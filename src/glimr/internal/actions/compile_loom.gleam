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
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glimr/console/console
import glimr/filesystem/filesystem
import glimr/loom/generator
import glimr/loom/gleam_parser
import glimr/loom/lexer
import glimr/loom/parser
import shellout
import simplifile

const views_path = "src/resources/views/"

const app_loom_path = "src/app/loom/"

const output_path = "src/compiled/loom/"

/// Tracks a compiled file with its source path, output path,
/// and previous content. Used for reverting generated files
/// if validation fails after compilation.
///
pub type CompiledFile {
  CompiledFile(source: String, output: String, previous: String)
}

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
          |> console.line_warning("Compiling loom templates...")
          |> console.print()
        False -> Nil
      }

      // Build component data map first
      let component_data = build_component_data_map()

      // Compile component files (they may depend on other components)
      let component_files =
        list.filter(all_files, fn(f) { string.contains(f, "components/") })
      let page_files =
        list.filter(all_files, fn(f) { !string.contains(f, "components/") })

      // Build component slot map (component name -> slot info)
      let component_slots = build_component_slot_map(component_files)

      let component_compiled =
        list.filter_map(component_files, fn(file) {
          compile_file(file, component_data, component_slots, verbose)
        })

      // Compile page files last (they may depend on components)
      let page_compiled =
        list.filter_map(page_files, fn(file) {
          compile_file(file, component_data, component_slots, verbose)
        })

      let all_compiled = list.flatten([component_compiled, page_compiled])

      format_generated_files()

      // Run gleam check once after all files are written
      validate_generated_files(all_compiled)

      case verbose {
        True -> Nil
        False -> io.println(console.success("Compiled loom templates"))
      }

      Ok(Nil)
    }
  }
}

/// Compiles a single loom file by path. Handles both template
/// files (.loom.html) and view files (.gleam), routing to the
/// appropriate compilation strategy.
///
pub fn run_path(path: String, verbose: Bool) -> Result(Nil, String) {
  case string.ends_with(path, ".loom.html") {
    True -> {
      compile_loom_path(path, verbose)
      Ok(Nil)
    }
    False ->
      case string.ends_with(path, ".gleam") {
        True -> {
          compile_view_file_path(path, verbose)
          Ok(Nil)
        }
        False ->
          Error(
            "Not a loom file or view file: path must end with '.loom.html' or '.gleam'",
          )
      }
  }
}

/// Compiles a loom template path. Checks if the file exists
/// and either compiles it or cleans up the generated file
/// if the source was deleted.
///
fn compile_loom_path(path: String, verbose: Bool) -> Nil {
  // Check if file was deleted - if so, clean up generated file
  case simplifile.is_file(path) {
    Ok(True) -> compile_existing_loom_path(path, verbose)
    _ -> cleanup_deleted_loom_file(path, verbose)
  }
}

/// Compiles an existing loom template. Routes layout changes
/// to full recompilation, component changes to dependency
/// recompilation, and regular templates to single compilation.
///
fn compile_existing_loom_path(path: String, verbose: Bool) -> Nil {
  case string.contains(path, "layouts/") {
    // Layout changes affect all templates
    True -> {
      let _ = run(verbose)
      Nil
    }
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
            Ok(compiled) -> {
              format_generated_files()
              validate_generated_files([compiled])
              case verbose {
                True -> Nil
                False -> io.println(console.success("Compiled 1 loom template"))
              }
            }
            Error(_) -> Nil
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
) -> Nil {
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
  let compiled =
    list.filter_map(templates_to_compile, fn(template_path) {
      compile_file(template_path, component_data, component_slots, verbose)
    })

  format_generated_files()
  validate_generated_files(compiled)

  case verbose {
    True -> Nil
    False ->
      io.println(
        console.success("Compiled ")
        <> int.to_string(list.length(compiled))
        <> " loom template(s)",
      )
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

/// Handles compilation triggered by app/loom file changes.
/// Routes layout changes to full recompilation, component
/// changes to dependency recompilation.
///
fn compile_view_file_path(path: String, verbose: Bool) -> Nil {
  // Check if this is an app/loom file
  case string.contains(path, app_loom_path) {
    False -> {
      io.println(console.error("Not an app/loom file: " <> path))
    }
    True -> {
      case string.contains(path, "layouts/") {
        // Layout changes affect all templates
        True -> {
          let _ = run(verbose)
          Nil
        }
        False -> {
          case string.contains(path, app_loom_path <> "components/") {
            // Component Data changed - recompile component + all templates using it
            True -> compile_component_and_dependents(path, verbose)
            // Regular view file - just recompile its template
            False -> compile_single_view_file(path, verbose)
          }
        }
      }
    }
  }
}

/// Compiles a component and all templates that depend on it.
/// Triggered when a component's Data type changes, requiring
/// all consumers to be recompiled.
///
fn compile_component_and_dependents(
  component_view_path: String,
  verbose: Bool,
) -> Nil {
  // Extract component name (e.g., "button" or "forms.input")
  let component_name = component_name_from_view_path(component_view_path)
  let component_tag = "<x-" <> component_name

  // Get the component's own template path
  let component_template = view_file_to_template_path(component_view_path)

  // Find all templates that use this component
  let all_templates = find_loom_files(views_path)
  let dependent_templates =
    all_templates
    |> list.filter(fn(template_path) {
      // Don't include the component itself - we'll add it separately
      case template_path == component_template {
        True -> False
        False -> {
          case simplifile.read(template_path) {
            Ok(content) -> string.contains(content, component_tag)
            Error(_) -> False
          }
        }
      }
    })

  // Build list of templates to compile: component first, then dependents
  let templates_to_compile = case simplifile.is_file(component_template) {
    Ok(True) -> [component_template, ..dependent_templates]
    _ -> dependent_templates
  }

  case templates_to_compile {
    [] -> {
      io.println(console.warning(
        "No templates found for component: " <> component_name,
      ))
    }
    _ -> {
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
      let compiled =
        list.filter_map(templates_to_compile, fn(template_path) {
          compile_file(template_path, component_data, component_slots, verbose)
        })

      format_generated_files()
      validate_generated_files(compiled)

      case verbose {
        True -> Nil
        False ->
          io.println(
            console.success("Compiled ")
            <> int.to_string(list.length(compiled))
            <> " loom template(s)",
          )
      }
    }
  }
}

/// Compiles a single view file's corresponding template.
/// Finds the matching .loom.html file and compiles it with
/// the updated view data.
///
fn compile_single_view_file(path: String, verbose: Bool) -> Nil {
  let loom_path = view_file_to_template_path(path)

  case simplifile.is_file(loom_path) {
    Ok(True) -> {
      case verbose {
        True ->
          console.output()
          |> console.line_warning(
            "Compiling loom views (triggered by " <> path <> ")...",
          )
          |> console.print()
        False -> Nil
      }

      let component_data = build_component_data_map()
      let component_slots = build_component_slot_map_all()
      case compile_file(loom_path, component_data, component_slots, verbose) {
        Ok(compiled) -> {
          format_generated_files()
          validate_generated_files([compiled])
          case verbose {
            True -> Nil
            False -> io.println(console.success("Compiled 1 loom template"))
          }
        }
        Error(_) -> Nil
      }
    }
    _ -> {
      io.println(console.warning(
        "No corresponding template found for "
        <> path
        <> " (expected: "
        <> loom_path
        <> ")",
      ))
    }
  }
}

/// Extracts component name from a view file path. Converts
/// path like src/app/loom/components/forms/input.gleam to
/// component name "forms:input".
///
fn component_name_from_view_path(path: String) -> String {
  path
  |> string.replace(app_loom_path <> "components/", "")
  |> string.replace(".gleam", "")
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
/// any files that cause compilation errors and reports the
/// failure to the user.
///
fn validate_generated_files(compiled_files: List(CompiledFile)) -> Nil {
  case shellout.command("gleam", ["check"], ".", []) {
    Ok(_) -> Nil
    Error(#(_, msg)) -> {
      // Find which compiled file has the error and revert it
      list.each(compiled_files, fn(file) {
        case string.contains(msg, file.output) {
          True -> {
            // Revert the file
            let _ = simplifile.write(file.output, file.previous)
            io.println(console.error(
              "Generated code failed to compile for "
              <> file.source
              <> "\n"
              <> extract_error_message(msg, file.output),
            ))
          }
          False -> Nil
        }
      })
    }
  }
}

/// Compiles a single template file to Gleam code. Runs the
/// lexer, parser, and generator, then writes the output and
/// returns a CompiledFile for validation tracking.
///
fn compile_file(
  path: String,
  component_data: generator.ComponentDataMap,
  component_slots: generator.ComponentSlotMap,
  verbose: Bool,
) -> Result(CompiledFile, Nil) {
  let is_component = string.contains(path, "components/")

  case simplifile.read(path) {
    Error(_) -> {
      io.println(console.error("Failed to read file: " <> path))
      Error(Nil)
    }
    Ok(content) -> {
      case lexer.tokenize(content) {
        Error(err) -> {
          io.println(console.error(
            "Lexer error in " <> path <> ": " <> lexer_error_to_string(err),
          ))
          Error(Nil)
        }
        Ok(tokens) -> {
          case parser.parse(tokens) {
            Error(err) -> {
              io.println(console.error(
                "Parser error in "
                <> path
                <> ": "
                <> parser_error_to_string(err),
              ))
              Error(Nil)
            }
            Ok(template) -> {
              let module_name = path_to_module_name(path)
              // Try to parse the corresponding app/loom file for Data type
              let view_file_path = path_to_view_file(path)
              let view_file = case
                gleam_parser.parse_view_file(view_file_path)
              {
                Ok(parsed) -> Some(parsed)
                Error(_) -> None
              }
              let generated =
                generator.generate(
                  template,
                  module_name,
                  is_component,
                  view_file,
                  component_data,
                  component_slots,
                )
              let output_file = path_to_output_path(path)

              let _ = filesystem.ensure_directory_exists(output_file)

              // Read previous content for potential revert
              let previous_content =
                simplifile.read(output_file) |> result.unwrap("")

              case simplifile.write(output_file, generated.code) {
                Ok(_) -> {
                  case verbose {
                    True ->
                      console.output()
                      |> console.line(
                        "  " <> path <> " -> " <> console.success(output_file),
                      )
                      |> console.print()
                    False -> Nil
                  }
                  Ok(CompiledFile(path, output_file, previous_content))
                }
                Error(_) -> {
                  io.println(console.error("Failed to write: " <> output_file))
                  Error(Nil)
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

/// Builds a map of component names to their Data fields.
/// Parses all component view files to extract their type
/// information for prop generation.
///
fn build_component_data_map() -> generator.ComponentDataMap {
  // Find all component view files in app/loom/components/
  let component_view_files =
    find_component_view_files(app_loom_path <> "components/")

  // Parse each one and build the map
  component_view_files
  |> list.filter_map(fn(path) {
    case gleam_parser.parse_view_file(path) {
      Error(_) -> Error(Nil)
      Ok(parsed) -> {
        // Extract component name from path
        // e.g., src/app/loom/components/button.gleam -> "button"
        let name = component_name_from_path(path)
        Ok(#(name, parsed.fields))
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

/// Finds all component view files in a directory. Searches
/// for .gleam files that define component Data types and
/// returns their full paths.
///
fn find_component_view_files(dir: String) -> List(String) {
  case simplifile.get_files(dir) {
    Error(_) -> []
    Ok(files) -> list.filter(files, fn(f) { string.ends_with(f, ".gleam") })
  }
}

/// Extracts the component name from a view file path. Strips
/// the base directory and extension, converting slashes to
/// colons for namespaced component names.
///
fn component_name_from_path(path: String) -> String {
  path
  |> string.replace(app_loom_path <> "components/", "")
  |> string.replace(".gleam", "")
  |> string.replace("/", ":")
}

/// Converts a template path to a module name. Strips the
/// base directory and extension, replacing slashes with
/// underscores for valid Gleam module names.
///
fn path_to_module_name(path: String) -> String {
  path
  |> string.replace(views_path, "")
  |> string.replace(".loom.html", "")
  |> string.replace("/", "_")
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

/// Converts a template path to its view file path. Maps from
/// views directory to app/loom directory, e.g.,
/// src/resources/views/home.loom.html -> src/app/loom/home.gleam
///
fn path_to_view_file(path: String) -> String {
  let relative =
    path
    |> string.replace(views_path, "")
    |> string.replace(".loom.html", ".gleam")

  app_loom_path <> relative
}

/// Converts a view file path to its template path. Maps from
/// app/loom directory to views directory, e.g.,
/// src/app/loom/home.gleam -> src/resources/views/home.loom.html
///
fn view_file_to_template_path(path: String) -> String {
  let relative =
    path
    |> string.replace(app_loom_path, "")
    |> string.replace(".gleam", ".loom.html")

  views_path <> relative
}

/// Converts a lexer error to a human-readable string. Formats
/// the error type and position information for display in
/// error messages.
///
fn lexer_error_to_string(err: lexer.LexerError) -> String {
  case err {
    lexer.UnterminatedVariable(pos) ->
      "Unterminated variable at position " <> int.to_string(pos)
    lexer.InvalidVariableName(name, pos) ->
      "Invalid variable name '"
      <> name
      <> "' at position "
      <> int.to_string(pos)
      <> " (variable names cannot contain spaces)"
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
  }
}

/// Extracts the relevant error message from gleam check output.
/// Finds the section related to the specific file and returns
/// a condensed error message for display.
///
fn extract_error_message(msg: String, file_path: String) -> String {
  // Try to find the error section related to our file
  case string.split_once(msg, file_path) {
    Ok(#(_, after_path)) -> {
      // Take the first meaningful part of the error
      let lines = string.split(after_path, "\n")
      lines
      |> list.take(10)
      |> list.filter(fn(line) { string.trim(line) != "" })
      |> string.join("\n")
    }
    Error(_) -> msg
  }
}
