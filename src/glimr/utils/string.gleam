//// String Utilities
////
//// Helper functions for string manipulation beyond what the
//// standard library provides. Includes pluralization and other
//// common string transformations.

import gleam/string

// ------------------------------------------------------------- Public Functions

/// Simple pluralization of English words. Handles common cases
/// like words ending in 's', 'x', 'z', 'ch', 'sh' (add 'es'),
/// consonant + 'y' (change to 'ies'), and default (add 's').
///
/// *Example:*
///
/// ```gleam
/// string.pluralize("user")     // => "users"
/// string.pluralize("category") // => "categories"
/// string.pluralize("box")      // => "boxes"
/// string.pluralize("match")    // => "matches"
/// ```
///
pub fn pluralize(word: String) -> String {
  let len = string.length(word)
  case len {
    0 -> word
    _ -> {
      let last = string.slice(word, len - 1, 1)
      let last_two = string.slice(word, len - 2, 2)

      case last, last_two {
        // Words ending in s, x, z, ch, sh -> add "es"
        "s", _ | "x", _ | "z", _ -> word <> "es"
        _, "ch" | _, "sh" -> word <> "es"
        // Words ending in consonant + y -> change y to ies
        "y", _ -> {
          let before_y = string.slice(word, len - 2, 1)
          case before_y {
            "a" | "e" | "i" | "o" | "u" -> word <> "s"
            _ -> string.slice(word, 0, len - 1) <> "ies"
          }
        }
        // Default: add "s"
        _, _ -> word <> "s"
      }
    }
  }
}

/// Checks if a character is alphanumeric or underscore. Returns
/// true for letters a-z, A-Z, digits 0-9, and underscore. Used
/// for validating identifier characters.
///
/// *Example:*
///
/// ```gleam
/// string.is_alphanumeric("A") // True
/// string.is_alphanumeric(":") // False
/// ```
///
pub fn is_alphanumeric(char: String) -> Bool {
  string.contains(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_",
    char,
  )
}

/// Checks if a single grapheme is an ASCII letter. Used
/// to distinguish HTML tag starts from operators like <
/// where only letters are valid tag name starters.
///
pub fn is_letter(char: String) -> Bool {
  string.contains("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", char)
}

/// Checks if a single grapheme is an ASCII whitespace
/// character (space, newline, tab, or carriage return).
/// Shared by the lexer and parser for consistent handling.
///
pub fn is_whitespace(char: String) -> Bool {
  char == " " || char == "\n" || char == "\t" || char == "\r"
}
