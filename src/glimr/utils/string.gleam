//// ------------------------------------------------------------
//// String Utilities
//// ------------------------------------------------------------
////
//// Helper functions for string manipulation beyond what the
//// standard library provides. Includes pluralization and other
//// common string transformations.
////

import gleam/string

// ------------------------------------------------------------- Public Functions

/// ------------------------------------------------------------
/// Pluralize
/// ------------------------------------------------------------
///
/// Simple pluralization of English words. Handles common cases
/// like words ending in 's', 'x', 'z', 'ch', 'sh' (add 'es'),
/// consonant + 'y' (change to 'ies'), and default (add 's').
///
/// ------------------------------------------------------------
///
/// *Example:*
///
/// ```gleam
/// pluralize("user")     // => "users"
/// pluralize("category") // => "categories"
/// pluralize("box")      // => "boxes"
/// pluralize("match")    // => "matches"
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
