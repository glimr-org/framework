//// Session Payload
////
//// Both the file store and cookie store need to serialize
//// session data and flash into a string and parse it back.
//// Centralizing the JSON format here guarantees every store
//// produces and consumes the same wire shape, so switching
//// backends never causes decoding failures from mismatched
//// payload formats.
////

import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list

// ------------------------------------------------------------- Public Functions

/// Data and flash are namespaced under "_data" and "_flash"
/// keys rather than merged flat so neither can collide with the 
/// other — a session key called "_flash" won't shadow the 
/// actual flash dict. JSON was chosen over ETF or other formats 
/// because the cookie store sends it to browsers where binary 
/// Erlang terms wouldn't be readable.
///
pub fn encode(data: Dict(String, String), flash: Dict(String, String)) -> String {
  let to_json_object = fn(d) {
    dict.to_list(d)
    |> list.map(fn(entry) { #(entry.0, json.string(entry.1)) })
    |> json.object
  }

  json.object([
    #("_data", to_json_object(data)),
    #("_flash", to_json_object(flash)),
  ])
  |> json.to_string
}

/// Falling back to empty dicts on any parse failure means a
/// corrupt or truncated payload degrades to a fresh session
/// rather than crashing the request. Using optional_field for
/// both "_data" and "_flash" handles partial payloads — old
/// sessions written before flash support was added will still
/// load correctly with an empty flash dict.
///
pub fn decode(
  payload_json: String,
) -> #(Dict(String, String), Dict(String, String)) {
  let string_dict = decode.dict(decode.string, decode.string)

  let decoder = {
    use data <- decode.optional_field("_data", dict.new(), string_dict)
    use flash <- decode.optional_field("_flash", dict.new(), string_dict)
    decode.success(#(data, flash))
  }

  case json.parse(payload_json, decoder) {
    Ok(result) -> result
    Error(_) -> #(dict.new(), dict.new())
  }
}
