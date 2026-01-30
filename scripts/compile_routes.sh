#!/usr/bin/env bash

# Glimr Route Compiler
#
# Compiles controllers with annotation comments for routing and 
# other functionality into a pattern-matched router in gleam.

set -uo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Verbose mode (set by compile_routes function based on args)
VERBOSE=false

# ============================================================================
# Configuration
# ============================================================================

CONTROLLER_DIR="src/app/http/controllers"
COMPILED_DIR="src/compiled/routes"
CONFIG_ROUTE_FILE="src/config/config_route.gleam"
GLIMR_TOML="glimr.toml"
MIDDLEWARE_BASE="app/http/middleware"
VALIDATOR_BASE="app/http/validators"

# ============================================================================
# Config Parsing
# ============================================================================

# Check if auto_compile is enabled in glimr.toml
read_auto_compile() {
  if [[ -f "$GLIMR_TOML" ]]; then
    if awk '/^\[routes\]/,/^\[/' "$GLIMR_TOML" 2>/dev/null | grep -q 'auto_compile.*=.*false'; then
      echo "false"
      return
    fi
  fi
  echo "true"
}

# Parse route groups from config_route.gleam
# Output format: name:prefix (one per line)
read_route_groups() {
  if [[ ! -f "$CONFIG_ROUTE_FILE" ]]; then
    echo "web:"
    return
  fi

  # Extract RouteGroupConfig blocks and parse name/prefix
  local in_config=false
  local current_name=""
  local current_prefix=""

  while IFS= read -r line; do
    if [[ "$line" =~ RouteGroupConfig\( ]]; then
      in_config=true
      current_name=""
      current_prefix=""
    fi

    if [[ "$in_config" == true ]]; then
      if [[ "$line" =~ name:[[:space:]]*\"([^\"]+)\" ]]; then
        current_name="${BASH_REMATCH[1]}"
      fi
      if [[ "$line" =~ prefix:[[:space:]]*\"([^\"]*)\" ]]; then
        current_prefix="${BASH_REMATCH[1]}"
      fi
      if [[ "$line" =~ \) ]]; then
        if [[ -n "$current_name" ]]; then
          echo "${current_name}:${current_prefix}"
        fi
        in_config=false
      fi
    fi
  done < "$CONFIG_ROUTE_FILE"
}

# ============================================================================
# Staleness Detection
# ============================================================================

routes_are_stale() {
  # No compiled directory = stale
  [[ ! -d "$COMPILED_DIR" ]] && return 0

  # No compiled files = stale
  local compiled_files
  compiled_files=$(find "$COMPILED_DIR" -name "*.gleam" 2>/dev/null | head -1)
  [[ -z "$compiled_files" ]] && return 0

  # No controllers = not stale (nothing to compile)
  [[ ! -d "$CONTROLLER_DIR" ]] && return 1

  # Find newest controller modification time
  local newest_controller
  newest_controller=$(find "$CONTROLLER_DIR" -name "*.gleam" -printf '%T@\n' 2>/dev/null | sort -rn | head -1)
  [[ -z "$newest_controller" ]] && return 1

  # Find oldest compiled route modification time
  local oldest_compiled
  oldest_compiled=$(find "$COMPILED_DIR" -name "*.gleam" -printf '%T@\n' 2>/dev/null | sort -n | head -1)
  [[ -z "$oldest_compiled" ]] && return 0

  # Stale if any controller is newer than oldest compiled
  [[ "${newest_controller%.*}" -gt "${oldest_compiled%.*}" ]]
}

# ============================================================================
# Utility Functions
# ============================================================================

error() {
  echo -e "${RED}error:${NC} $1" >&2
  exit 1
}

warn() {
  echo -e "${YELLOW}warning:${NC} $1" >&2
}

success() {
  echo -e "${GREEN}$1${NC}"
}

# Capitalize first letter
capitalize() {
  echo "${1^}"
}

# Get controller alias from module path
# app/http/controllers/api/home_controller -> api_home_controller
controller_alias() {
  local path="$1"
  if [[ "$path" =~ controllers/(.+)$ ]]; then
    echo "${BASH_REMATCH[1]}" | tr '/' '_'
  else
    basename "$path"
  fi
}

# Get module path from file path
# src/app/http/controllers/home_controller.gleam -> app/http/controllers/home_controller
module_from_path() {
  local path="$1"
  path="${path#src/}"
  path="${path%.gleam}"
  echo "$path"
}

# Extract path parameters from route path
# /users/:id/posts/:post_id -> id post_id
extract_path_params() {
  local path="$1"
  echo "$path" | grep -oE ':[a-zA-Z_][a-zA-Z0-9_]*' | sed 's/^://' | tr '\n' ' ' | sed 's/ $//'
}

# Convert path to pattern match
# /users/:id -> ["users", id]
path_to_pattern() {
  local path="$1"
  path="${path#/}"  # Remove leading slash
  path="${path%/}"  # Remove trailing slash

  if [[ -z "$path" ]]; then
    echo "[]"
    return
  fi

  local segments=()
  IFS='/' read -ra parts <<< "$path"
  for part in "${parts[@]}"; do
    if [[ "$part" =~ ^: ]]; then
      segments+=("${part#:}")
    else
      segments+=("\"$part\"")
    fi
  done

  local result="["
  result+=$(IFS=', '; echo "${segments[*]}")
  result+="]"
  echo "$result"
}

# Check if path has parameters
path_has_params() {
  [[ "$1" =~ : ]]
}

# ============================================================================
# Validation
# ============================================================================

# Validate path format
validate_path_format() {
  local path="$1"
  local handler="$2"

  # Path must match: ^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))+$|^/$
  if [[ "$path" == "/" ]]; then
    return 0
  fi

  if ! [[ "$path" =~ ^(/([a-zA-Z0-9_-]+|:[a-zA-Z_][a-zA-Z0-9_]*))+$ ]]; then
    error "Invalid route '$path' for $handler
Path must contain only slashes, letters, numbers, hyphens, underscores, or :params"
  fi
}

# Validate reserved path params
validate_reserved_params() {
  local path="$1"
  local reserved=("req" "_req" "ctx" "_ctx")

  for param in $(extract_path_params "$path"); do
    for r in "${reserved[@]}"; do
      if [[ "$param" == "$r" ]]; then
        error "Reserved path parameter name in route '$path': $param
These names are reserved: req, _req, ctx, _ctx"
      fi
    done
  done
}

# Check if middleware exists and has run function
check_middleware() {
  local mw="$1"
  local src_path="src/${MIDDLEWARE_BASE}/${mw}.gleam"
  local test_path="test/fixtures/${MIDDLEWARE_BASE}/${mw}.gleam"

  local file_path=""
  if [[ -f "$src_path" ]]; then
    file_path="$src_path"
  elif [[ -f "$test_path" ]]; then
    file_path="$test_path"
  else
    echo "not_found"
    return
  fi

  if grep -q 'pub fn run(' "$file_path"; then
    echo "ok"
  else
    echo "missing_run"
  fi
}

# Check if validator exists and has validate function
check_validator() {
  local v="$1"
  local src_path="src/${VALIDATOR_BASE}/${v}.gleam"
  local test_path="test/fixtures/${VALIDATOR_BASE}/${v}.gleam"

  local file_path=""
  if [[ -f "$src_path" ]]; then
    file_path="$src_path"
  elif [[ -f "$test_path" ]]; then
    file_path="$test_path"
  else
    echo "not_found"
    return
  fi

  if grep -q 'pub fn validate(' "$file_path"; then
    echo "ok"
  else
    echo "missing_validate"
  fi
}

# ============================================================================
# Controller Parsing
# ============================================================================

# Parse a controller file and output route data
# Output format (one route per line):
# TYPE|METHOD|PATH|HANDLER|MIDDLEWARE|VALIDATOR|PARAMS
# Where TYPE is "route" or "redirect"
parse_controller() {
  local file="$1"
  local module
  module=$(module_from_path "$file")
  local alias
  alias=$(controller_alias "$module")

  local group_middleware=()
  local in_doc_comment=false
  local current_method=""
  local current_path=""
  local current_middleware=()
  local current_validator=""
  local current_redirects=()
  local collecting_signature=false
  local signature_buffer=""

  while IFS= read -r line || [[ -n "$line" ]]; do
    local trimmed
    trimmed=$(echo "$line" | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')

    # Check for group middleware (file-level, before any functions)
    if [[ "$trimmed" =~ ^//[[:space:]]*@group_middleware[[:space:]]+\"([^\"]+)\" ]]; then
      group_middleware+=("${BASH_REMATCH[1]}")
      continue
    fi

    # Check for doc comment annotations
    if [[ "$trimmed" =~ ^/// ]]; then
      in_doc_comment=true
      local after_slashes
      after_slashes=$(echo "$trimmed" | sed 's|^///[[:space:]]*||')

      # HTTP method annotations
      for method in get post put patch delete head options; do
        if [[ "$after_slashes" =~ ^@${method}[[:space:]]+\"([^\"]+)\" ]]; then
          current_method="$method"
          current_path="${BASH_REMATCH[1]}"
        fi
      done

      # Middleware annotation
      if [[ "$after_slashes" =~ ^@middleware[[:space:]]+\"([^\"]+)\" ]]; then
        current_middleware+=("${BASH_REMATCH[1]}")
      fi

      # Validator annotation
      if [[ "$after_slashes" =~ ^@validator[[:space:]]+\"([^\"]+)\" ]]; then
        current_validator="${BASH_REMATCH[1]}"
      fi

      # Redirect annotations
      if [[ "$after_slashes" =~ ^@redirect[[:space:]]+\"([^\"]+)\" ]]; then
        current_redirects+=("${BASH_REMATCH[1]}:303")
      fi
      if [[ "$after_slashes" =~ ^@redirect_permanent[[:space:]]+\"([^\"]+)\" ]]; then
        current_redirects+=("${BASH_REMATCH[1]}:308")
      fi

      continue
    fi

    # Check for pub fn declaration
    if [[ "$trimmed" =~ ^pub[[:space:]]+fn[[:space:]]+ ]]; then
      if [[ -n "$current_method" && -n "$current_path" ]]; then
        collecting_signature=true
        signature_buffer="$trimmed"

        # Check if signature is complete (contains opening brace)
        if [[ "$signature_buffer" =~ \{ ]]; then
          collecting_signature=false

          # Extract function name and params
          local fn_name
          fn_name=$(echo "$signature_buffer" | sed 's/^pub[[:space:]]*fn[[:space:]]*//' | sed 's/(.*$//')

          local params_str
          params_str=$(extract_params_from_signature "$signature_buffer")

          # Combine group middleware with route middleware
          local all_middleware=("${group_middleware[@]}" "${current_middleware[@]}")
          local mw_str
          mw_str=$(IFS=','; echo "${all_middleware[*]}")

          # Output the route
          echo "route|$current_method|$current_path|${alias}.${fn_name}|$mw_str|$current_validator|$params_str"

          # Output any redirects
          for redir in "${current_redirects[@]}"; do
            local redir_path="${redir%:*}"
            local redir_status="${redir#*:}"
            echo "redirect|$redir_status|$redir_path|$current_path|||"
          done

          # Reset state
          current_method=""
          current_path=""
          current_middleware=()
          current_validator=""
          current_redirects=()
        fi
      fi
      in_doc_comment=false
      continue
    fi

    # Continue collecting multi-line signature
    if [[ "$collecting_signature" == true ]]; then
      signature_buffer+=" $trimmed"
      if [[ "$signature_buffer" =~ \{ ]]; then
        collecting_signature=false

        # Extract function name and params
        local fn_name
        fn_name=$(echo "$signature_buffer" | sed 's/^pub[[:space:]]*fn[[:space:]]*//' | sed 's/(.*$//')

        local params_str
        params_str=$(extract_params_from_signature "$signature_buffer")

        # Combine group middleware with route middleware
        local all_middleware=("${group_middleware[@]}" "${current_middleware[@]}")
        local mw_str
        mw_str=$(IFS=','; echo "${all_middleware[*]}")

        # Output the route
        echo "route|$current_method|$current_path|${alias}.${fn_name}|$mw_str|$current_validator|$params_str"

        # Output any redirects
        for redir in "${current_redirects[@]}"; do
          local redir_path="${redir%:*}"
          local redir_status="${redir#*:}"
          echo "redirect|$redir_status|$redir_path|$current_path|||"
        done

        # Reset state
        current_method=""
        current_path=""
        current_middleware=()
        current_validator=""
        current_redirects=()
      fi
      continue
    fi

    # Non-doc comment line, reset if not empty or regular comment
    if [[ -n "$trimmed" && ! "$trimmed" =~ ^// ]]; then
      in_doc_comment=false
      current_method=""
      current_path=""
      current_middleware=()
      current_validator=""
      current_redirects=()
    fi
  done < "$file"
}

# Extract parameters from function signature
# Input: pub fn show(req: Request, ctx: Context, id: String) -> Response {
# Output: req:Request,ctx:Context,id:String
extract_params_from_signature() {
  local sig="$1"

  # Extract content between first ( and matching )
  local params_raw
  params_raw=$(echo "$sig" | sed 's/.*(\(.*\)).*/\1/' | sed 's/{.*//')

  # Handle nested parentheses by tracking depth
  local result=""
  local current=""
  local depth=0
  local i

  for ((i=0; i<${#params_raw}; i++)); do
    local char="${params_raw:$i:1}"
    case "$char" in
      '(')
        ((depth++))
        current+="$char"
        ;;
      ')')
        ((depth--))
        current+="$char"
        ;;
      ',')
        if [[ $depth -eq 0 ]]; then
          # End of parameter
          local param
          param=$(echo "$current" | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')
          if [[ -n "$param" ]]; then
            # Parse name:type
            local name type
            if [[ "$param" =~ : ]]; then
              name=$(echo "$param" | cut -d: -f1 | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')
              type=$(echo "$param" | cut -d: -f2- | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')
            else
              name="$param"
              type=""
            fi
            [[ -n "$result" ]] && result+=","
            result+="${name}:${type}"
          fi
          current=""
        else
          current+="$char"
        fi
        ;;
      *)
        current+="$char"
        ;;
    esac
  done

  # Handle last parameter
  local param
  param=$(echo "$current" | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')
  if [[ -n "$param" ]]; then
    local name type
    if [[ "$param" =~ : ]]; then
      name=$(echo "$param" | cut -d: -f1 | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')
      type=$(echo "$param" | cut -d: -f2- | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')
    else
      name="$param"
      type=""
    fi
    [[ -n "$result" ]] && result+=","
    result+="${name}:${type}"
  fi

  echo "$result"
}

# ============================================================================
# Route Validation
# ============================================================================

# Validate all routes from parsed data
validate_routes() {
  local routes_data="$1"

  while IFS='|' read -r type method path handler middleware validator params; do
    [[ -z "$type" ]] && continue

    if [[ "$type" == "route" ]]; then
      # Validate path format
      validate_path_format "$path" "$handler"

      # Validate reserved params
      validate_reserved_params "$path"

      # Validate middleware
      if [[ -n "$middleware" ]]; then
        IFS=',' read -ra mw_list <<< "$middleware"
        for mw in "${mw_list[@]}"; do
          [[ -z "$mw" ]] && continue
          local status
          status=$(check_middleware "$mw")
          case "$status" in
            not_found)
              error "Invalid route '$path' for $handler
Middleware \"$mw\" doesn't exist"
              ;;
            missing_run)
              error "Invalid route '$path' for $handler
Middleware \"$mw\" doesn't have a public \"run\" function"
              ;;
          esac
        done
      fi

      # Validate validator
      if [[ -n "$validator" ]]; then
        local status
        status=$(check_validator "$validator")
        case "$status" in
          not_found)
            error "Invalid route '$path' for $handler
Validator \"$validator\" doesn't exist"
            ;;
          missing_validate)
            error "Invalid route '$path' for $handler
Validator \"$validator\" doesn't have a public \"validate\" function"
            ;;
        esac
      fi

      # Validate handler params
      validate_handler_params "$path" "$handler" "$params" "$validator"
    fi
  done <<< "$routes_data"
}

# Validate handler parameters
validate_handler_params() {
  local path="$1"
  local handler="$2"
  local params="$3"
  local validator="$4"

  local route_params
  route_params=$(extract_path_params "$path")

  # Parse function params into arrays
  local -a fn_param_names=()
  local -a fn_param_types=()

  if [[ -n "$params" ]]; then
    IFS=',' read -ra param_pairs <<< "$params"
    for pair in "${param_pairs[@]}"; do
      local name="${pair%%:*}"
      local type="${pair#*:}"
      [[ "$name" == "$pair" ]] && type=""
      fn_param_names+=("$name")
      fn_param_types+=("$type")
    done
  fi

  # Check for untyped req/ctx
  for i in "${!fn_param_names[@]}"; do
    local name="${fn_param_names[$i]}"
    local type="${fn_param_types[$i]}"
    local lower_name
    lower_name=$(echo "$name" | tr '[:upper:]' '[:lower:]')

    if [[ "$lower_name" == "req" || "$lower_name" == "_req" || "$lower_name" == "ctx" || "$lower_name" == "_ctx" ]]; then
      if [[ -z "$type" ]]; then
        local expected_type
        if [[ "$lower_name" == "req" || "$lower_name" == "_req" ]]; then
          expected_type="Request"
        else
          expected_type="Context"
        fi
        error "Handler $handler has parameter '$name' without a type annotation
Please specify the type: $name: $expected_type"
      fi
    fi
  done

  # Get handler route params (exclude Request, Context, Data)
  local -a handler_route_params=()
  for i in "${!fn_param_names[@]}"; do
    local name="${fn_param_names[$i]}"
    local type="${fn_param_types[$i]}"

    # Skip Request params
    [[ "$type" == "Request" || "$type" == "wisp.Request" ]] && continue
    # Skip Context params
    [[ "$type" == "Context" || "$type" == "ctx.Context" ]] && continue
    # Skip Data params (validator)
    if [[ -n "$validator" ]]; then
      local validator_name
      validator_name=$(basename "$validator")
      [[ "$type" == "Data" || "$type" == "${validator_name}.Data" ]] && continue
    fi

    # Strip leading underscore
    local clean_name="$name"
    [[ "$name" =~ ^_ ]] && clean_name="${name#_}"
    handler_route_params+=("$clean_name")
  done

  # Check for missing route params
  for rp in $route_params; do
    local found=false
    for hp in "${handler_route_params[@]}"; do
      if [[ "$hp" == "$rp" ]]; then
        found=true
        break
      fi
    done
    if [[ "$found" == false ]]; then
      error "Route '$path' defines :$rp but handler $handler has no matching parameter"
    fi
  done

  # Check for extra handler params
  for i in "${!fn_param_names[@]}"; do
    local name="${fn_param_names[$i]}"
    local type="${fn_param_types[$i]}"

    # Skip special params
    [[ "$type" == "Request" || "$type" == "wisp.Request" ]] && continue
    [[ "$type" == "Context" || "$type" == "ctx.Context" ]] && continue
    if [[ -n "$validator" ]]; then
      local validator_name
      validator_name=$(basename "$validator")
      [[ "$type" == "Data" || "$type" == "${validator_name}.Data" ]] && continue
    fi

    local clean_name="$name"
    [[ "$name" =~ ^_ ]] && clean_name="${name#_}"

    local found=false
    for rp in $route_params; do
      if [[ "$rp" == "$clean_name" ]]; then
        found=true
        break
      fi
    done

    if [[ "$found" == false ]]; then
      local hint=""
      local lower_name
      lower_name=$(echo "$clean_name" | tr '[:upper:]' '[:lower:]')
      if [[ "$lower_name" == "validated" || "$lower_name" == "data" ]] && [[ -n "$validator" ]]; then
        local validator_name
        validator_name=$(basename "$validator")
        hint="
If this is meant to be validated data, specify the type: $name: ${validator_name}.Data"
      fi
      error "Handler $handler has parameter '$clean_name' but route '$path' has no matching segment$hint"
    fi
  done

  # Check for missing validator Data param
  if [[ -n "$validator" ]]; then
    local has_data=false
    local validator_name
    validator_name=$(basename "$validator")
    for type in "${fn_param_types[@]}"; do
      if [[ "$type" == "Data" || "$type" == "${validator_name}.Data" ]]; then
        has_data=true
        break
      fi
    done
    if [[ "$has_data" == false ]]; then
      error "Route '$path' uses @validator \"$validator\" but handler $handler has no Data parameter"
    fi
  fi
}

# ============================================================================
# Code Generation
# ============================================================================

# Generate imports for a route group
generate_imports() {
  local routes_data="$1"
  local -A controllers=()
  local -A middleware=()
  local -A validators=()

  while IFS='|' read -r type method path handler mw_list validator params; do
    [[ -z "$type" || "$type" == "redirect" ]] && continue

    # Extract controller from handler (alias.fn_name)
    local controller="${handler%.*}"
    controllers["$controller"]=1

    # Collect middleware
    if [[ -n "$mw_list" ]]; then
      IFS=',' read -ra mw_arr <<< "$mw_list"
      for mw in "${mw_arr[@]}"; do
        [[ -n "$mw" ]] && middleware["$mw"]=1
      done
    fi

    # Collect validators
    if [[ -n "$validator" ]]; then
      validators["$validator"]=1
    fi
  done <<< "$routes_data"

  # Output controller imports
  for ctrl in "${!controllers[@]}"; do
    # Find the full module path for this alias
    local module_path
    module_path=$(find_module_for_alias "$ctrl")
    local default_name
    default_name=$(basename "$module_path")

    if [[ "$ctrl" == "$default_name" ]]; then
      echo "import $module_path"
    else
      echo "import $module_path as $ctrl"
    fi
  done

  # Output middleware imports
  for mw in "${!middleware[@]}"; do
    echo "import ${MIDDLEWARE_BASE}/$mw"
  done

  # Output validator imports
  for v in "${!validators[@]}"; do
    echo "import ${VALIDATOR_BASE}/$v"
  done
}

# Find module path for a controller alias
# This is a reverse lookup - we need to track the original paths
# For now, we'll search for the controller file
find_module_for_alias() {
  local alias="$1"
  # Search for the controller file that matches this alias
  local file
  file=$(find src/app/http/controllers -name "*.gleam" 2>/dev/null | while read -r f; do
    local mod_path
    mod_path=$(module_from_path "$f")
    local ctrl_alias
    ctrl_alias=$(controller_alias "$mod_path")
    if [[ "$ctrl_alias" == "$alias" ]]; then
      echo "$mod_path"
      break
    fi
  done | head -1)

  if [[ -n "$file" ]]; then
    echo "$file"
  else
    # Fallback: convert alias back to path
    echo "app/http/controllers/$(echo "$alias" | tr '_' '/')"
  fi
}

# Collect used HTTP methods
collect_methods() {
  local routes_data="$1"
  local -A methods=()

  while IFS='|' read -r type method path handler mw validator params; do
    [[ "$type" == "route" ]] && methods["$(capitalize "$method")"]=1
  done <<< "$routes_data"

  echo "${!methods[@]}"
}

# Check if any route uses middleware
uses_middleware() {
  local routes_data="$1"
  while IFS='|' read -r type method path handler mw validator params; do
    [[ "$type" == "route" && -n "$mw" ]] && return 0
  done <<< "$routes_data"
  return 1
}

# Check if any handler uses Request param
uses_request() {
  local routes_data="$1"
  while IFS='|' read -r type method path handler mw validator params; do
    [[ "$type" != "route" ]] && continue
    if [[ "$params" =~ :Request || "$params" =~ :wisp\.Request ]]; then
      return 0
    fi
  done <<< "$routes_data"
  return 1
}

# Check if any handler uses Context param
uses_context() {
  local routes_data="$1"
  while IFS='|' read -r type method path handler mw validator params; do
    [[ "$type" != "route" ]] && continue
    if [[ "$params" =~ :Context || "$params" =~ :ctx\.Context ]]; then
      return 0
    fi
  done <<< "$routes_data"
  return 1
}

# Generate handler call with correct parameter ordering
generate_handler_call() {
  local handler="$1"
  local path="$2"
  local params="$3"
  local middleware="$4"
  local validator="$5"

  local route_params
  route_params=$(extract_path_params "$path")

  # Build args based on function signature order
  local -a args=()
  if [[ -n "$params" ]]; then
    IFS=',' read -ra param_pairs <<< "$params"
    for pair in "${param_pairs[@]}"; do
      local name="${pair%%:*}"
      local type="${pair#*:}"
      [[ "$name" == "$pair" ]] && type=""

      # Strip underscore from name for matching
      local clean_name="$name"
      [[ "$name" =~ ^_ ]] && clean_name="${name#_}"

      # Determine what to pass
      if [[ "$type" == "Request" || "$type" == "wisp.Request" ]]; then
        args+=("req")
      elif [[ "$type" == "Context" || "$type" == "ctx.Context" ]]; then
        args+=("ctx")
      elif [[ -n "$validator" ]]; then
        local validator_name
        validator_name=$(basename "$validator")
        if [[ "$type" == "Data" || "$type" == "${validator_name}.Data" ]]; then
          args+=("validated")
        else
          args+=("$clean_name")
        fi
      else
        args+=("$clean_name")
      fi
    done
  fi

  local args_str
  args_str=$(IFS=', '; echo "${args[*]}")
  local call="${handler}(${args_str})"

  # Wrap with validator if present
  if [[ -n "$validator" ]]; then
    local validator_name
    validator_name=$(basename "$validator")
    call="{
          use validated <- ${validator_name}.validate(req, ctx)
          $call
        }"
  fi

  # Wrap with middleware if present
  if [[ -n "$middleware" ]]; then
    local mw_calls=()
    IFS=',' read -ra mw_arr <<< "$middleware"
    for mw in "${mw_arr[@]}"; do
      [[ -n "$mw" ]] && mw_calls+=("$(basename "$mw").run")
    done
    local mw_list
    mw_list=$(IFS=', '; echo "${mw_calls[*]}")
    call="{
          use req, ctx <- middleware.apply([$mw_list], req, ctx)
          $call
        }"
  fi

  echo "$call"
}

# Generate code for a single path with its routes
generate_path_case() {
  local path="$1"
  shift
  local routes=("$@")

  local pattern
  pattern=$(path_to_pattern "$path")

  # Check if this is a redirect
  local first_route="${routes[0]}"
  IFS='|' read -r type method rpath handler mw validator params <<< "$first_route"

  if [[ "$type" == "redirect" ]]; then
    # method field contains status for redirects
    local status="$method"
    local redirect_fn="wisp.redirect"
    [[ "$status" == "308" ]] && redirect_fn="wisp.permanent_redirect"
    echo "    $pattern ->
      $redirect_fn(\"$handler\")"
    return
  fi

  # Generate method cases
  local method_cases=""
  local -a methods_used=()

  for route in "${routes[@]}"; do
    IFS='|' read -r type method rpath handler mw validator params <<< "$route"
    [[ "$type" != "route" ]] && continue

    local method_upper
    method_upper=$(capitalize "$method")
    methods_used+=("$method_upper")

    local handler_call
    handler_call=$(generate_handler_call "$handler" "$rpath" "$params" "$mw" "$validator")

    method_cases+="        $method_upper -> $handler_call
"
  done

  local methods_list
  methods_list=$(IFS=', '; echo "${methods_used[*]}")

  echo "    $pattern ->
      case method {
$method_cases        _ -> wisp.method_not_allowed([$methods_list])
      }"
}

# Generate the full routes code
generate_routes_code() {
  local routes_data="$1"

  # Group routes by path
  declare -A path_routes
  local -a paths_ordered=()

  while IFS='|' read -r type method path handler mw validator params; do
    [[ -z "$type" ]] && continue

    # For redirects, path is in the 'path' field (third field)
    local route_path="$path"

    if [[ -z "${path_routes[$route_path]+isset}" ]]; then
      paths_ordered+=("$route_path")
    fi
    path_routes["$route_path"]+="$type|$method|$path|$handler|$mw|$validator|$params"$'\n'
  done <<< "$routes_data"

  # Sort paths: static before parameterized, then alphabetically
  IFS=$'\n' sorted_paths=($(for p in "${paths_ordered[@]}"; do
    if path_has_params "$p"; then
      echo "1:$p"
    else
      echo "0:$p"
    fi
  done | sort | cut -d: -f2-))
  unset IFS

  # Generate cases
  local cases=""
  for path in "${sorted_paths[@]}"; do
    local routes_for_path="${path_routes[$path]}"
    local -a route_arr=()
    while IFS= read -r line; do
      [[ -n "$line" ]] && route_arr+=("$line")
    done <<< "$routes_for_path"

    local case_code
    case_code=$(generate_path_case "$path" "${route_arr[@]}")
    cases+="$case_code

"
  done

  echo "  case path {
$cases
    _ -> wisp.not_found()
  }"
}

# ============================================================================
# Main Compilation
# ============================================================================

# Compile routes for a specific group
compile_group() {
  local group_name="$1"
  local group_prefix="$2"
  local all_routes="$3"
  local all_prefixes="$4"

  # Filter routes for this group
  local group_routes=""
  while IFS='|' read -r type method path handler mw validator params; do
    [[ -z "$type" ]] && continue

    local route_path="$path"

    # Find the best matching prefix for this route
    local best_prefix=""
    local best_len=0
    while IFS=':' read -r name prefix; do
      [[ -z "$name" ]] && continue
      # Check if route starts with this prefix
      if [[ -z "$prefix" ]] || [[ "$route_path" == "$prefix"* ]]; then
        local prefix_len=${#prefix}
        if [[ $prefix_len -gt $best_len ]]; then
          best_prefix="$prefix"
          best_len=$prefix_len
        fi
      fi
    done <<< "$all_prefixes"

    # Only include if this group's prefix is the best match
    if [[ "$best_prefix" == "$group_prefix" ]]; then
      group_routes+="$type|$method|$path|$handler|$mw|$validator|$params"$'\n'
    fi
  done <<< "$all_routes"

  [[ -z "$group_routes" ]] && return 0

  # Validate routes
  validate_routes "$group_routes"

  # Generate imports
  local imports
  imports=$(generate_imports "$group_routes")

  # Check what features are used
  local methods
  methods=$(collect_methods "$group_routes")

  local has_middleware=false
  uses_middleware "$group_routes" && has_middleware=true

  local has_req=false
  uses_request "$group_routes" && has_req=true

  local has_ctx=false
  uses_context "$group_routes" && has_ctx=true

  # Generate the routes code
  local routes_code
  routes_code=$(generate_routes_code "$group_routes")

  # Build the file
  local output=""

  # Header comment
  output+="//// This file was generated by Glimr
//// https://github.com/glimr-org/glimr?tab=readme-ov-file#routes
////
//// DO NOT EDIT THIS FILE. If you would like to use plain pattern
//// matching over this compiled route approach, take a look at
//// the docs link below detailing how to do so.
////
//// See: https://github.com/glimr-org/glimr?tab=readme-ov-file#direct-pattern-matching
////

"

  # Imports
  output+="$imports"

  # HTTP methods import
  if [[ -n "$methods" ]]; then
    output+=$'\n'"import gleam/http.{$methods}"
  fi

  # Middleware import
  if [[ "$has_middleware" == true ]]; then
    output+=$'\n'"import glimr/http/middleware"
  fi

  # Wisp import
  output+=$'\n'"import wisp"

  # Function signature
  local req_arg="req"
  local ctx_arg="ctx"
  local method_arg="method"

  [[ "$has_req" == false ]] && req_arg="_req"
  [[ "$has_ctx" == false ]] && ctx_arg="_ctx"
  [[ -z "$methods" ]] && method_arg="_method"

  output+=$'\n\n'"pub fn routes(path, $method_arg, $req_arg, $ctx_arg) {"
  output+=$'\n'"$routes_code"
  output+=$'\n'"}"
  output+=$'\n'

  # Write file
  local dest="${COMPILED_DIR}/${group_name}.gleam"
  mkdir -p "$COMPILED_DIR"
  echo "$output" > "$dest"

  # Count routes for output
  local count=0
  while IFS='|' read -r type _ _ _ _ _ _; do
    [[ "$type" == "route" ]] && ((count++))
  done <<< "$group_routes"

  if [[ "$VERBOSE" == true ]]; then
    echo "  $group_name.gleam -> $(success "$count routes")" >&2
  fi

  # Return count for total (to stdout for capture)
  echo "$count"
}

# Main compile function
# Usage: compile_routes [-v|--verbose]
compile_routes() {
  # Parse arguments
  VERBOSE=false
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -v|--verbose)
        VERBOSE=true
        shift
        ;;
      -h|--help)
        echo "route:compile - Compile controller routes to optimized pattern matching"
        echo ""
        echo "Usage: ./glimr route:compile [options]"
        echo ""
        echo "Options:"
        echo "  -v, --verbose  Display detailed information about compiled routes"
        echo "  -h, --help     Show this help message"
        return 0
        ;;
      *)
        shift
        ;;
    esac
  done

  if [[ "$VERBOSE" == true ]]; then
    echo "Compiling routes..." >&2
  fi

  # Ensure output directory exists
  mkdir -p "$COMPILED_DIR"

  # Find all controller files
  if [[ ! -d "$CONTROLLER_DIR" ]]; then
    warn "No controllers found in $CONTROLLER_DIR"
    return 0
  fi

  local controller_files
  controller_files=$(find "$CONTROLLER_DIR" -name "*.gleam" 2>/dev/null)

  if [[ -z "$controller_files" ]]; then
    warn "No controller files found"
    return 0
  fi

  # Parse all controllers
  local all_routes=""
  while IFS= read -r file; do
    local routes
    routes=$(parse_controller "$file")
    [[ -n "$routes" ]] && all_routes+="$routes"$'\n'
  done <<< "$controller_files"

  if [[ -z "$all_routes" ]]; then
    warn "No routes found in controllers"
    return 0
  fi

  # Read route groups
  local groups
  groups=$(read_route_groups)

  # Compile each group and count total routes
  local total_routes=0
  while IFS=':' read -r name prefix; do
    local count
    count=$(compile_group "$name" "$prefix" "$all_routes" "$groups")
    ((total_routes += count)) || true
  done <<< "$groups"

  if [[ "$VERBOSE" == true ]]; then
    echo "$(success "Routes compiled successfully")" >&2
  else
    echo "$(success "Compiled $total_routes routes")"
  fi
}

# Compile routes only if needed
compile_routes_if_needed() {
  local auto_compile
  auto_compile=$(read_auto_compile)

  if [[ "$auto_compile" != "true" ]]; then
    return 0
  fi

  if routes_are_stale; then
    compile_routes
  fi
}
