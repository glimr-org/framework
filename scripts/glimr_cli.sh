#!/usr/bin/env bash

# Glimr CLI entry point
#
# This script is sourced by the user's ./glimr wrapper. All command
# routing logic lives here so users get updates automatically

GLIMR_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Detect platform and find the compiler binary
detect_compiler() {
  local os arch binary

  case "$(uname -s)" in
    Linux*)  os="linux" ;;
    Darwin*) os="darwin" ;;
    MINGW*|MSYS*|CYGWIN*) os="windows" ;;
    *) os="unknown" ;;
  esac

  case "$(uname -m)" in
    x86_64|amd64) arch="x64" ;;
    arm64|aarch64) arch="arm64" ;;
    *) arch="unknown" ;;
  esac

  if [[ "$os" == "windows" ]]; then
    binary="$GLIMR_SCRIPT_DIR/bin/glimr-compiler-${os}-${arch}.exe"
  else
    binary="$GLIMR_SCRIPT_DIR/bin/glimr-compiler-${os}-${arch}"
  fi

  echo "$binary"
}

GLIMR_COMPILER=$(detect_compiler)

# Check compiler exists
require_compiler() {
  if [[ ! -x "$GLIMR_COMPILER" ]]; then
    echo "Error: No compiler binary found for your platform"
    echo "Expected: $GLIMR_COMPILER"
    echo ""
    echo "Supported platforms: linux-x64, linux-arm64, darwin-x64, darwin-arm64, windows-x64"
    exit 1
  fi
}

compile_routes_cmd() {
  require_compiler
  "$GLIMR_COMPILER" routes "$@"
}

compile_routes_if_needed_cmd() {
  require_compiler
  # Use --check-only: exit 0 means compile needed
  if "$GLIMR_COMPILER" routes --check-only 2>/dev/null; then
    "$GLIMR_COMPILER" routes
  fi
}

echo ""

case "${1:-}" in
  build|run|serve)
    compile_routes_if_needed_cmd
    ;;
  route:compile)
    shift
    # Check for --help to pass to Gleam
    if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
      gleam run --no-print-progress -m glimr_console -- route:compile --help
    else
      compile_routes_cmd "$@"
    fi
    echo ""
    exit 0
    ;;
esac

gleam run --no-print-progress -m glimr_console -- "$@"

echo ""
