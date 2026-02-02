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
    echo -e "\033[0;31mError: No compiler binary found for your platform\033[0m"
    echo -e "\033[0;31mExpected: $GLIMR_COMPILER\033[0m"
    echo ""
    echo -e "\033[0;33mSupported platforms:\033[0m linux-x64, linux-arm64, darwin-x64, darwin-arm64, windows-x64"
    echo ""
    exit 1
  fi
}

compile_routes_cmd() {
  require_compiler
  "$GLIMR_COMPILER" routes "$@"
}

# Track if any compilation occurred
GLIMR_COMPILED=false

compile_routes_if_needed_cmd() {
  require_compiler
  # Use --check-only: exit 0 means compile needed
  if "$GLIMR_COMPILER" routes --check-only 2>/dev/null; then
    "$GLIMR_COMPILER" routes
    GLIMR_COMPILED=true
  fi
}

compile_loom_cmd() {
  require_compiler
  "$GLIMR_COMPILER" loom "$@"
}

compile_loom_if_needed_cmd() {
  require_compiler
  # Use --check-only: exit 0 means compile needed
  # Use --stale to compile only changed files
  if "$GLIMR_COMPILER" loom --check-only >/dev/null 2>&1; then
    "$GLIMR_COMPILER" loom --stale
    GLIMR_COMPILED=true
  fi
}

echo ""

case "${1:-}" in
  build|run|serve)
    compile_routes_if_needed_cmd
    compile_loom_if_needed_cmd
    if [[ "$GLIMR_COMPILED" == true ]]; then
      echo ""
    fi
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
  loom:compile)
    shift
    # Check for --help to pass to Gleam
    if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
      gleam run --no-print-progress -m glimr_console -- loom:compile --help
    else
      compile_loom_cmd "$@"
    fi
    echo ""
    exit 0
    ;;
esac

gleam run --no-print-progress -m glimr_console -- "$@"

echo ""
