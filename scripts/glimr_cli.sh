#!/usr/bin/env bash

# Glimr CLI entry point
#
# This script is sourced by the user's ./glimr wrapper. All command 
# routing logic lives here so users get updates automatically

GLIMR_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$GLIMR_SCRIPT_DIR/compile_routes.sh"

echo ""

case "${1:-}" in
  build|run|serve)
    compile_routes_if_needed
    ;;
  route:compile)
    shift
    compile_routes "$@"
    echo ""
    exit 0
    ;;
esac

gleam run --no-print-progress -m glimr_console -- "$@"

echo ""
