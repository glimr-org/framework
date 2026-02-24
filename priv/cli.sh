#!/usr/bin/env bash

# Glimr CLI - Registry-based command routing
# This script is sourced by the app's ./glimr script

REGISTRY_PATH="priv/storage/framework/console/commands.json"

glimr_run() {
    # No arguments - show command list
    if [ $# -eq 0 ]; then
        echo ""
        gleam run --no-print-progress -m glimr/internal/console/commands/command_list
        echo ""
        return 0
    fi

    # Version flag
    if [ "$1" = "-V" ] || [ "$1" = "--version" ]; then
        echo ""
        gleam run --no-print-progress -m glimr/internal/console/commands/version
        echo ""
        return 0
    fi

    # Get command name and remaining args
    local CMD="$1"
    shift

    # Look up module in registry
    local MODULE
    MODULE=$(glimr_lookup_module "$CMD")

    if [ -z "$MODULE" ]; then
        echo ""
        echo -e "\033[31mCommand not found: $CMD\033[0m" >&2
        echo -e "\033[31mRun ./glimr to see available commands.\033[0m" >&2
        echo ""
        return 1
    fi

    echo ""
    gleam run --no-print-progress -m "$MODULE" -- "_c_name=$CMD" "$@"
    echo ""
}

# Look up command module from registry JSON
glimr_lookup_module() {
    local CMD="$1"

    if [ ! -f "$REGISTRY_PATH" ]; then
        return 1
    fi

    # Extract module for the given command name from JSON
    # Format: "command_name": {"description": "...", "module": "path/to/module"}
    grep -o "\"${CMD}\"[[:space:]]*:[[:space:]]*{[^}]*}" "$REGISTRY_PATH" \
        | grep -o '"module"[[:space:]]*:[[:space:]]*"[^"]*"' \
        | sed 's/.*"module"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/'
}
