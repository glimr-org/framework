# Glimr ✨
 
A batteries-included web framework for Gleam that brings functional programming elegance and developer productivity to web development.

If you'd like to stay updated on Glimr's development, Follow [@migueljarias](https://x.com/migueljarias) on X (that's me) for updates.

## About Glimr

> **Note:** This repository contains the core code of the Glimr framework. If you want to build an application using Glimr, visit the main [Glimr repository](https://github.com/glimr-org/glimr).

## Features

- **Type Safe Routing** - Pattern matching routes with compile-time type safety
- **Loom Template Engine** - Blade-inspired templates with components, slots, and conditionals
- **Middleware System** - Composable middleware at route and group levels
- **Middleware Groups** - Pre-configured middleware stacks for different route types (Web, API, Custom)
- **Form Validation** - Elegant form validation layer to easily validate requests
- **Automatic Migrations** - Schema-based migration generation with snapshot diffing
- **SQL Queries** - Write raw SQL files with full editor LSP support, compiled to typed Gleam functions
- **Connection Pooling** - Efficient database connection management for PostgreSQL and SQLite
- **Transaction Support** - Atomic operations with automatic retry on deadlock
- **Caching** - Unified caching API with file, SQLite, and PostgreSQL backends
- **Sessions** - Server-side sessions with flash messages, backed by PostgreSQL, SQLite, Redis, file, or cookie drivers
- **Console Commands** - CLI task runner with database access support and argument parsing

## Installation

Add Glimr to your Gleam project:

```sh
gleam add glimr
```

## Getting Started

For a complete application structure with controllers, middleware, database support, etc. check out the main [Glimr](https://github.com/glimr-org/glimr) repository.

## Learn More

- [Glimr Repository](https://github.com/glimr-org/glimr) - Main Glimr repository
- [Gleam Documentation](https://gleam.run/documentation/) - Learn Gleam
- [Wisp Documentation](https://hexdocs.pm/wisp/) - Web server library

### Built With

Glimr is built on top of these excellent Gleam packages:

- [**wisp**](https://hexdocs.pm/wisp/) - The web framework that powers Glimr's HTTP handling
- [**gleam_http**](https://hexdocs.pm/gleam_http/) - HTTP types and utilities
- [**gleam_json**](https://hexdocs.pm/gleam_json/) - JSON encoding and decoding
- [**gleam_stdlib**](https://hexdocs.pm/gleam_stdlib/) - Gleam's standard library
- [**gleam_time**](https://github.com/gleam-lang/time) - Work with time in Gleam!
- [**simplifile**](https://github.com/bcpeinhardt/simplifile) - Simple file operations for Gleam
- [**dot_env**](https://github.com/aosasona/dotenv) - Load environment variables from .env

Special thanks to the Gleam community for building such an awesome ecosystem!

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

The Glimr framework is open-sourced software licensed under the [MIT](https://opensource.org/license/MIT) license.

## Credits

Glimr is inspired by Laravel and other modern web frameworks, adapted for Gleam's functional programming paradigm.
