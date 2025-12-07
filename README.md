# Glimr ✨
 
A type-safe web framework for Gleam that brings functional programming elegance and developer productivity to web development.

## About Glimr

> **Note:** This repository contains the core code of the Glimr framework. If you want to build an application using Glimr, visit the main [Glimr repository](https://github.com/glimr-org/glimr).

## Features

- **Type-Safe Routing** - Express-style routing with compile-time safety and route parameter extraction
- **Route Grouping** - Group routes by middleware, path prefix, or name prefix
- **Middleware System** - Composable middleware at the route and group level
- **Middleware Groups** - Separate handling for web and API routes with appropriate error responses
- **Context System** - Type-safe dependency injection throughout your application
- **Form Validation** - Built-in validation rules for validating form requests
- **HTML and Lustre** - Return HTML or lustre views

## Installation

Add Glimr to your Gleam project:

```sh
gleam add glimr
```

## Getting Started

For a complete application structure with controllers, middleware, and configuration, check out the [Glimr](https://github.com/glimr-org/glimr) starter project.

## Learn More

- [Framework Repository](https://github.com/glimr-org/framework) - Core framework code
- [Gleam Documentation](https://gleam.run/documentation/) - Learn Gleam
- [Wisp Documentation](https://hexdocs.pm/wisp/) - Web server library

### Built With

Glimr is built on top of these excellent Gleam libraries:

- [**Wisp**](https://hexdocs.pm/wisp/) - The web framework that powers Glimr's HTTP handling
- [**gleam_http**](https://hexdocs.pm/gleam_http/) - HTTP types and utilities
- [**gleam_json**](https://hexdocs.pm/gleam_json/) - JSON encoding and decoding
- [**gleam_stdlib**](https://hexdocs.pm/gleam_stdlib/) - Gleam's standard library

Special thanks to the Gleam community for building such an awesome ecosystem!

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

The Glimr framework is open-sourced software licensed under the [MIT](https://opensource.org/license/MIT) license.

## Credits

Glimr is inspired by Laravel and other modern web frameworks, adapted for Gleam's functional programming paradigm.
