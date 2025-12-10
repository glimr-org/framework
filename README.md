# Glimr ✨
 
A type-safe web framework for Gleam that brings functional programming elegance and developer productivity to web development.

If you'd like to stay updated on Glimr's development, Follow [@migueljarias](https://x.com/migueljarias) on X (that's me) for updates, behind-the-scenes stuff and overall nonsense.

## About Glimr

> **Note:** This repository contains the core code of the Glimr framework. If you want to build an application using Glimr, visit the main [Glimr repository](https://github.com/glimr-org/glimr).

## Features

- **Type Safe Routing** - Pattern matching routes with compile-time type safety and automatic 404/405 handling
- **View Builder** - Fluent API for rendering HTML and Lustre components with layouts
- **Template Engine** - Simple `{{variable}}` syntax for dynamic content
- **Redirect Builder** - Clean redirect API with flash message support
- **Middleware System** - Composable middleware at route and group levels
- **Middleware Groups** - Pre-configured middleware stacks for different route types (Web, API, Custom)
- **Lustre Integration** - Server-side rendering of Lustre components
- **Context/Singleton System** - Type-safe use of singletons throughout your application
- **Form Validation** - Built-in validation rules for validating form requests

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
