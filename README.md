# kicad_lib

[![wakatime](https://wakatime.com/badge/github/adom-inc/kicad_lib.svg)](https://wakatime.com/badge/github/adom-inc/kicad_lib)

> [!WARNING]
> This library is still in active development. All APIs and data structures are subject to change.

A Rust library for working with [KiCad](https://www.kicad.org/) files. This library handles the parsing and serialization of the KiCad file formats while also providing helpful functionality for generating and manipulating many of the internal structures used in the formats.

## Structure

-   [`kicad_sexpr`](/kicad_sexpr) - A crate for parsing the KiCad S-Expression format into a friendly tree representation. If you want to parse out the tree structure agnostic to the actual KiCad data format, then this is the crate to use.
-   [`kicad_format`](/kicad_format) - A crate for parsing the sexpr trees into a Rust interpretation of the structures used in the file formats. If you are only trying to manipulate the underlying data and not the S-Expression tree directly, then this is the crate to use.

## Usage

Since this library is not hosted on [crates.io](https://crates.io), to use it simply add the following to your `Cargo.toml` file:

```toml
kicad_sexpr = { git = "https://github.com/adom-inc/kicad_lib" }
```

or

```toml
kicad_format = { git = "https://github.com/adom-inc/kicad_lib" }
```

## Documentation

The documentation for this library is still work in progress, but you can bring up the documentation for the crate you are using with:

```console
$ cargo doc --package kicad_sexpr --open
```

or

```console
$ cargo doc --package kicad_format --open
```
