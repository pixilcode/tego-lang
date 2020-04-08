# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
* If expression
  * `if <expr> then <expr> else <expr>`
  * `if <expr> ? <expr> : <expr>`

## [0.1.1]
### Added
* Parentheses can now be used for grouping
* `-` and `not` unary operators now function
* `()` is treated as an empty tuple


## 0.1.0
### Added
* The following operators work
  * Integer arithmetic operators: `+`, `-`, `*`, `/`, `%`
  * Boolean arithmetic operators: `and`, `or`, `xor`
  * Comparison operators: `==`, `/=`, `<=`, `>=`, `<`, `>`
  * Tuple operator: `,`
* Types
  * Integer
  * Boolean
  * Unit
  * Tuple
* REPL evaluates expressions composed by the components listed above
* The type checker functions, though it is unintegrated and partially untested

[Unreleased]: https://github.com/theDragonFire/tego-lang/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/theDragonFire/tego-lang/releases/v0.1.1