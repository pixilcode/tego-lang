# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
* Pattern-matching over tuples
  * `let a, b = 1, 2 in ...`
  * Parentheses in matching and/or expression are optional
    * `let (a, b) = 1, 2 in ...`
    * `let a, b = (1, 2) in ...`
    * `let (a, b) = (1, 2) in ...`
  * Note that grouping doesn't create tuples in tuples
     * `a, (b, c)` == `(a, b), c` == `a, b, c`

## [0.2.0]
### Added
* If expression
  * `if <expr> then <expr> else <expr>`
  * `if <expr> ? <expr> : <expr>`
* Let expression
  * `let <match> = <expr> in <expr>`
* Variables in interpreter
* Match expressions
  * Currently only implemented for a simple identifier
  * Identifiers are composed only of the alphabet (a-z, A-Z)
  * Plan is to add other destructuring identifiers

### Removed
* Type checker
  * Type checker was getting complicated to maintain
  * Also, it wasn't being used in the actual project
  * In the future, the goal is to make type *inference*
* New line allowance before tokens
  * New lines will be used to signify the end of an expression
  * Therefore, can't allow code such as
    ```
    3
    + 2
    ```
    

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
    * Note that the tuple operator does not create tuples in tuples
    * `a, (b, c)` == `(a, b), c` == `a, b, c`
* Types
  * Integer
  * Boolean
  * Unit
  * Tuple
* REPL evaluates expressions composed by the components listed above
* The type checker functions, though it is unintegrated and partially untested

[Unreleased]: https://github.com/theDragonFire/tego-lang/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/theDragonFire/tego-lang/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/theDragonFire/tego-lang/releases/v0.1.1