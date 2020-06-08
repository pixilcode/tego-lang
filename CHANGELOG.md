# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
* Differentiation between string and generic tuple storage
  * Strings are now stored as strings for as long as they only contain characters
  * Helps to conserve space and utilize the internal string representation
  * `Char` joined with `Char` results in a String
* `Char` and `String` value pattern matching
  * `'a'` matches the character `'a'`
  * `"string"` matches the string `"string"`
* Positive integers can be used to index a tuple
  * Tuples are 0-indexed
  * Trying to access past the end results in `()`
  * `0 (1, true, 'a') == 1`
  * `1 (1, true, 'a') == true`
  * `2 (1, true, 'a') == 'a'`
  * `3 (1, true, 'a') == ()`

### Fixed
* A non-tuple value joined with `()` now evaluates to that same value
  * Previously, the above action would produce a single-value tuple, which doesn't make sense
* An identifier starting with a keyword (ex. `matchStr`) isn't parsed as a keyword
  * Previously, the above identifier would be parsed as `match` `Str`

## [0.4.1] - 2020-06-03
### Added
* Support for comments
  * `--` starts a single line comment
  * `{-` and `-}` wrap a multi-line comment
* Improved parsing errors, so the user actually *understands* what went wrong

### Changed
* `:` option removed from if expressions
  * Only `if ... then ... else ...` and `if ... ? ... else ...` are allowed now

### Fixed
* A match expression declaration couldn't be followed by any code
  * ```
    foo a =
      match a to
      | () -> ()
      | h, t -> foo t
    
    bar b = ...
    ```
  * The above code produced a 'missing newline' error
  * Now, it works
  * Error came from having an optional newline after the match arm, which consumed all whitespace up to the next token; then, at the end of the declaration, a newline was expected, but all newlines had already been consumed

## [0.4.0] - 2020-05-19
### Added
* `Char` type
  * Syntax: `'c'`
* `String` type
  * Syntax: `"this is string"`
  * Represented as a tuple of chars
    * `"abc" : (Char, Char, Char)`
    * Therefore, can be concatenated using the `,` operator

### Changed
* Using a tuple match pattern works differently
  * Any unfilled spaces in tuples are filled with unit (`()`)
  * Matching `a, b, c` to `1` results in `a = 1`, `b = ()`, and `c = ()`
  * Matching `a, b, c` to `1, 2` results in `a = 1`, `b = 2`, and `c = ()`
  * Previously, the above two would produce errors
  * This feature allows for optional parameters and more (more on this later)

### Fixed
* Optional newline wasn't allowed after the equal sign in a declaration
* Subtraction error caused by trying to print out empty tuple/type of empty tuple
  * When printing a tuple or the type of a tuple, an extra `, ` was added on the end
  * This was solved by removing the last two characters of the string
  * However, when the tuple was empty, an empty string was returned
  * Therefore, the length of the string was 0 (usize)
  * Subtracting `2` from unsized `0` led to an overflow
  * Added guard around this subtraction

## [0.3.2] - 2020-05-15
### Added
* Ability to read and run files
  * Binary now has two commands: `run` and `repl`
  * `repl` opens up the REPL
  * `run <code-file>` runs a code file (extension is `.tgo`) 

## [0.3.1] - 2020-05-11
### Added
* Delay expressions
  * Delay the evaluation of an expression until later
  * `delay <ident> = <expr> in <expr>`
  * Only accepts identifiers, no other match expressions
  * Expression is evaluated when the variable is used

### Changed
* Declarations are not evaluated until needed
  * Any declarations referencing future declarations now work
* Checks made for keywords are now done in identifier parser
  * Identifier parser fails if the identifier is a keyword


## [0.3.0] - 2020-04-30
### Added
* Pattern-matching over values
  * Can now match integers and bools
  * `let 1 = 1 in ...`
  * `let true = true in ...`
* Pattern-match catch-all/ignore
  * `_` pattern discards the value
  * `let _ = 1 in ...`
  * Useful in match expressions
* Match expression
  * Syntax:
    ```
    match <expr> to
    | <match> -> <expr>
    | <match> -> <expr>
    | ...
    ```
* Expression Declarations
  * `<ident> <match>* = <expr>`
  * Defines `<ident>` before expression is evaluated
  * Currently, declarations are evaluated in order; that will change
  * If there are any match expressions, `<ident>` will be treated as a function
  * Declarations can be used in the REPL
  * (Soon to come, file read and evaluate)

### Changed
* Unit (`()`) is now (completely) a zero-element tuple instead of a first-class type
* While it was changed in some places so that it functioned like that, the vestigal parts of code were completely removed


## [0.2.1] - 2020-04-21
### Added
* Pattern-matching over tuples
  * `let a, b = 1, 2 in ...`
  * Parentheses in matching and/or expression are optional
    * `let (a, b) = 1, 2 in ...`
    * `let a, b = (1, 2) in ...`
    * `let (a, b) = (1, 2) in ...`
  * Note that grouping doesn't create tuples in tuples
     * `a, (b, c)` == `(a, b), c` == `a, b, c`
* Lambda function expressions
  * `fn <match> -> <expr>`
  * Lambda functions can only take 1 argument
* Function application
  * Arguments can now be applied to functions to get results
  * `(fn a -> a + 1) 1` == `2 : Int`
  * `(fn a, b -> a + b) (1 , 2)` == `3 : Int`
  * Function application has high precedence
    * `fn a -> a 1` is a function that takes a function as an argument and applies 1 as an argument; it can be read as `fn a -> (a 1)`
    * `(fn a, b -> a + b) 1, 2` is an error because the argument, which is only `1`, can't be unwrapped into a tuple; it can be read as `((fn a, b -> a + b) 1), 2`


## [0.2.0] - 2020-04-10
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
    

## [0.1.1] - 2020-04-06
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

[Unreleased]: https://github.com/theDragonFire/tego-lang/compare/v0.4.1...HEAD
[0.4.1]: https://github.com/theDragonFire/tego-lang/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/theDragonFire/tego-lang/compare/v0.3.2...v0.4.0
[0.3.2]: https://github.com/theDragonFire/tego-lang/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/theDragonFire/tego-lang/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/theDragonFire/tego-lang/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/theDragonFire/tego-lang/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/theDragonFire/tego-lang/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/theDragonFire/tego-lang/releases/v0.1.1