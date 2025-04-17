# Tego Language

Tego is a functional programming language designed for simplicity, pattern matching, and extensibility. It features a REPL, a parser, and an interpreter, allowing users to write, evaluate, and execute Tego programs interactively or from files.

## Features

- **Pattern Matching**: Supports matching over tuples, integers, booleans, characters, and strings.
- **Functional Programming**: Includes lambda expressions, function application, and higher-order functions.
- **Custom Types**: Allows defining and matching custom types and tuples.
- **Commands and IO**: Provides commands (IO monads) for interacting with the environment, including `println`, `readLine`, and `readInt`.
- **Error Handling**: Offers detailed and user-friendly parsing and runtime error messages.
- **Boxed Values**: Supports boxed values for grouping and manipulation.
- **REPL**: Interactive Read-Eval-Print Loop for testing and experimenting with Tego code.
- **File Execution**: Run `.tgo` files directly from the command line.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/theDragonFire/tego-lang.git
   cd tego-lang
   ```

2. Build the project using Cargo:
   ```bash
   cargo build --release
   ```

3. The compiled binary will be available in the `target/release` directory.

## Usage

### REPL

*WIP*

Start the interactive REPL:
```bash
cargo run -- repl
```

Example session:
```
Welcome to
   /\
  //\\
 //||\\  _   __   ___
   ||  ||_| / || // \\
   ||  ||_  \_|| \\_//
            \_||       

Type ':q' or ':quit' to exit

>> let a = 1
>> a + 2
3 : Int
```

### Running Files

Run a `.tgo` file:
```bash
cargo run -- run path/to/file.tgo
```

Example:
```bash
cargo run -- run tego/examples/addOne.tgo
```


## Language Syntax

### Declarations

```tego
val = 1
id a = a
const a _ = a
```

### Expressions

- **If Expressions**:
  ```tego
  if a > 1 then "greater" else "lesser"
  ```

- **Match Expressions**:
  ```tego
  match a to
  | 1 -> "one"
  | _ -> "other"
  ```

- **Lambda Functions**:
  ```tego
  fn a -> a + 1
  ```

### Commands

`do` expressions chain commands together for IO operations.

- `return a`: Wraps a value in a command.
- `println a`: Prints a value followed by a newline.
- `readLine`: Reads a line from standard input.
- `readInt`: Reads an integer from standard input. (WIP)

## Examples

### Hello World

```tego
main = "Hello, World!"
```

### Factorial

```tego
factorial n =
  match n to
  | 0 -> 1
  | _ -> n * factorial (n - 1)
```

### Fibonacci

```tego
fib n =
  match n to
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)
```

## Development

### Running Tests

Run the test suite:
```bash
cargo test
```

### Project Structure

- **`tego_parser`**: Handles parsing of Tego code into an abstract syntax tree (AST).
- **`tego_interpreter`**: Evaluates the AST and executes commands.
- **`tego`**: CLI for running the REPL or executing files.

### Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## License

This project is licensed under the GPL3 License. See the [LICENSE](LICENSE) file for details.