# TODO

* Priorities
  1. Redo tuple, error value types
  2. IO Monad
  3. Type Inference
  4. Parsing Errors
     * Parsing functions return custom error
     * Parsing functions take a custom input that tracks column and line number
  5. Sum types (`|` operator)
     * See [Or types](http://journal.stuffwithstuff.com/2010/08/23/void-null-maybe-and-nothing/)
  6. Custom types
     * Named tuples (`Item a`, `Point (Int, Int)`, `Color (Int, Int, Int)`, etc.)
     * Named enums
     * `type` declaration
       * `type Identity a = Identity a` (Basic polymorphic type)
       * `type Point = Point (Int, Int)` (Product type)
       * `type Option a = Some a | None` (Polymorphic sum type)
     * Right side is type, left side is type constructor
     * Type/type constructor can only have one argument
     * Named types can be matched and unwrapped in match patterns using type constructor
  7. Integers access fields in a tuple
     * They return the item in the 0-indexed position in the tuple
     * `1 (true, 2, false)` => `2`
     * Accessing past the end returns `()` (like `Some(a)`/ `None`)
     * `4 (true, 2, false)` => `()`
* Build compiler (maybe use craftinginterpreters.com?)
* Add named tuples
* Add `,,` operator, boxed tuples
* Add `.` composition/application(?) operator
  * `id 1` == `1.id`
  * `fn x -> id (id x)` ~= `id . id`
* Add documentation to all functions/enums/structs
* Values become Rc's?
  * Once made, they should never change ∴ can have multiple owned references
  * Maybe Cows instead
  * Just stop cloning the value when it's not necessary!!!
* REPL can expect multiple lines
* Match functions
  * `fn[ a, true -> a + 1 | a, false -> a ]`
  * Creates a function that has a match expression built in
* Check for name clashes (decls and variables)
* Maybe find a way to use [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index) in the environment instead...
* Also, maybe see if the language (or a new one) can be improved with [this](http://willcrichton.net/notes/type-level-programming/)
* Add regex expressions and regex matches using regex crate (eg. `\a+b*\`)
* Open file in repl
* Add strings to pattern matching
* Code standard library
* Comments
* If-guarded match statements
* Add primitive types to matching
* Fix recursion problem with [this](https://www.reddit.com/r/ProgrammingLanguages/comments/gkx10d/recursion_without_stack_overflow/) - tried this (see 'trampolining' branch), but was unable to
* Add file tracking to `Span` struct (useful for error handling)
* Add more detailed error messages (see [this](https://elm-lang.org/news/compiler-errors-for-humans) and [this](https://blog.rust-lang.org/2016/08/10/Shape-of-errors-to-come.html))
* Check literal error parsing for keywords (ex. 'if', 'match')
