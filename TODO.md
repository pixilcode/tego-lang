# TODO

* Priorities
  1. Type Inference
  2. Sum types (`|` operator)
     * See [Or types](http://journal.stuffwithstuff.com/2010/08/23/void-null-maybe-and-nothing/)
  3. Custom types
     * Named tuples (`Item a`, `Point (Int, Int)`, `Color (Int, Int, Int)`, etc.)
     * Named enums
     * `type` declaration
       * `type Identity a = Identity a` (Basic polymorphic type)
       * `type Point = Point (Int, Int)` (Product type)
       * `type Option a = Some a | None` (Polymorphic sum type)
     * Right side is type, left side is type constructor
     * Type/type constructor can only have one argument
     * Named types can be matched and unwrapped in match patterns using type constructor
  4. Add `.` composition/application(?) operator
     * `id 1` == `1.id`
     * `fn x -> id (id x)` ~= `id . id`
  5. Match functions
     * `fn[ a, true -> a + 1 | a, false -> a ]`
     * Creates a function that has a match expression built in
* Build compiler (maybe use craftinginterpreters.com?)
* Add documentation to all functions/enums/structs
* Values become Rc's?
  * Once made, they should never change ∴ can have multiple owned references
  * Maybe Cows instead
  * Just stop cloning the value when it's not necessary!!!
* REPL can expect multiple lines
* Check for name clashes (decls and variables)
* Maybe find a way to use [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index) in the environment instead...
* Also, maybe see if the language (or a new one) can be improved with [this](http://willcrichton.net/notes/type-level-programming/)
* Add regex expressions and regex matches using regex crate (eg. `\a+b*\`)
* Open file in repl
* Code standard library
* Comments
* If-guarded match statements
* Add primitive types to matching
* Fix recursion problem with [this](https://www.reddit.com/r/ProgrammingLanguages/comments/gkx10d/recursion_without_stack_overflow/) - tried this (see 'trampolining' branch), but was unable to
* Add file tracking to `Span` struct (useful for error handling)
* Add more detailed error messages (see [this](https://elm-lang.org/news/compiler-errors-for-humans) and [this](https://blog.rust-lang.org/2016/08/10/Shape-of-errors-to-come.html))
* Check literal error parsing for keywords (ex. 'if', 'match')
* Use an arena instead of boxes? (https://discordapp.com/channels/442252698964721669/443150878111694848/
717089937379557397)
* Factor out an error interface so that some of the patterns in parse errors can be used elsewhere (including printing out a line of code with the error)
* Make an issue where people can put parse error codes that they don't understand
* Use `Cow<'a, str>` instead of `String` in tuple representation
* Add generators to language (should be easy, just modify `Tuple` to add a new type)
* Use spans in the AST instead of Strings
* Add keyword Spans to AST (ex. `if`, `do`, etc.)
* Write test for 'ioCommand.tgo' (it requires user input)
* Write tests for Commands
* Add generator functions (potential for infinite lists)
  * `ones = yield 1 then ones`
  * `naturals n = yield n then naturals (n + 1)`
  * Consider memoizing results
* Improve interactions between `Int`s and `Char`s
  * Interactions like `+`, `==`, `<=`, etc.
* Check to make sure adding/subtracting/etc. don't overflow (see `checked_add` method on `i32`)
* Add 'symbols'
  * Kind of like static strings
  * `:foo`
  * Can be compared for equality, matched on
  * Can be used to create types w/o type declaration
  * Can be used for 'user keywords'
  * Can be statically checked