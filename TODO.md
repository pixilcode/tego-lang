# TODO
* Priorities
  1. Declarations
     * Almost done, just need to add delayed evaluation so that decls evaluate correctly
     * Also, might as well **add a delayed** (lazy) evaluation keyword while I'm at it
  2. Read and Run Files
  3. IO Monad
  4. Type Inference
  5. Parsing Errors
     * Parsing functions return custom error
     * Parsing functions take a custom input that tracks column and line number
  6. Sum types (`|` operator)
     * See [Or types](http://journal.stuffwithstuff.com/2010/08/23/void-null-maybe-and-nothing/)
  7. Custom types
     * Named tuples (`Item a`, `Point (Int, Int)`, `Color (Int, Int, Int)`, etc.)
     * Named enums (`
     * `type` declaration
       * `type Identity a = Identity a` (Basic polymorphic type)
       * `type Point = Point (Int, Int)` (Product type)
       * `type Option a = Some a | None` (Polymorphic sum type)
     * Right side is type, left side is type constructor
     * Type/type constructor can only have one argument
     * Named types can be matched and unwrapped in match patterns using type constructor
  8. Integers access fields in a tuple
     * They return the item in the 0-indexed position in the tuple
     * `1 (true, 2, false)` => `2`
* Build compiler (maybe use craftinginterpreters.com?)
* Be able to read and run files
* Add lists -> Actually, lists are just tuples with all the same type
* Add named tuples
* Add `,,` operator
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