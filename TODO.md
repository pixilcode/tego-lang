# TODO
* Priorities
  1. Declarations
  2. Read and Run Files
  2. Type Inference
  3. Parsing Errors
     * Parsing functions return custom error
     * Parsing functions take a custom input that tracks column and line number
  5. Sum types (`|` operator)
     * See [Or types](http://journal.stuffwithstuff.com/2010/08/23/void-null-maybe-and-nothing/)
  4. Custom types
     * Named tuples (`Item a`, `Point (Int, Int)`, `Color (Int, Int, Int)`, etc.)
     * Named enums (`
     * `type` declaration
       * `type Identity a = Identity a` (Basic polymorphic type)
       * `type Point = Point (Int, Int)` (Product type)
       * `type Option a = Some a | None` (Polymorphic sum type)
     * Right side is type, left side is type constructor
     * Type/type constructor can only have one argument
     * Named types can be matched and unwrapped in match patterns using type constructor
  6. Integers access fields in a tuple
     * They return the item in the 0-indexed position in the tuple
     * `1 (true, 2, false)` => `2`
* Build compiler (maybe use craftinginterpreters.com?)
* Be able to read and run files
* Add lists -> Actually, lists are just tuples with all the same type
* Add named tuples
* Add `,,` operator
* *Add function pattern for pattern matching*
* Add `.` composition/application(?) operator
  * `id 1` == `1.id`
  * `fn x -> id (id x)` ~= `id . id`
* Add documentation to all functions/enums/structs
* Values become Rc's?
  * Once made, they should never change ∴ can have multiple owned references
* **Add optional new lines to expressions**
* REPL can expect multiple lines
