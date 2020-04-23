# TODO
* Build compiler (maybe use craftinginterpreters.com?)
* Be able to read and run files
* Add lists -> Actually, lists are just tuples with all the same type
* Add type inference
* Add enums (`|` operator)
  * See [Or types](http://journal.stuffwithstuff.com/2010/08/23/void-null-maybe-and-nothing/)
  * Also allows for named enums
* Add named tuples
* Add `,,` operator
* Error management
* *Add function pattern for pattern matching*
* Add `.` composition/application(?) operator
* Integers can be applied as functions
  * Applied on tuples, they return the item in the 0-indexed position in the tuple
  * `1 (true, 2, false)` => `2`
* Add documentation to all functions/enums/structs
* Value's become Rc's?
* Add optional new lines to expressions