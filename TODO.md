# TODO
* Build compiler (maybe use craftinginterpreters.com?)
* Add pattern matching capabilities (`match` expression)
* Be able to read and run files
* Turn `()` from first-class into tuple
* Add lists
* Add type inference
* Add enums (`|` operator)
* Add named tuples
* Add functions
* Add `,,` operator
* Error management
* Add value pattern for pattern matching
* Add function pattern for pattern matching
* Add `.` composition/application(?) operator
* Integers can be applied as functions
  * Applied on tuples, they return the item in the 0-indexed position in the tuple
  * `1 (true, 2, false)` => `2`