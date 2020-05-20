Alright, so I'm having a little trouble with lifetimes. I tried to break it down to a piece of code that could be built, but the code it's in is complicated, with lots of intricate parts for which it would take a long time to build mock objects. So I'm just going to explain it the best I can. If anyone wants to take a closer look, the problem code can be found here: https://github.com/theDragonFire/tego-lang/blob/master/src/execute/interpreter.rs

I have a function with the signature:
```rust
fn eval_expr_thunk<'a>(expr: &'a Expr, env: &'a WrappedEnv) -> EvalResult<'a> {
```
I match on `expr`, an enum that represents an AST. Two of these matches are:
```rust
Expr::Unary(op, a) => 
    eval_expr_thunk(&*a, env).and_then(
        |a| EvalResult::Value(&op.eval(*a))
    ),
Expr::Binary(a, op, b) =>
    eval_expr_thunk(&*a, env).and_then(
        |a| eval_expr_thunk(&*b, env).and_then(
            |b| EvalResult::Value(&op.eval(*a, *b))
        )
	),
```
In the first match, lifetimes compile correctly, but in the second match they don't. I don't understand why.

For reference: `a` and `b` contain expressions that evaluate to, let's say, `u32`. `op.eval` evaluates some arithmetic operation (`+`, `-`, etc.). The implementation of EvalResult is
```rust
enum EvalResult<'a> {
    Thunk(Box<dyn FnOnce() -> EvalResult<'a> + 'a>),
    Value(&'a Value),
}

impl<'a> EvalResult<'a> {
    fn and_then<F>(self, f: F) -> Self
    where F: Clone + FnOnce(&Value) -> Self + 'a {
        match self {
            EvalResult::Thunk(thunk) => EvalResult::thunk(move || thunk().and_then(f.clone())),
            EvalResult::Value(val) => f(val)
        }
    }

    fn thunk<F>(f: F) -> Self
    where F: Fn() -> EvalResult<'a> + 'a {
        EvalResult::Thunk(Box::new(f))
    }
}
```
Sorry for all the text. I tried to simplify it as much as possible.