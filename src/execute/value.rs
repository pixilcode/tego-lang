use crate::type_::Type;
use crate::environment::{Env, EnvVal};
use crate::ast::match_::{Match, MatchVal};
use crate::ast::expr::Expr;
use crate::execute::interpreter::{WrappedEnv, VarEnv, eval_expr};
use std::rc::Weak;
use std::cell::RefCell;
use std::ops;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Tuple(Vec<Value>),
    Function(Match, Box<Expr>, StoredEnv),
    Delayed { value: Box<Expr>, self_ptr: StoredEnv, outer_env: StoredEnv },
    Error(String),
}

macro_rules! impl_op {
    ($op:ty, $func:ident, $name:expr => $( $type_:pat = $new_val:expr ),+) => {
        impl $op for Value {
            type Output = Self;
            fn $func(self) -> Self {
                let error = || unary_op_error($name, self.type_());
                if let Value::Error(_) = self {
                    self
                } else {
                    match self {
                        $( $type_ => $new_val, )+
                        _ => error()
                    }
                }
            }
        }
    };
    
    ($op:ty, $func:ident, $name:expr => $( $type_a:pat, $type_b:pat = $new_val:expr ),+) => {
        impl $op for Value {
            type Output = Self;
            fn $func(self, other: Self) -> Self {
                let error = || binary_op_error($name, self.type_(), other.type_());
                if let Value::Error(_) = self {
                    self
                } else if let Value::Error(_) = other {
                    other
                } else {
                    match (&self, &other) {
                        $( ($type_a, $type_b) => $new_val, )+
                        _ => error()
                    }
                }
            }
        }
    };
    
    ($func:ident, $name:expr => $( $type_:pat = $new_val:expr ),+) => {
        pub fn $func(self) -> Self {
            let error = || unary_op_error($name, self.type_());
            if let Value::Error(_) = self {
                self
            } else {
                match self {
                    $( $type_ => $new_val, )+
                    _ => error()
                }
            }
        }
    };
    
    ($func:ident, $name:expr => $( $type_a:pat, $type_b:pat = $new_val:expr ),+) => {
        #[allow(unreachable_patterns)]
        pub fn $func(self, other: Self) -> Self {
            // For now, won't work because of matching later
            //let error = || binary_op_error($name, self.type_(), other.type_());
            let error = binary_op_error($name, self.type_(), other.type_());
            if let Value::Error(_) = self {
                self
            } else if let Value::Error(_) = other {
                other
            } else {
                match (self, other) {
                    $( ($type_a, $type_b) => $new_val, )+
                    _ => error
                }
            }
        }
    };
}

impl Value {
    pub fn eval(self, env: Option<WrappedEnv>) -> Self {
        match self {
            Value::Delayed { value, self_ptr, outer_env } => 
                Env::get_evaluated_value(&self_ptr.clone().unwrap())
                    .unwrap_or_else(|_| {
                        let val = eval_expr(
                            *value.clone(),
                            &Env::with_parent(
                                &outer_env.clone().unwrap(),
                                &env.unwrap_or_else(Env::empty)
                            )
                        );
                        Env::set_value(&self_ptr.clone().unwrap(), val.clone());
                        val
                    }),
            v => v.clone()
        }
    }
    
    pub fn is_type(&self, t: &Type) -> bool {
        t == &self.type_()
    }
    
    pub fn type_(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
            Value::Tuple(vals) =>
                Type::Tuple(vals.iter().map(|v| v.type_()).collect()),
            Value::Function(_, _, _) => Type::Fn_,
            v @ Value::Delayed{ .. } => v.clone().eval(None).type_(),
            Value::Error(_) => Type::Error,
        }
    }
    
    pub fn function(param: Match, body: Box<Expr>, env: WrappedEnv) -> Self {
        Value::Function(param, body, StoredEnv::Expr(env))
    }
    
    pub fn decl_function(param: Match, body: Box<Expr>, env: Weak<RefCell<VarEnv>>) -> Self {
        Value::Function(param, body, StoredEnv::Decl(env))
    }
    
    pub fn delayed(value: Expr, self_ptr: Weak<RefCell<VarEnv>>, outer_env: WrappedEnv) -> Self {
        Value::Delayed {
            value: Box::new(value),
            self_ptr: StoredEnv::Decl(self_ptr), // So it doesn't create a loop
            outer_env: StoredEnv::Expr(outer_env)
        }
    }
    
    pub fn delayed_decl(value: Expr, self_ptr: Weak<RefCell<VarEnv>>, outer_env: Weak<RefCell<VarEnv>>) -> Self {
        Value::Delayed {
            value: Box::new(value),
            self_ptr: StoredEnv::Decl(self_ptr),
            outer_env: StoredEnv::Decl(outer_env)
        }
    }
    
    pub fn unit() -> Self {
        Value::Tuple(vec![])
    }
    
    impl_op!(join, "join" =>
        Value::Tuple(mut a_vals), Value::Tuple(mut b_vals) =
            Value::Tuple({
                a_vals.append(&mut b_vals);
                a_vals
            }),
        Value::Tuple(mut a_vals), b =
            Value::Tuple({
                a_vals.push(b);
                a_vals
            }),
        a, Value::Tuple(mut b_vals) =
            Value::Tuple({
                b_vals.insert(0, a);
                b_vals
            }),
        a, b =
            Value::Tuple(vec![a, b])
    );
    
    impl_op!(less_than, "less than" =>
        Value::Int(a), Value::Int(b) = Value::Bool(a < b)
    );
    
    impl_op!(greater_than, "greater than" =>
        Value::Int(a), Value::Int(b) = Value::Bool(a > b)
    );
    
    impl_op!(less_than_equal, "less than/equal to" =>
        Value::Int(a), Value::Int(b) = Value::Bool(a <= b)
    );
    
    impl_op!(greater_than_equal, "greater than/equal to" =>
        Value::Int(a), Value::Int(b) = Value::Bool(a >= b)
    );
}

impl EnvVal for Value {
    fn unwrap_matches(&self, pattern: &Match) -> Result<Vec<(String, Self)>, String> {
        match (pattern, self) {
            (Match::Ident(ident), val) => Ok(vec![(ident.to_string(), val.clone())]),
            (Match::Tuple(tup_match), Value::Tuple(tup_val)) =>
                unwrap_tuple(&tup_match, &tup_val),
            (Match::Value(MatchVal::Int(a)), Value::Int(b)) =>
                if a == b {
                    Ok(vec![])
                } else {
                    Err(format!("Expected '{}', found '{}'", a, b))
                },
            (Match::Value(MatchVal::Bool(a)), Value::Bool(b)) =>
                if a == b {
                    Ok(vec![])
                } else {
                    Err(format!("Expected '{}', found '{}'", a, b))
                },
            (Match::Ignore, _) => Ok(vec![]),
            (pattern, value) => match_error(pattern, value)
        }
    }
    
    fn is_evaluated(&self) -> bool {
        match self {
            Value::Delayed { .. } => false,
            _ => true
        }
    }
}

fn unwrap_tuple(tup_match: &[Match], tup_val: &[Value]) -> Result<Vec<(String, Value)>, String> {
    let match_len = tup_match.len();
    let val_len = tup_val.len();
    match (match_len, val_len) {
        (0, 0) => Ok(vec![]),
        (0, _) => Err("Tried to match non-empty tuple against '()'".to_string()),
        (_, 0) => Err("Not enough elements to match tuple".to_string()),
        (1, 1) => tup_val[0].unwrap_matches(&tup_match[0]),
        (1, _) => Value::Tuple(tup_val.to_vec())
                .unwrap_matches(&tup_match[0]),
        _ => tup_val[0].unwrap_matches(&tup_match[0])
            .and_then(
                |mut vals|
                unwrap_tuple(&tup_match[1..], &tup_val[1..])
                .and_then(
                    |mut rest| {
                        vals.append(&mut rest);
                        Ok(vals)
                    }
                )
            )
    }
}

fn match_error(expected: &Match, found: &Value) -> Result<Vec<(String, Value)>, String> {
    Err(format!("Expected {}, found {}", 
        match expected {
            Match::Ident(_) => "value".to_string(),
            Match::Tuple(_) => "tuple".to_string(),
            Match::Value(v) => format!("'{}'", v),
            Match::Ignore => "anything".to_string(),
        },
        found.type_()
    ))
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            match self {
                Value::Int(i) => i32::to_string(i),
                Value::Bool(b) => bool::to_string(b),
                Value::Tuple(vals) => {
                    let result = vals.iter()
                        .map(|v| format!("{}", v))
                        .fold(
                            String::new(),
                            |a, s| a + &s + ", "
                        );
                    // Get rid of the last ", "
                    let result = &result[..result.len()-2];
                    format!("({})", result)
                },
                Value::Function(_, _, _) => "<fn>".to_string(),
                v @ Value::Delayed { .. } => format!("{}", v.clone().eval(None)),
                Value::Error(error) =>
                    format!("Error: {}", error)
            })
    }
}

impl_op! {
    ops::Add, add, "add" =>
        Value::Int(a), Value::Int(b) = Value::Int(a + b)
}

impl_op! {
    ops::Sub, sub, "subtract" =>
        Value::Int(a), Value::Int(b) = Value::Int(a - b)
}

impl_op! {
    ops::Mul, mul, "multiply" =>
        Value::Int(a), Value::Int(b) = Value::Int(a * b)
}

impl_op! {
    ops::Div, div, "divide" =>
        Value::Int(a), Value::Int(b) = Value::Int(a / b)
}

impl_op! {
    ops::Rem, rem, "modulo" =>
        Value::Int(a), Value::Int(b) = Value::Int(a % b)
}

impl_op! {
    ops::BitAnd, bitand, "and" =>
        Value::Int(a), Value::Int(b) = Value::Int(a & b),
        Value::Bool(a), Value::Bool(b) = Value::Bool(a & b)
}

impl_op! {
    ops::BitOr, bitor, "or" =>
        Value::Int(a), Value::Int(b) = Value::Int(a | b),
        Value::Bool(a), Value::Bool(b) = Value::Bool(a | b)
}

impl_op! {
    ops::BitXor, bitxor, "xor" =>
        Value::Int(a), Value::Int(b) = Value::Int(a ^ b),
        Value::Bool(a), Value::Bool(b) = Value::Bool(a ^ b)
}

impl_op! {
    ops::Neg, neg, "negate" =>
        Value::Int(a) = Value::Int(-a)
}

impl_op! {
    ops::Not, not, "not" =>
        Value::Bool(a) = Value::Bool(!a)
}

fn binary_op_error(op: &str, type_a: Type, type_b: Type) -> Value {
    Value::Error(format!(
        "Can't perform {} operation on '{}' and '{}'",
        op.to_uppercase(),
        type_a,
        type_b)
    )
}

fn unary_op_error(op: &str, type_: Type) -> Value {
    Value::Error(format!(
        "Can't perform {} operation on '{}'",
        op.to_uppercase(),
        type_)
    )
}

#[derive(Debug, Clone)]
pub enum StoredEnv {
    Expr(WrappedEnv),
    Decl(Weak<RefCell<VarEnv>>) // To avoid memory leaks
}

impl StoredEnv {
    pub fn unwrap(self) -> WrappedEnv {
        match self {
            StoredEnv::Expr(env) => env,
            StoredEnv::Decl(env) =>
                match env.upgrade() {
                    Some(env) => env,
                    None => unreachable!()
                }
        }
    }
}

impl PartialEq for StoredEnv {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (StoredEnv::Expr(a), StoredEnv::Expr(b)) => a == b,
            (StoredEnv::Decl(a), StoredEnv::Decl(b)) => a.upgrade() == b.upgrade(),
            (_, _) => false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    // ADDITION TESTS
    basic_test!(
        add_int_int
        Value::Int(1) + Value::Int(2) => Value::Int(3)
    );
    
    basic_test!(
        add_deep_error_left
        (Value::Int(1) + Value::Bool(true)) + Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        add_deep_error_right
        Value::Int(1) + (Value::Int(2) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // SUBTRACTION TESTS
    basic_test!(
        sub_int_int
        Value::Int(3) - Value::Int(2) => Value::Int(1)
    );
    
    basic_test!(
        sub_deep_error_left
        (Value::Int(1) + Value::Bool(true)) - Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        sub_deep_error_right
        Value::Int(3) - (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // MULTIPLICATION TESTS
    basic_test!(
        mul_int_int
        Value::Int(2) * Value::Int(3) => Value::Int(6)
    );
    
    basic_test!(
        mul_deep_error_left
        (Value::Int(1) + Value::Bool(true)) * Value::Int(3) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        mul_deep_error_right
        Value::Int(2) * (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // DIVISION TESTS
    basic_test!(
        div_int_int
        Value::Int(3) / Value::Int(2) => Value::Int(1)
    );
    
    basic_test!(
        div_deep_error_left
        (Value::Int(1) + Value::Bool(true)) / Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        div_deep_error_right
        Value::Int(3) / (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // MODULO TESTS
    basic_test!(
        mod_int_int
        Value::Int(3) % Value::Int(2) => Value::Int(1)
    );
    
    basic_test!(
        mod_deep_error_left
        (Value::Int(1) + Value::Bool(true)) % Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        mod_deep_error_right
        Value::Int(3) % (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // AND TESTS
    basic_test!(
        and_bool_bool
        Value::Bool(true) & Value::Bool(false) =>
            Value::Bool(false)
    );
    
    basic_test!(
        and_bool_int
        Value::Bool(true) & Value::Int(3) =>
            binary_op_error("and", Type::Bool, Type::Int)
    );
    
    basic_test!(
        and_int_bool
        Value::Int(5) & Value::Bool(false) =>
            binary_op_error("and", Type::Int, Type::Bool)
    );
    
    basic_test!(
        and_int_int
        Value::Int(5) & Value::Int(3) => Value::Int(1)
    );
    
    basic_test!(
        and_deep_error_left
        (Value::Int(1) + Value::Bool(true)) & Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        and_deep_error_right
        Value::Int(3) & (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // OR TESTS
    basic_test!(
        or_bool_bool
        Value::Bool(true) | Value::Bool(false) =>
            Value::Bool(true)
    );
    
    basic_test!(
        or_bool_int
        Value::Bool(true) | Value::Int(3) =>
            binary_op_error("or", Type::Bool, Type::Int)
    );
    
    basic_test!(
        or_int_bool
        Value::Int(5) | Value::Bool(false) =>
            binary_op_error("or", Type::Int, Type::Bool)
    );
    
    basic_test!(
        or_int_int
        Value::Int(5) | Value::Int(3) => Value::Int(7)
    );
    
    basic_test!(
        or_deep_error_left
        (Value::Int(1) + Value::Bool(true)) | Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        or_deep_error_right
        Value::Int(3) | (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // XOR TESTS
    basic_test!(
        xor_bool_bool
        Value::Bool(true) ^ Value::Bool(false) =>
            Value::Bool(true)
    );
    
    basic_test!(
        xor_bool_int
        Value::Bool(true) ^ Value::Int(3) =>
            binary_op_error("xor", Type::Bool, Type::Int)
    );
    
    basic_test!(
        xor_int_bool
        Value::Int(5) ^ Value::Bool(false) =>
            binary_op_error("xor", Type::Int, Type::Bool)
    );
    
    basic_test!(
        xor_int_int
        Value::Int(5) ^ Value::Int(3) => Value::Int(6)
    );
    
    basic_test!(
        xor_deep_error_left
        (Value::Int(1) + Value::Bool(true)) ^ Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    basic_test!(
        xor_deep_error_right
        Value::Int(3) ^ (Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // NEGATION TESTS
    basic_test!(
        neg_int
        -Value::Int(1) => Value::Int(-1)
    );
    
    basic_test!(
        neg_deep_error
        -(Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // NOT TESTS
    basic_test!(
        not_bool
        !Value::Bool(true) => Value::Bool(false)
    );
    
    basic_test!(
        not_int
        !Value::Int(1) => unary_op_error("not", Type::Int)
    );
    
    basic_test!(
        not_deep_error
        !(Value::Int(1) + Value::Bool(true)) =>
            binary_op_error("add", Type::Int, Type::Bool)
    );
    
    // UNWRAP TESTS
    basic_test!(
        unwrap_ident
        Value::Int(1).unwrap_matches(&Match::ident("a")) =>
            Ok(vec![("a".to_string(), Value::Int(1))]);
        Value::Tuple(vec![Value::Int(1), Value::Int(2)])
            .unwrap_matches(&Match::ident("a")) =>
                Ok(vec![
                    ("a".to_string(), Value::Tuple(vec![
                        Value::Int(1),
                        Value::Int(2)
                    ]))
                ])
    );
    
    basic_test!(
        unwrap_tuple
        Value::Tuple(
            vec![Value::Int(1), Value::Int(2), Value::Int(3)]
        ).unwrap_matches(
            &Match::tuple(
                Match::ident("a"),
                Match::tuple(Match::ident("b"), Match::ident("c"))
            )
        ) =>
            Ok(vec![
                ("a".to_string(), Value::Int(1)),
                ("b".to_string(), Value::Int(2)),
                ("c".to_string(), Value::Int(3))
            ]);
        
        Value::Tuple(
            vec![Value::Int(1), Value::Int(2), Value::Int(3)]
        ).unwrap_matches(
            &Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            )
        ) =>
            Ok(vec![
                ("a".to_string(), Value::Int(1)),
                ("b".to_string(), Value::Tuple(vec![Value::Int(2), Value::Int(3)]))
            ]);
        
        Value::Tuple(
            vec![Value::Int(1), Value::Int(2)]
        ).unwrap_matches(
            &Match::tuple(
                Match::ident("a"),
                Match::tuple(Match::ident("b"), Match::ident("c"))
            )
        ) =>
            Err("Not enough elements to match tuple".to_string());
        
        Value::Tuple(vec![]).unwrap_matches(
            &Match::Tuple(vec![])
        ) =>
            Ok(vec![]);
        
        Value::Tuple(
            vec![Value::Int(1), Value::Int(2)]
        ).unwrap_matches(
            &Match::Tuple(vec![])
        ) => Err("Tried to match non-empty tuple against '()'".to_string());
            
        
        Value::Int(1).unwrap_matches(
            &Match::tuple(Match::ident("a"), Match::ident("b"))
        ) =>
            Err("Expected tuple, found Int".to_string())
    );
}