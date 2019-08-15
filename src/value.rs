use crate::type_::Type;
use std::ops;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Unit,
    Tuple(Vec<Value>),
    Error(String)
}

impl Value {
    pub fn is_type(&self, t: &Type) -> bool {
        t == &self.type_()
    }
    
    pub fn type_(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
            Value::Unit => Type::Unit,
            Value::Tuple(vals) =>
                Type::Tuple(vals.iter().map(|v| v.type_()).collect()),
            Value::Error(_) => Type::Error,
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    
    // ADDITION TESTS
    basic_test!(
        add_int_int
        Value::Int(1) + Value::Int(2) => Value::Int(3)
    );
    
    basic_test!(
        add_int_unit
        Value::Int(1) + Value::Unit =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        add_unit_int
        Value::Unit + Value::Int(1) =>
            binary_op_error("add", Type::Unit, Type::Int)
    );
    
    basic_test!(
        add_unit_unit
        Value::Unit + Value::Unit =>
            binary_op_error("add", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        add_deep_error_left
        (Value::Int(1) + Value::Unit) + Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        add_deep_error_right
        Value::Int(1) + (Value::Int(2) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    // SUBTRACTION TESTS
    basic_test!(
        sub_int_int
        Value::Int(3) - Value::Int(2) => Value::Int(1)
    );
    
    basic_test!(
        sub_int_unit
        Value::Int(3) - Value::Unit =>
            binary_op_error("subtract", Type::Int, Type::Unit)
    );
    
    basic_test!(
        sub_unit_int
        Value::Unit - Value::Int(2) =>
            binary_op_error("subtract", Type::Unit, Type::Int)
    );
    
    
    basic_test!(
        sub_unit_unit
        Value::Unit - Value::Unit =>
            binary_op_error("subtract", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        sub_deep_error_left
        (Value::Int(1) + Value::Unit) - Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        sub_deep_error_right
        Value::Int(3) - (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    // MULTIPLICATION TESTS
    basic_test!(
        mul_int_int
        Value::Int(2) * Value::Int(3) => Value::Int(6)
    );
    
    basic_test!(
        mul_int_unit
        Value::Int(2) * Value::Unit =>
            binary_op_error("multiply", Type::Int, Type::Unit)
    );
    
    basic_test!(
        mul_unit_int
        Value::Unit * Value::Int(3) =>
            binary_op_error("multiply", Type::Unit, Type::Int)
    );
    
    basic_test!(
        mul_unit_unit
        Value::Unit * Value::Unit =>
            binary_op_error("multiply", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        mul_deep_error_left
        (Value::Int(1) + Value::Unit) * Value::Int(3) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        mul_deep_error_right
        Value::Int(2) * (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    // DIVISION TESTS
    basic_test!(
        div_int_int
        Value::Int(3) / Value::Int(2) => Value::Int(1)
    );
    
    basic_test!(
        div_int_unit
        Value::Int(3) / Value::Unit =>
            binary_op_error("divide", Type::Int, Type::Unit)
    );
    
    basic_test!(
        div_unit_int
        Value::Unit / Value::Int(2) =>
            binary_op_error("divide", Type::Unit, Type::Int)
    );
    
    basic_test!(
        div_unit_unit
        Value::Unit / Value::Unit =>
            binary_op_error("divide", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        div_deep_error_left
        (Value::Int(1) + Value::Unit) / Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        div_deep_error_right
        Value::Int(3) / (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    // MODULO TESTS
    basic_test!(
        mod_int_int
        Value::Int(3) % Value::Int(2) => Value::Int(1)
    );
    
    basic_test!(
        mod_int_unit
        Value::Int(3) % Value::Unit =>
            binary_op_error("modulo", Type::Int, Type::Unit)
    );
    
    basic_test!(
        mod_unit_int
        Value::Unit % Value::Int(2) =>
            binary_op_error("modulo", Type::Unit, Type::Int)
    );
    
    basic_test!(
        mod_unit_unit
        Value::Unit % Value::Unit =>
            binary_op_error("modulo", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        mod_deep_error_left
        (Value::Int(1) + Value::Unit) % Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        mod_deep_error_right
        Value::Int(3) % (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
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
        and_int_unit
        Value::Int(5) & Value::Unit =>
            binary_op_error("and", Type::Int, Type::Unit)
    );
    
    basic_test!(
        and_unit_int
        Value::Unit & Value::Int(2) =>
            binary_op_error("and", Type::Unit, Type::Int)
    );
    
    basic_test!(
        and_unit_unit
        Value::Unit & Value::Unit =>
            binary_op_error("and", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        and_deep_error_left
        (Value::Int(1) + Value::Unit) & Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        and_deep_error_right
        Value::Int(3) & (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
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
        or_int_unit
        Value::Int(5) | Value::Unit =>
            binary_op_error("or", Type::Int, Type::Unit)
    );
    
    basic_test!(
        or_unit_int
        Value::Unit | Value::Int(2) =>
            binary_op_error("or", Type::Unit, Type::Int)
    );
    
    basic_test!(
        or_unit_unit
        Value::Unit | Value::Unit =>
            binary_op_error("or", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        or_deep_error_left
        (Value::Int(1) + Value::Unit) | Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        or_deep_error_right
        Value::Int(3) | (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
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
        xor_int_unit
        Value::Int(5) ^ Value::Unit =>
            binary_op_error("xor", Type::Int, Type::Unit)
    );
    
    basic_test!(
        xor_unit_int
        Value::Unit ^ Value::Int(2) =>
            binary_op_error("xor", Type::Unit, Type::Int)
    );
    
    basic_test!(
        xor_unit_unit
        Value::Unit ^ Value::Unit =>
            binary_op_error("xor", Type::Unit, Type::Unit)
    );
    
    basic_test!(
        xor_deep_error_left
        (Value::Int(1) + Value::Unit) ^ Value::Int(2) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    basic_test!(
        xor_deep_error_right
        Value::Int(3) ^ (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
    
    // NEGATION TESTS
    basic_test!(
        neg_int
        -Value::Int(1) => Value::Int(-1)
    );
    
    basic_test!(
        neg_unit
        -Value::Unit =>
            unary_op_error("negate", Type::Unit)
    );
    
    basic_test!(
        neg_deep_error
        -(Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
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
        !(Value::Int(1) + Value::Unit) =>
            binary_op_error("add", Type::Int, Type::Unit)
    );
}