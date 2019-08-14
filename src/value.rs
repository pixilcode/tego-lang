use crate::type_::Type;
use std::ops;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Unit,
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
            Value::Error(_) => Type::Error,
        }
    }
}

impl ops::Add for Value {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        if let Value::Error(_) = self {
            self
        } else if let Value::Error(_) = other {
            other
        } else {
            match self {
                Value::Int(a) =>
                    if let Value::Int(b) = other {
                        Value::Int(a + b)
                    } else {
                        Value::Error(
                            format!("Can't add 'Int' and '{}'", other.type_())
                        )
                    },
                Value::Error(_) => unreachable!(),
                _ => Value::Error(
                    format!("Can't add '{}' and '{}'", self.type_(), other.type_()))
            }
        }
    }
}

impl ops::Sub for Value {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        if let Value::Error(_) = self {
            self
        } else if let Value::Error(_) = other {
            other
        } else {
            match self {
                Value::Int(a) =>
                    if let Value::Int(b) = other {
                        Value::Int(a - b)
                    } else {
                        Value::Error(
                            format!("Can't subtract 'Int' and '{}'", other.type_())
                        )
                    },
                Value::Error(_) => unreachable!(),
                _ => Value::Error(
                    format!("Can't subtract '{}' and '{}'", self.type_(), other.type_()))
            }
        }
    }
}

impl ops::Mul for Value {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        if let Value::Error(_) = self {
            self
        } else if let Value::Error(_) = other {
            other
        } else {
            match self {
                Value::Int(a) =>
                    if let Value::Int(b) = other {
                        Value::Int(a * b)
                    } else {
                        Value::Error(
                            format!("Can't multiply 'Int' and '{}'", other.type_())
                        )
                    },
                Value::Error(_) => unreachable!(),
                _ => Value::Error(
                    format!("Can't multiply '{}' and '{}'", self.type_(), other.type_()))
            }
        }
    }
}

impl ops::Div for Value {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        if let Value::Error(_) = self {
            self
        } else if let Value::Error(_) = other {
            other
        } else {
            match self {
                Value::Int(a) =>
                    if let Value::Int(b) = other {
                        Value::Int(a / b)
                    } else {
                        Value::Error(
                            format!("Can't divide 'Int' and '{}'", other.type_())
                        )
                    },
                Value::Error(_) => unreachable!(),
                _ => Value::Error(
                    format!("Can't divide '{}' and '{}'", self.type_(), other.type_()))
            }
        }
    }
}

impl ops::Rem for Value {
    type Output = Self;
    fn rem(self, other: Self) -> Self {
        if let Value::Error(_) = self {
            self
        } else if let Value::Error(_) = other {
            other
        } else {
            match self {
                Value::Int(a) =>
                    if let Value::Int(b) = other {
                        Value::Int(a % b)
                    } else {
                        Value::Error(
                            format!("Can't modulo 'Int' and '{}'", other.type_())
                        )
                    },
                Value::Error(_) => unreachable!(),
                _ => Value::Error(
                    format!("Can't modulo '{}' and '{}'", self.type_(), other.type_()))
            }
        }
    }
}

impl ops::Neg for Value {
    type Output = Self;
    fn neg(self) -> Self {
        if let Value::Error(_) = self {
            self
        } else {
            match self {
                Value::Int(a) => Value::Int(-a),
                Value::Error(_) => unreachable!(),
                _ => Value::Error(
                    format!("Can't negate (integer) '{}'", self.type_()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    macro_rules! operator_test {
        ( $name:ident $actual:expr => $expected:expr) => {
            #[allow(clippy::eq_op)]
            #[test]
            fn $name() {
                assert_eq!($expected, $actual)
            }
        };
    }
    
    // ADDITION TESTS
    operator_test!(
        add_int_int
        Value::Int(1) + Value::Int(2) => Value::Int(3)
    );
    
    operator_test!(
        add_int_unit
        Value::Int(1) + Value::Unit =>
            binary_op_error("add", "Int", "Unit")
    );
    
    operator_test!(
        add_unit_int
        Value::Unit + Value::Int(1) =>
            binary_op_error("add", "Unit", "Int")
    );
    
    operator_test!(
        add_unit_unit
        Value::Unit + Value::Unit =>
            binary_op_error("add", "Unit", "Unit")
    );
    
    operator_test!(
        add_deep_error_left
        (Value::Int(1) + Value::Unit) + Value::Int(2) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    operator_test!(
        add_deep_error_right
        Value::Int(1) + (Value::Int(2) + Value::Unit) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    // SUBTRACTION TESTS
    operator_test!(
        sub_int_int
        Value::Int(3) - Value::Int(2) => Value::Int(1)
    );
    
    operator_test!(
        sub_int_unit
        Value::Int(3) - Value::Unit =>
            binary_op_error("subtract", "Int", "Unit")
    );
    
    operator_test!(
        sub_unit_int
        Value::Unit - Value::Int(2) =>
            binary_op_error("subtract", "Unit", "Int")
    );
    
    
    operator_test!(
        sub_unit_unit
        Value::Unit - Value::Unit =>
            binary_op_error("subtract", "Unit", "Unit")
    );
    
    operator_test!(
        sub_deep_error_left
        (Value::Int(1) + Value::Unit) - Value::Int(2) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    operator_test!(
        sub_deep_error_right
        Value::Int(3) - (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    // MULTIPLICATION TESTS
    operator_test!(
        mul_int_int
        Value::Int(2) * Value::Int(3) => Value::Int(6)
    );
    
    operator_test!(
        mul_int_unit
        Value::Int(2) * Value::Unit =>
            binary_op_error("multiply", "Int", "Unit")
    );
    
    operator_test!(
        mul_unit_int
        Value::Unit * Value::Int(3) =>
            binary_op_error("multiply", "Unit", "Int")
    );
    
    operator_test!(
        mul_unit_unit
        Value::Unit * Value::Unit =>
            binary_op_error("multiply", "Unit", "Unit")
    );
    
    operator_test!(
        mul_deep_error_left
        (Value::Int(1) + Value::Unit) * Value::Int(3) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    operator_test!(
        mul_deep_error_right
        Value::Int(2) * (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    // DIVISION TESTS
    operator_test!(
        div_int_int
        Value::Int(3) / Value::Int(2) => Value::Int(1)
    );
    
    operator_test!(
        div_int_unit
        Value::Int(3) / Value::Unit =>
            binary_op_error("divide", "Int", "Unit")
    );
    
    operator_test!(
        div_unit_int
        Value::Unit / Value::Int(2) =>
            binary_op_error("divide", "Unit", "Int")
    );
    
    operator_test!(
        div_unit_unit
        Value::Unit / Value::Unit =>
            binary_op_error("divide", "Unit", "Unit")
    );
    
    operator_test!(
        div_deep_error_left
        (Value::Int(1) + Value::Unit) / Value::Int(2) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    operator_test!(
        div_deep_error_right
        Value::Int(3) / (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    // MODULO TESTS
    operator_test!(
        mod_int_int
        Value::Int(3) % Value::Int(2) => Value::Int(1)
    );
    
    operator_test!(
        mod_int_unit
        Value::Int(3) % Value::Unit =>
            binary_op_error("modulo", "Int", "Unit")
    );
    
    operator_test!(
        mod_unit_int
        Value::Unit % Value::Int(2) =>
            binary_op_error("modulo", "Unit", "Int")
    );
    
    operator_test!(
        mod_unit_unit
        Value::Unit % Value::Unit =>
            binary_op_error("modulo", "Unit", "Unit")
    );
    
    operator_test!(
        mod_deep_error_left
        (Value::Int(1) + Value::Unit) % Value::Int(2) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    operator_test!(
        mod_deep_error_right
        Value::Int(3) % (Value::Int(1) + Value::Unit) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    // INTEGER NEGATION TESTS
    operator_test!(
        i_neg_int
        -Value::Int(1) => Value::Int(-1)
    );
    
    operator_test!(
        i_neg_unit
        -Value::Unit =>
            unary_op_error("negate (integer)", "Unit")
    );
    
    operator_test!(
        i_neg_deep_error
        -(Value::Int(1) + Value::Unit) =>
            binary_op_error("add", "Int", "Unit")
    );
    
    fn binary_op_error(op: &str, type_a: &str, type_b: &str) -> Value {
        Value::Error(format!("Can't {} '{}' and '{}'",
            op,
            type_a,
            type_b)
        )
    }
    
    fn unary_op_error(op: &str, type_: &str) -> Value {
        Value::Error(format!("Can't {} '{}'", op, type_))
    }
}