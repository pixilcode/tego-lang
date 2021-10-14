//! A general-use output for the parser in the form of an
//! abstract syntax tree (AST).
//! 
//! There are four types of nodes in the AST, corresponding
//! to the four impelmentable traits of the parser:
//!   * Expr: a Tego expression
//!   * Match: a Tego match pattern
//!   * Decl: a Tego declaration
//!   * Prog: a Tego program
//! 
//! Each node has different types, which are enumerated in
//! its corresponding documentation.
//! 
//! There are also helper types associated with expression
//! nodes and match pattern nodes:
//!   * BinaryOp: represents the binary operation of a
//!     binary expression
//!   * UnaryOp: represents the unary operation of a unary
//!     expression
//!   * ExprValue: represents the value of a literal
//!     expression
//!   * MatchVal: represents the value of a literal match
//!     pattern

mod decl;
mod expr;
mod match_;
mod prog;

pub use decl::Decl;
pub use expr::BinaryOp;
pub use expr::Expr;
pub use expr::ExprValue;
pub use expr::UnaryOp;
pub use match_::Match;
pub use match_::MatchVal;
pub use prog::Prog;
