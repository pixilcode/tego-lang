use slotmap::new_key_type;

mod match_;
mod expr;
mod decl;
mod prog;

mod map;

pub use crate::ast::match_::*;
pub use crate::ast::expr::*;
pub use crate::ast::decl::*;
pub use crate::ast::prog::*;

pub use crate::ast::map::AstMap;

new_key_type! {
	pub struct MatchId;
	pub struct ExprId;
	pub struct DeclId;
	pub struct ProgId;
}