use slotmap::DenseSlotMap;
use std::rc::Rc;
use crate::ast::*;

pub struct AstMap {
	matches: DenseSlotMap<MatchId, Rc<Match>>,
	exprs: DenseSlotMap<ExprId, Rc<Expr>>,
	decls: DenseSlotMap<DeclId, Rc<Decl>>,
	progs: DenseSlotMap<ProgId, Rc<Prog>>
}
