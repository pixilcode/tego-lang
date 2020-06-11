pub trait MatchOutput {
	fn tuple(a: Self, b: Self) -> Self;
	fn unit() -> Self;
	fn boxed(a: Self) -> Self;
	fn ignore() -> Self;
	fn bool (b: bool) -> Self;
	fn int(i: i32) -> Self;
	fn ident(s: &str) -> Self;
	fn string(s: &str) -> Self;
	fn char(c: char) -> Self;
}

pub trait ExprOutput: Sized + Clone {
	type Match: MatchOutput;

	fn binary(a: Self, op: &str, b: Self) -> Self;
	fn unary(op: &str, a: Self) -> Self;
	fn let_expr(ident: Self::Match, value: Self, body: Self) -> Self;
	fn delayed(ident: Self::Match, value: Self, body: Self) -> Self;
	fn if_expr(cond: Self, t: Self, f: Self) -> Self;
	fn match_(val: Self, patterns: Vec<(Self::Match, Self)>) -> Self;
	fn fn_expr(param: Self::Match, body: Self) -> Self;
	fn fn_app(function: Self, arg: Self) -> Self;
	fn unit() -> Self;
	fn boxed(inner: Self) -> Self;
	fn bool(b: bool) -> Self;
	fn int(i: i32) -> Self;
	fn variable(ident: &str) -> Self;
	fn string(s: &str) -> Self;
	fn char(c: char) -> Self;
}

pub trait DeclOutput {
	type Expr: ExprOutput;

	fn expression(ident: &str, body: Self::Expr) -> Self;

	fn to_main(&self, _main_fn_ident: &str) -> Option<Self::Expr> {
		None
	}
}

pub trait ProgOutput {
	type Decl: DeclOutput;

	fn binary(main: <Self::Decl as DeclOutput>::Expr, decls: Vec<Self::Decl>) -> Self;
	fn library(decls: Vec<Self::Decl>) -> Self;
}

// Implementation of all traits for unit `()`
// Can be used to simply check if code parses correctly

impl MatchOutput for () {
	fn tuple(_: Self, _: Self) -> Self {}
	fn unit() -> Self {}
	fn boxed(_: Self) -> Self {}
	fn ignore() -> Self {}
	fn bool (_: bool) -> Self {}
	fn int(_: i32) -> Self {}
	fn ident(_: &str) -> Self {}
	fn string(_: &str) -> Self {}
	fn char(_: char) -> Self {}
}

impl ExprOutput for () {
	type Match = ();

	fn binary(_: Self, _: &str, _: Self) -> Self {}
	fn unary(_: &str, _: Self) -> Self {}
	fn let_expr(_: Self::Match, _: Self, _: Self) -> Self {}
	fn delayed(_: Self::Match, _: Self, _: Self) -> Self {}
	fn if_expr(_: Self, _: Self, _: Self) -> Self {}
	fn match_(_: Self, _: Vec<(Self::Match, Self)>) -> Self {}
	fn fn_expr(_: Self::Match, _: Self) -> Self {}
	fn fn_app(_: Self, _: Self) -> Self {}
	fn unit() -> Self {}
	fn boxed(_: Self) -> Self {}
	fn bool(_: bool) -> Self {}
	fn int(_: i32) -> Self {}
	fn variable(_: &str) -> Self {}
	fn string(_: &str) -> Self {}
	fn char(_: char) -> Self {}
}

impl DeclOutput for () {
	type Expr = ();

	fn expression(_: &str, _: Self::Expr) -> Self {}
}

impl ProgOutput for () {
	type Decl = ();

	fn binary(_: <Self::Decl as DeclOutput>::Expr, _: Vec<Self::Decl>) -> Self {}
	fn library(_: Vec<Self::Decl>) -> Self {}
}
