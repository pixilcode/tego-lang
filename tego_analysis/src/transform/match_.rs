use tego_parser::ast as parser;
use crate::ast::{MatchId, AstMap};

pub fn transform_match(map: AstMap, match_: parser::Match) -> (MatchId, AstMap) {
	match match_ {
		parser::Match::Ident(ident) => unimplemented!(),
		parser::Match::Tuple(matches) => unimplemented!(),
		parser::Match::Boxed(match_) => unimplemented!(),
		parser::Match::Value(val) => unimplemented!(),
		parser::Match::Unit => unimplemented!(),
		parser::Match::Ignore => unimplemented!(),
	}
}