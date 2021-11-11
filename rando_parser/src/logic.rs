pub mod ast;

pub mod lexer;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar, "/src/logic/grammar.rs");

use std::convert::Infallible;

use ast::Scope;
use grammar::TreeParser;
use lexer::Tok;

pub fn parse(
    input: &str,
) -> Result<Vec<Scope>, lalrpop_util::ParseError<usize, Tok<'_>, Infallible>> {
    let lexer = lexer::lex(input);
    TreeParser::new().parse(input, lexer)
}
