pub mod ast;
pub mod parser;

mod check;
mod indent;

use codespan_reporting::diagnostic::Diagnostic;

use crate::common::error::ParseError;

use self::{ast::ScopeChild, check::TreeError, parser::error::LogicParseError};

pub enum LogicError<'a, 'b> {
    Parse(ParseError<'a, LogicParseError<'a>>),
    Tree(TreeError<'a, 'b>),
}

pub fn parse<'a, 'b>(
    input: &'a str,
    scopes: &[&'b str],
) -> Result<ScopeChild<'a>, LogicError<'a, 'b>> {
    parser::parse_items(input)
        .map_err(LogicError::Parse)
        .and_then(|items| check::convert_scope_children(items, scopes).map_err(LogicError::Tree))
}

impl<'a> LogicError<'a, '_> {
    pub fn diagnostics<F: Clone>(&self, file: F, source: &'a str) -> Vec<Diagnostic<F>> {
        match self {
            LogicError::Parse(e) => e.diagnostics(&file, source).collect(),
            LogicError::Tree(e) => e.diagnostics(file).collect(),
        }
    }
}
