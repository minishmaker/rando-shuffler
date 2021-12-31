pub mod ast;
pub mod parser;

mod check;
mod indent;

use std::convert::identity;

pub use check::TreeError;
use codespan_reporting::diagnostic::Diagnostic;

use self::{ast::ScopeChild, parser::error::ParseError};

pub enum LogicError<'a, 'b> {
    Parse(ParseError<'a>),
    Tree(TreeError<'a, 'b>),
}

pub fn parse<'a, 'b>(
    input: &'a str,
    scopes: &[&'b str],
) -> Result<ScopeChild<'a>, LogicError<'a, 'b>> {
    let items = parser::parse_items(input)?;

    check::convert_scope_children(items, scopes).map_err(LogicError::Tree)
}

impl<'a> From<ParseError<'a>> for LogicError<'a, '_> {
    fn from(e: ParseError<'a>) -> Self {
        LogicError::Parse(e)
    }
}

impl<'a, 'b, 'c> LogicError<'a, 'b> {
    pub fn diagnostics<F: Clone + 'a>(
        &'c self,
        file: F,
        content: &'a str,
    ) -> impl Iterator<Item = Diagnostic<F>> + 'c {
        let (parse, tree) = match self {
            LogicError::Parse(p) => (Some(p.diagnostic(content, file)), None),
            LogicError::Tree(t) => (None, Some(t.diagnostics(file))),
        };

        parse.into_iter().chain(tree.into_iter().flat_map(identity))
    }
}
