pub mod ast;
pub mod parser;

mod check;
mod indent;

pub use check::TreeError;

use self::ast::ScopeChild;

pub fn parse<'a, 'b>(
    input: &'a str,
    scopes: &[&'b str],
) -> Result<ScopeChild<'a>, TreeError<'a, 'b>> {
    // Temporary unwrap
    let items = parser::parse_items(input).unwrap();

    check::convert_scope_children(items, scopes)
}
