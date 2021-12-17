use nom::{
    bytes::complete::tag,
    character::complete::space1,
    combinator::{peek, recognize},
    error::Error,
    multi::many0,
    sequence::{pair, preceded},
    IResult, Parser,
};

use super::parser::comment_line_end;

/// Parser for an indentation level.
/// Skips any preceeding blank or comment lines, then matches the given indentation
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Indent<'a> {
    space: &'a str,
}

impl Indent<'_> {
    fn new(space: &str) -> Indent<'_> {
        Indent { space }
    }

    pub fn empty() -> Indent<'static> {
        Indent { space: "" }
    }
}

impl<'a> Parser<&'a str, &'a str, Error<&'a str>> for Indent<'_> {
    fn parse(&mut self, input: &'a str) -> IResult<&'a str, &'a str, Error<&'a str>> {
        preceded(many0(comment_line_end), tag(self.space))(input)
    }
}

/// Gets the next indentation level, given the previous one.
/// Skips any preceeding comments or blank lines.
pub fn get_indent<'a>(prev: Indent<'a>, input: &'a str) -> IResult<&'a str, Indent<'a>> {
    preceded(
        many0(comment_line_end),
        peek(recognize(pair(prev, space1))).map(Indent::new),
    )(input)
}
