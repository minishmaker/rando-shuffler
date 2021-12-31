use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{space0, space1},
    combinator::{map_res, peek, recognize},
    multi::many0,
    sequence::{pair, preceded},
    IResult, Parser,
};

use crate::common::parser::require;

use super::parser::{
    comment_line_end,
    error::{ParseError, ParseErrorKind},
};

/// Parser for an indentation level.
/// Skips any preceeding blank or comment lines, then matches the given indentation
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Indent<'a> {
    pub space: &'a str,
}

impl Indent<'_> {
    fn new(space: &str) -> Indent<'_> {
        Indent { space }
    }

    pub fn empty() -> Indent<'static> {
        Indent { space: "" }
    }
}

impl<'a> Parser<&'a str, &'a str, ParseError<'a>> for Indent<'a> {
    fn parse(&mut self, input: &'a str) -> IResult<&'a str, &'a str, ParseError<'a>> {
        preceded(
            many0(comment_line_end),
            alt((
                tag(self.space),
                map_res(space0, |actual| {
                    Err(ParseErrorKind::WrongIndent {
                        base: self.space,
                        actual,
                    })
                }),
            )),
        )(input)
    }
}

/// Gets the next indentation level, given the previous one.
/// Skips any preceeding comments or blank lines.
pub fn get_indent<'a>(
    prev: Indent<'a>,
    input: &'a str,
) -> IResult<&'a str, Indent<'a>, ParseError<'a>> {
    preceded(
        many0(comment_line_end),
        alt((
            peek(recognize(pair(prev, space1))),
            require(map_res(space0, |actual| {
                Err(ParseErrorKind::WrongIndent {
                    base: prev.space,
                    actual,
                })
            })),
        )),
    )
    .map(Indent::new)
    .parse(input)
}
