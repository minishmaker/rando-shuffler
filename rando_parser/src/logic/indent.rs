use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{line_ending, not_line_ending, space0, space1},
    combinator::{cut, map_res, peek, recognize},
    multi::many0,
    sequence::{pair, preceded, terminated},
    IResult, Parser,
};

use crate::common::error::{recoverable, ParseError};

use super::parser::{comment_line_end, error::LogicParseError};

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

impl<'a> Parser<&'a str, &'a str, ParseError<'a, LogicParseError<'a>>> for Indent<'a> {
    fn parse(
        &mut self,
        input: &'a str,
    ) -> IResult<&'a str, &'a str, ParseError<'a, LogicParseError<'a>>> {
        preceded(many0(comment_line_end), tag(self.space))(input)
    }
}

/// Gets the next indentation level, given the previous one.
/// Skips any preceeding comments or blank lines.
pub fn get_indent<'a>(
    prev: Indent<'a>,
    input: &'a str,
) -> IResult<&'a str, Indent<'a>, ParseError<'a, LogicParseError<'a>>> {
    preceded(
        many0(comment_line_end),
        alt((
            peek(recognize(pair(prev, space1))),
            recoverable(cut(map_res(
                terminated(space0, pair(not_line_ending, line_ending)),
                |actual| {
                    Err(LogicParseError::WrongIndent {
                        base: prev.space,
                        actual,
                    })
                },
            ))),
        )),
    )
    .map(Indent::new)
    .parse(input)
}
