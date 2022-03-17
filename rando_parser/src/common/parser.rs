use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alphanumeric1, char, line_ending, multispace1, not_line_ending, space0},
    combinator::{consumed, map_res, recognize},
    error::{ErrorKind, FromExternalError, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded},
    IResult, InputTakeAtPosition, Parser,
};

use crate::{FullIdent, Ident};

use super::error::CommonError;

pub fn relation_name<'a, E>(input: &'a str) -> IResult<&str, &str, E>
where
    E: ParseError<&'a str>,
{
    recognize(many0(alt((alphanumeric1, tag("_")))))(input)
}

pub fn keyword<'a, E>(input: &'a str) -> IResult<&str, &str, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, CommonError<'a>>,
{
    alt((
        keyword_unchecked,
        map_res(recognize(ident_part_unchecked), |i| {
            Err(CommonError::ExpectedKeyword(i))
        }),
    ))(input)
}

fn keyword_unchecked<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    recognize(pair(asciilower1, many0(alt((alphanumeric1, tag("_"))))))(input)
}

pub fn full_ident<'a, E>(input: &'a str) -> IResult<&str, FullIdent<'_>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, CommonError<'a>>,
{
    alt((
        separated_list1(char('.'), ident_part).map(|idents| FullIdent::Namespaced { idents }),
        preceded(char('g'), ident_part).map(|ident| FullIdent::Global { ident }),
    ))(input)
}

pub fn ident_part<'a, E>(input: &'a str) -> IResult<&str, Ident<'_>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, CommonError<'a>>,
{
    alt((
        ident_part_unchecked,
        map_res(keyword_unchecked, |k| Err(CommonError::ExpectedIdent(k))),
    ))(input)
}

pub fn ident_part_unchecked<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&str, Ident<'_>, E> {
    alt((
        char('_').map(|_| Ident::Anon),
        normal_ident.map(Ident::Normal),
        escaped_ident.map(Ident::Escaped),
    ))(input)
}

fn normal_ident<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    recognize(pair(asciiupper1, many0(alt((alphanumeric1, tag("_"))))))(input)
}

pub fn asciilower1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    input.split_at_position1_complete(|item| !item.is_ascii_lowercase(), ErrorKind::AlphaNumeric)
}
pub fn asciiupper1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    input.split_at_position1_complete(|item| !item.is_ascii_uppercase(), ErrorKind::AlphaNumeric)
}

pub fn escaped_ident<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    delimited(char('"'), is_not("\"\r\n"), char('"'))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace and comments, returning the output of `inner`.
pub fn cs<'a: 'b, 'b, F: 'b, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
        alt((
            delimited(tag("/*"), take_until("*/"), tag("*/")),
            delimited(tag("//"), not_line_ending, line_ending),
        ))(input)
    }

    delimited(
        many0(alt((multispace1, comment))),
        inner,
        many0(alt((multispace1, comment))),
    )
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing non-newline whitespace, returning the output of `inner`.
pub fn ls<'a: 'b, 'b, F: 'b, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(space0, inner, space0)
}

/// A stateful combinator that takes a parser `inner` and produces a parser that will first match with the
/// provided parser, and then match the exact string matched by the first successful parse.
pub fn sticky<'a: 'b, 'b, F: 'b, O: Clone, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    // Make available for updating
    let mut val: Option<(&str, O)> = None;

    // Get the input with the inner function
    let mut inner = consumed(inner);
    move |input| {
        match &val {
            // Just tag and clone if done
            Some((text, val)) => tag(*text).map(|_| val.clone()).parse(input),
            // Parse for the first time
            None => {
                let (input, (text, parsed)) = inner(input)?;
                val = Some((text, parsed.clone()));
                Ok((input, parsed))
            }
        }
    }
}
