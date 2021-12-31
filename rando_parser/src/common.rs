use std::ops::Range;

use crate::{FullIdent, Ident};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alphanumeric1, char, multispace0, space0},
    combinator::{consumed, map_res, recognize},
    error::{ErrorKind, FromExternalError, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded},
    IResult, InputTakeAtPosition, Parser,
};

pub type Span<T> = (usize, T, usize);

pub fn limits<T>(spans: &[Span<T>]) -> Option<Span<()>> {
    (spans.len() > 0).then(|| (spans[0].0, (), spans[spans.len() - 1].2))
}

pub fn substr_index<'a, 'b>(full: &'a str, sub: &'b str) -> Option<usize> {
    assert!(full.len() <= (isize::MAX as usize));
    let full = full.as_bytes().as_ptr_range();
    let sub = sub.as_bytes().as_ptr_range();
    assert!(sub.start <= sub.end && full.start <= full.end);
    if sub.start >= full.start && sub.end <= full.end {
        unsafe {
            // Safety: sub is known to be within full
            Some(sub.start.offset_from(full.start) as usize)
        }
    } else {
        None
    }
}

pub fn span<'a: 'b, 'b, F: 'b, O, E>(
    full: &'a str,
    mut parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Span<O>, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        let (next, out) = parser(input)?;
        let end = substr_index(full, next).unwrap();
        let len = input.len() - next.len();
        Ok((next, (end - len, out, end)))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CommonError {
    ExpectedKeyword,
    ExpectedIdent,
}

impl CommonError {
    pub fn diagnostic_message(&self) -> String {
        match self {
            CommonError::ExpectedKeyword => "Expected keyword, found identifier".into(),
            CommonError::ExpectedIdent => "Expected identifier, found keyword".into(),
        }
    }

    pub fn diagnostic_notes(&self) -> Vec<String> {
        match self {
            CommonError::ExpectedKeyword => vec!["Keywords must begin with a lowercase letter".into()],
            CommonError::ExpectedIdent => vec![
                "Identifiers cannot start with lowercase letters unless enclosed in quotes (\")"
                    .into()
            ]
        }
    }

    pub fn range(&self, remaining: &str, full: &str) -> Range<usize> {
        span::<_, _, ()>(full, recognize(many0(alt((alphanumeric1, tag("_"))))))(remaining)
            .map(|(_, (s, _, e))| s..e)
            .unwrap()
    }
}

pub fn relation_name(input: &str) -> IResult<&str, &str> {
    recognize(many0(alt((alphanumeric1, tag("_")))))(input)
}

pub fn keyword<'a, E>(input: &'a str) -> IResult<&str, &str, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, CommonError>,
{
    alt((
        keyword_unchecked,
        map_res(ident_part_unchecked, |_| Err(CommonError::ExpectedKeyword)),
    ))(input)
}

fn keyword_unchecked<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&str, &str, E> {
    recognize(pair(asciilower1, many0(alt((alphanumeric1, tag("_"))))))(input)
}

pub fn full_ident<'a, E>(input: &'a str) -> IResult<&str, FullIdent<'_>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, CommonError>,
{
    alt((
        separated_list1(char('.'), ident_part).map(|idents| FullIdent::Namespaced { idents }),
        preceded(char('g'), ident_part).map(|ident| FullIdent::Global { ident }),
    ))(input)
}

pub fn ident_part<'a, E>(input: &'a str) -> IResult<&str, Ident<'_>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, CommonError>,
{
    alt((
        ident_part_unchecked,
        map_res(keyword_unchecked, |_| Err(CommonError::ExpectedIdent)),
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
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a: 'b, 'b, F: 'b, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
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

/// Upgrades errors to failures. For some reason, this isn't in base nom.
pub fn require<'a: 'b, 'b, F: 'b, O: Clone, E: ParseError<&'a str>>(
    mut inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        inner(input).map_err(|e| match e {
            nom::Err::Error(e) => nom::Err::Failure(e),
            e => e,
        })
    }
}
