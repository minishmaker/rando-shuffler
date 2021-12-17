use crate::{FullIdent, Ident};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alphanumeric1, char, multispace0, space0},
    combinator::{consumed, recognize},
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded},
    IResult, InputTakeAtPosition, Parser,
};

pub type Span<T> = (usize, T, usize);

fn substr_index<'a, 'b>(full: &'a str, sub: &'b str) -> Option<usize> {
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

pub fn span<'a, 'b: 'a, F: 'a, O, E>(
    full: &'b str,
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

pub fn keyword(input: &str) -> IResult<&str, &str> {
    recognize(pair(asciilower1, many0(alt((alphanumeric1, tag("_"))))))(input)
}

pub fn full_ident(input: &str) -> IResult<&str, FullIdent<'_>> {
    alt((
        separated_list1(char('.'), ident_part).map(|idents| FullIdent::Namespaced { idents }),
        preceded(char('g'), ident_part).map(|ident| FullIdent::Global { ident }),
    ))(input)
}

pub fn ident_part(input: &str) -> IResult<&str, Ident<'_>> {
    alt((
        char('_').map(|_| Ident::Anon),
        normal_ident.map(Ident::Normal),
        escaped_ident.map(Ident::Escaped),
    ))(input)
}

fn normal_ident(input: &str) -> IResult<&str, &str> {
    recognize(pair(asciiupper1, many0(alt((alphanumeric1, tag("_"))))))(input)
}

pub fn asciilower1(input: &str) -> IResult<&str, &str> {
    input.split_at_position1_complete(|item| !item.is_ascii_lowercase(), ErrorKind::AlphaNumeric)
}
pub fn asciiupper1(input: &str) -> IResult<&str, &str> {
    input.split_at_position1_complete(|item| !item.is_ascii_uppercase(), ErrorKind::AlphaNumeric)
}

pub fn escaped_ident(input: &str) -> IResult<&str, &str> {
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
