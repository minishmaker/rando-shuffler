use std::convert::identity;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, not_line_ending, one_of, space0, space1},
    combinator::{all_consuming, eof, map_res, opt, success, value},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, Parser,
};

use crate::common::{full_ident, ident_part, keyword, ls, require, span, sticky};

use super::{
    ast::{Arrow, Item, ItemHeader},
    indent::{get_indent, Indent},
};

pub mod error;
use error::{ParseError, ParseErrorKind};

#[cfg(test)]
mod test;

pub fn parse_items(full: &str) -> Result<Vec<Item>, ParseError> {
    let indent = Indent::empty();
    all_consuming(many0(preceded(indent, move |input| {
        item(indent, full, input)
    })))(full)
    .finish()
    .map(|(_, items)| items)
}

fn item<'a>(
    indent: Indent<'a>,
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, Item<'a>, ParseError<'a>> {
    pair(
        |input| header(full, input),
        alt((
            preceded(
                ls(char(':')),
                require(|input| children(indent, full, input)),
            ),
            comment_line_end.map(|_| Vec::new()),
        )),
    )
    .map(|(header, children)| Item { header, children })
    .parse(input)
}

fn children<'a>(
    indent: Indent<'a>,
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, Vec<Item<'a>>, ParseError<'a>> {
    alt((
        |input| inline_children(full, input),
        |input| block_children(indent, full, input),
    ))(input)
}

fn block_children<'a>(
    indent: Indent<'a>,
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, Vec<Item<'a>>, ParseError<'a>> {
    let (input, indent) = get_indent(indent, input)
        .map_err(|i| {
            println!("{:?}", i);
            i
        })?;

    let error_check = require(map_res(opt(space1), |s| {
        if let Some(actual) = s {
            Err(ParseErrorKind::WrongIndent {
                base: indent.space,
                actual,
            })
        } else {
            Ok(())
        }
    }));

    many1(preceded(pair(indent, error_check), move |input| {
        item(indent, full, input)
    }))(input)
}

fn inline_children<'a>(
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, Vec<Item<'a>>, ParseError<'a>> {
    terminated(
        alt((
            separated_list1(
                ls(char(',')),
                (|input| node_header(full, input)).map(|h| Item {
                    header: h,
                    children: Vec::new(),
                }),
            ),
            (|input| logic_sugar(full, input)).map(|l| vec![l]),
        )),
        comment_line_end,
    )(input)
}

fn logic_sugar<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, Item<'a>, ParseError<'a>> {
    let mut op = sticky(one_of("&|"));
    let mut backup = require(map_res(one_of("&|"), |o| {
        Err(ParseErrorKind::OpMix {
            expected: if o == '&' { '|' } else { '&' },
        })
    }));

    let (input, (start, children, end)) = {
        span(
            full,
            delimited(
                char('('),
                require(separated_list1(
                    &mut op,
                    ls(alt((
                        (|input| node_header(full, input)).map(|h| Item {
                            header: h,
                            children: Vec::new(),
                        }),
                        |input| logic_sugar(full, input),
                    ))),
                )),
                alt((char(')'), &mut backup)),
            ),
        )(input)?
    };

    // If it matches &, it must be &
    let op = op("&").is_ok().then(|| "and").unwrap_or("or");
    let header = ItemHeader::Node {
        append: false,
        keyword: (start, op, end),
        idents: Vec::new(),
    };

    Ok((input, Item { header, children }))
}

fn header<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, ItemHeader<'a>, ParseError<'a>> {
    alt((
        |input| node_header(full, input),
        |input| edge_header(full, input),
    ))(input)
}

fn node_header<'a>(
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, ItemHeader<'a>, ParseError<'a>> {
    tuple((
        span(full, keyword),
        many0(ls(span(full, full_ident))),
        opt(char('+')),
    ))
    .map(|(k, i, a)| ItemHeader::Node {
        append: a.is_some(),
        keyword: k,
        idents: i,
    })
    .parse(input)
}

fn edge_header<'a>(
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, ItemHeader<'a>, ParseError<'a>> {
    tuple((
        span(full, ident_part),
        require(ls(span(full, arrow))),
        require(span(full, ident_part)),
    ))
    .map(|(l, a, r)| ItemHeader::Edge {
        left: l,
        arrow: a,
        right: r,
    })
    .parse(input)
}

fn arrow(input: &str) -> IResult<&str, Arrow, ParseError> {
    alt((
        preceded(tag("<-"), opt(char('>'))).map(|r| Arrow::new(true, r.is_some()).unwrap()),
        value(Arrow::Right, tag("->")),
        map_res(success(Err(ParseErrorKind::Arrow)), identity),
    ))(input)
}

pub fn comment_line_end(input: &str) -> IResult<&str, Option<&str>, ParseError> {
    delimited(
        space0,
        opt(preceded(char('#'), not_line_ending)),
        alt((line_ending, eof)),
    )(input)
}
