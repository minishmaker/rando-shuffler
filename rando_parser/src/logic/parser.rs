use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, not_line_ending, one_of, space0, space1},
    combinator::{all_consuming, cut, eof, opt, value},
    error::context,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, Parser,
};

use crate::common::{
    error::{
        many0_accumulate, many1_accumulate, merge_tuple, separated_list1_accumulate, ParseError,
        ParseExt,
    },
    parser::{full_ident, ident_part, keyword, ls, sticky},
    span::{span, span_ok, Span},
};

use super::{
    ast::{Arrow, Item, ItemHeader},
    indent::{get_indent, Indent},
};

pub mod error;
use error::LogicParseError;

#[cfg(test)]
mod test;

type ParseResult<'a, T> =
    IResult<&'a str, Result<T, Vec<LogicParseError<'a>>>, ParseError<'a, LogicParseError<'a>>>;

pub fn parse_items(full: &str) -> Result<Vec<Item>, ParseError<LogicParseError>> {
    let indent = Indent::empty();
    all_consuming(many0_accumulate(preceded(indent, move |input| {
        item(indent, full, input)
    })))(full)
    .finish()
    .map(|(_, e)| e.map_err(Into::into))?
}

fn item<'a>(indent: Indent<'a>, full: &'a str, input: &'a str) -> ParseResult<'a, Item<'a>> {
    pair(
        |input| header(full, input),
        alt((
            preceded(ls(char(':')), cut(|input| children(indent, full, input))),
            comment_line_end.map_ok(|_| Vec::new()),
        )),
    )
    .map(|(a, b)| merge_tuple(a, b))
    .map_ok(|(header, children)| Item { header, children })
    .parse(input)
}

fn children<'a>(
    indent: Indent<'a>,
    full: &'a str,
    input: &'a str,
) -> ParseResult<'a, Vec<Item<'a>>> {
    alt((
        |input| inline_children(full, input),
        |input| block_children(indent, full, input),
    ))(input)
}

fn block_children<'a>(
    indent: Indent<'a>,
    full: &'a str,
    input: &'a str,
) -> ParseResult<'a, Vec<Item<'a>>> {
    let (input, indent) = get_indent(indent, input)?;

    let error_check = space1.map(|actual| {
        Err::<(), _>(vec![LogicParseError::WrongIndent {
            base: indent.space,
            actual,
        }])
    });

    many1_accumulate(preceded(
        pair(indent, opt(error_check)),
        alt((
            move |input| item(indent, full, input),
            |input| logic_sugar(full, input),
        )),
    ))(input)
}

fn inline_children<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Vec<Item<'a>>> {
    terminated(
        alt((
            separated_list1_accumulate(
                ls(char(',')).map(Ok),
                (|input| node_header(full, input)).map_ok(|h| Item {
                    header: h,
                    children: Vec::new(),
                }),
            ),
            (|input| logic_sugar(full, input)).map_ok(|l| vec![l]),
        )),
        comment_line_end,
    )(input)
}

fn logic_sugar<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Item<'a>> {
    let mut op = sticky(one_of("&|"));
    let mut op_recover = alt((
        (&mut op).map(Ok),
        span(full, one_of("&|")).map(|o| {
            Err(vec![LogicParseError::OpMix {
                expected: o.map(|c| if c == '&' { '|' } else { '&' }),
            }])
        }),
    ));

    let (input, Span(start, children, end)) = {
        span(
            full,
            delimited(
                char('('),
                cut(separated_list1_accumulate(
                    &mut op_recover,
                    ls(alt((
                        (|input| node_header(full, input)).map_ok(|h| Item {
                            header: h,
                            children: Vec::new(),
                        }),
                        |input| logic_sugar(full, input),
                    ))),
                )),
                cut(char(')')),
            ),
        )(input)?
    };

    std::mem::drop(op_recover);
    // If it matches &, it must be &
    let op = op("&").is_ok().then(|| "and").unwrap_or("or");
    let header = ItemHeader::Node {
        append: false,
        keyword: Span(start, op, end),
        idents: Vec::new(),
    };
    Ok((input, children.map(|children| Item { header, children })))
}

fn header<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, ItemHeader<'a>> {
    alt((
        |input| node_header(full, input),
        |input| edge_header(full, input),
    ))(input)
}

fn node_header<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, ItemHeader<'a>> {
    tuple((
        span(full, keyword),
        many0_accumulate(ls(span(full, full_ident)).map(Ok)),
        opt(char('+')),
    ))
    .map(|(k, i, a)| i.map(|i| (k, i, a)))
    .map_ok(|(k, i, a)| ItemHeader::Node {
        append: a.is_some(),
        keyword: k,
        idents: i,
    })
    .parse(input)
}

fn edge_header<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, ItemHeader<'a>> {
    tuple((
        span(full, ident_part),
        cut(ls(span_ok(full, arrow))),
        cut(span(full, ident_part)),
    ))
    .map(|(l, a, r)| a.map(|a| (l, a, r)))
    .map_ok(|(l, a, r)| ItemHeader::Edge {
        left: l,
        arrow: a,
        right: r,
    })
    .parse(input)
}

fn arrow(input: &str) -> ParseResult<Arrow> {
    context(
        "arrow",
        alt((
            preceded(tag("<-"), opt(char('>'))).map(|r| Arrow::new(true, r.is_some()).unwrap()),
            value(Arrow::Right, tag("->")),
        )),
    )
    .map(Ok)
    .parse(input)
}

pub fn comment_line_end(input: &str) -> ParseResult<Option<&str>> {
    delimited(
        space0,
        opt(preceded(char('#'), not_line_ending)),
        alt((line_ending, eof)),
    )
    .map(Ok)
    .parse(input)
}
