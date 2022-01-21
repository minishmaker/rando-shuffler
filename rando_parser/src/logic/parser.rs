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
        cut_custom, many0_accumulate, many1_accumulate, recoverable, separated_list1_accumulate,
        throw, ParseError,
    },
    parser::{full_ident, ident_part, keyword, ls, sticky},
    span::{span, Span},
};

use super::{
    ast::{Arrow, Item, ItemHeader},
    indent::{get_indent, Indent},
};

pub mod error;
use error::LogicParseError;

#[cfg(test)]
mod test;

type ParseResult<'a, T> = IResult<&'a str, T, ParseError<'a, LogicParseError<'a>>>;

pub fn parse_items(full: &str) -> Result<Vec<Item>, ParseError<LogicParseError>> {
    let indent = Indent::empty();
    all_consuming(many0_accumulate(preceded(indent, move |input| {
        item(indent, full, input)
    })))(full)
    .finish()
    .map(|(_, items)| items)
}

fn item<'a>(indent: Indent<'a>, full: &'a str, input: &'a str) -> ParseResult<'a, Item<'a>> {
    pair(
        |input| header(full, input),
        alt((
            preceded(ls(char(':')), cut(|input| children(indent, full, input))),
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
    let (input, indent) = get_indent(indent, input).map_err(|i| {
        println!("{:?}", i);
        i
    })?;

    let error_check =
        cut_custom::<(), _, _>(throw(space1, |actual| LogicParseError::WrongIndent {
            base: indent.space,
            actual,
        }));

    many1_accumulate(preceded(pair(indent, opt(error_check)), move |input| {
        item(indent, full, input)
    }))(input)
}

fn inline_children<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Vec<Item<'a>>> {
    terminated(
        alt((
            separated_list1_accumulate(
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

fn logic_sugar<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Item<'a>> {
    let mut op = sticky(one_of("&|"));
    let mut op_recover = alt((
        &mut op,
        recoverable(cut_custom(throw(span(full, one_of("&|")), |o| {
            LogicParseError::OpMix {
                expected: o.map(|c| if c == '&' { '|' } else { '&' }),
            }
        }))),
    ));

    let (input, Span(start, children, end)) = {
        span(
            full,
            delimited(
                char('('),
                cut(separated_list1_accumulate(
                    &mut op_recover,
                    ls(alt((
                        (|input| node_header(full, input)).map(|h| Item {
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
    Ok((input, Item { header, children }))
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
        many0_accumulate(ls(span(full, full_ident))),
        opt(char('+')),
    ))
    .map(|(k, i, a)| ItemHeader::Node {
        append: a.is_some(),
        keyword: k,
        idents: i,
    })
    .parse(input)
}

fn edge_header<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, ItemHeader<'a>> {
    tuple((
        span(full, ident_part),
        cut(ls(span(full, arrow))),
        cut(span(full, ident_part)),
    ))
    .map(|(l, a, r)| ItemHeader::Edge {
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
    )(input)
}

pub fn comment_line_end(input: &str) -> ParseResult<Option<&str>> {
    delimited(
        space0,
        opt(preceded(char('#'), not_line_ending)),
        alt((line_ending, eof)),
    )(input)
}
