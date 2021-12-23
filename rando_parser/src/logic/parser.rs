use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, not_line_ending, one_of, space0},
    combinator::{all_consuming, eof, opt, value},
    error::Error,
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Err, IResult, Parser,
};

use crate::{
    common::{full_ident, ident_part, keyword, ls, span, sticky, Span},
    FullIdent, Ident,
};

use super::{
    ast::Arrow,
    indent::{get_indent, Indent},
};

#[cfg(test)]
mod test;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Item<'a> {
    pub header: ItemHeader<'a>,
    pub children: Vec<Item<'a>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ItemHeader<'a> {
    Node {
        append: bool,
        keyword: Span<&'a str>,
        idents: Vec<Span<FullIdent<'a>>>,
    },
    Edge {
        left: Span<Ident<'a>>,
        arrow: Span<Arrow>,
        right: Span<Ident<'a>>,
    },
}

pub fn parse_items(full: &str) -> Result<Vec<Item>, Err<Error<&str>>> {
    let indent = Indent::empty();
    all_consuming(many0(preceded(indent, move |input| {
        item(indent, full, input)
    })))(full)
    .map(|(_, items)| items)
}

fn item<'a>(indent: Indent<'a>, full: &'a str, input: &'a str) -> IResult<&'a str, Item<'a>> {
    pair(
        |input| header(full, input),
        alt((
            preceded(ls(char(':')), |input| children(indent, full, input)),
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
) -> IResult<&'a str, Vec<Item<'a>>> {
    alt((
        |input| inline_children(full, input),
        |input| block_children(indent, full, input),
    ))(input)
}

fn block_children<'a>(
    indent: Indent<'a>,
    full: &'a str,
    input: &'a str,
) -> IResult<&'a str, Vec<Item<'a>>> {
    let (input, indent) = get_indent(indent, input)?;

    many1(preceded(indent, move |input| item(indent, full, input)))(input)
}

fn inline_children<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, Vec<Item<'a>>> {
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

fn logic_sugar<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, Item<'a>> {
    let mut op = sticky(one_of("&|"));

    let (input, (start, children, end)) = {
        span(
            full,
            delimited(
                char('('),
                separated_list1(
                    &mut op,
                    ls(alt((
                        (|input| node_header(full, input)).map(|h| Item {
                            header: h,
                            children: Vec::new(),
                        }),
                        |input| logic_sugar(full, input),
                    ))),
                ),
                char(')'),
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

fn header<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, ItemHeader<'a>> {
    alt((
        |input| node_header(full, input),
        |input| edge_header(full, input),
    ))(input)
}

fn node_header<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, ItemHeader<'a>> {
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

fn edge_header<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, ItemHeader<'a>> {
    tuple((
        span(full, ident_part),
        ls(span(full, arrow)),
        span(full, ident_part),
    ))
    .map(|(l, a, r)| ItemHeader::Edge {
        left: l,
        arrow: a,
        right: r,
    })
    .parse(input)
}

fn arrow(input: &str) -> IResult<&str, Arrow> {
    alt((
        preceded(tag("<-"), opt(char('>'))).map(|r| Arrow::new(true, r.is_some()).unwrap()),
        value(Arrow::Right, tag("->")),
    ))(input)
}

pub fn comment_line_end(input: &str) -> IResult<&str, Option<&str>> {
    delimited(
        space0,
        opt(preceded(char('#'), not_line_ending)),
        alt((line_ending, eof)),
    )(input)
}
