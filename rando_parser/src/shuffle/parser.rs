use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, u32},
    combinator::{not, opt},
    error::ParseError as NomParseError,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser as _,
};

use crate::common::{
    error::{
        fold_separated_list0_accumulate, many1_accumulate, merge_tuple, separated_list1_accumulate,
        ParseError, ParseExt, RandoError,
    },
    parser::{cs, full_ident, relation_name},
    span::span,
};

use super::ast::{DataExpr, RelationCtor, RelationExpr, SetCtor, Value};

pub struct ShuffleExprError<'a>(&'a ());

impl<'a> RandoError<'a> for ShuffleExprError<'a> {
    fn from_common(common: crate::common::error::CommonError<'a>) -> Self {
        todo!()
    }

    fn diagnostic<F: Clone>(
        &self,
        file: &F,
        input: &str,
    ) -> codespan_reporting::diagnostic::Diagnostic<F> {
        todo!()
    }
}

type ParseResult<'a, T> =
    IResult<&'a str, Result<T, Vec<ShuffleExprError<'a>>>, ParseError<'a, ShuffleExprError<'a>>>;

pub fn relation_expr<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RelationExpr<'a>> {
    alt((
        tuple((
            word("match"),
            cs(set_constructor),
            word("in"),
            cs(|i| map(full, i)),
            cs(word("with")),
            |i| match_body(full, i),
        ))
        .map(|(_, a, _, b, _, c)| merge_tuple(merge_tuple(a, b), c))
        .map_ok(|((i, r), o)| RelationExpr::Match {
            input: i,
            relation: Box::new(r),
            output: o,
        }),
        |i| map(full, i),
    ))(input)
}

fn match_body<'a>(
    full: &'a str,
    input: &'a str,
) -> ParseResult<'a, Vec<(SetCtor<'a>, RelationExpr<'a>)>> {
    delimited(
        char('('),
        many1_accumulate(
            separated_pair(
                cs(set_constructor),
                tag("->"),
                cs(|i| relation_expr(full, i)),
            )
            .map(|(a, b)| merge_tuple(a, b)),
        ),
        char(')'),
    )(input)
}

fn map<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RelationExpr<'a>> {
    alt((
        preceded(
            word("map"),
            pair(
                separated_pair(
                    cs(|i| connect(full, i)),
                    word("to"),
                    cs(|i| connect(full, i)),
                ),
                opt(preceded(
                    cs(word("including")),
                    separated_list1_accumulate(cs(char(',')).map(Ok), |i| connect(full, i)),
                )),
            ),
        )
        .map(|((a, b), c)| merge_tuple(merge_tuple(a, b), c.transpose()))
        .map_ok(|((f, t), i)| RelationExpr::Map {
            from: Box::new(f),
            to: Box::new(t),
            including: i.unwrap_or_default(),
        }),
        |i| connect(full, i),
    ))(input)
}

fn connect<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RelationExpr<'a>> {
    alt((
        terminated(|i| then(full, i), not(cs(word("connect")))),
        separated_list1_accumulate(cs(word("connect")).map(Ok), |i| then(full, i))
            .map_ok(RelationExpr::Connect),
    ))(input)
}

fn then<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RelationExpr<'a>> {
    alt((
        terminated(|i| unary(full, i), not(cs(word("then")))),
        separated_list1_accumulate(cs(word("then")).map(Ok), |i| unary(full, i))
            .map_ok(RelationExpr::Then),
    ))(input)
}

fn unary<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RelationExpr<'a>> {
    alt((
        preceded(
            word("repeat"),
            pair(cs(opt(word("individually"))), |i| unary(full, i))
                .map(|(a, b)| b.map(|b| (a, b)))
                .map_ok(|(i, r)| RelationExpr::Repeat {
                    individual: i.is_some(),
                    rel: Box::new(r),
                }),
        ),
        preceded(pair(word("from"), cs(tag(""))), |i| unary(full, i))
            .map_ok(Box::new)
            .map_ok(RelationExpr::From),
        |i| atom(full, i),
    ))(input)
}

fn atom<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RelationExpr<'a>> {
    alt((
        span(full, relation_name)
            .map(RelationExpr::Reference)
            .map(Ok),
        (|i| data(full, i)).map_ok(RelationExpr::Data),
        set_constructor.map_ok(RelationExpr::Set),
        relation_constructor.map_ok(RelationExpr::Constructor),
        tag("...").map(|_| Ok(RelationExpr::Extension)),
        delimited(char('('), cs(|i| relation_expr(full, i)), char(')')),
    ))(input)
}

// TODO: Support Ntgr
fn set_constructor(input: &str) -> ParseResult<SetCtor> {
    delimited(
        char('{'),
        fold_separated_list0_accumulate(
            char(',').map(Ok),
            cs(pair(item, opt(preceded(cs(char(':')), u32)))).map(|(i, c)| i.map(|i| (i, c))),
            || Ok(HashMap::new()),
            |mut m, (i, c)| {
                *m.entry(i).or_insert(0) += c.unwrap_or(1);
                m
            },
        ),
        pair(opt(cs(char(','))), char('}')),
    )
    .map_ok(SetCtor)
    .parse(input)
}

fn relation_constructor(input: &str) -> ParseResult<RelationCtor> {
    delimited(
        char('{'),
        fold_separated_list0_accumulate(
            char(',').map(Ok),
            cs(pair(
                separated_pair(item, tag("->"), item),
                opt(preceded(cs(char(':')), u32)),
            ))
            .map(|((a, b), c)| (merge_tuple(a, b), c))
            .map(|(i, c)| i.map(|i| (i, c))),
            || Ok(HashMap::new()),
            |mut m, (i, c)| {
                *m.entry(i).or_insert(0) += c.unwrap_or(1);
                m
            },
        ),
        pair(opt(cs(char(','))), char('}')),
    )
    .map_ok(RelationCtor)
    .parse(input)
}

fn data<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, DataExpr<'a>> {
    todo!()
}

fn word<'a, E: NomParseError<&'a str> + 'a>(
    val: &'a str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> + '_ {
    terminated(tag(val), not(alpha1))
}

fn item(input: &str) -> ParseResult<Value> {
    alt((
        full_ident.map(Value::FullIdent),
        tag("()").map(|_| Value::Unit),
    ))
    .map(Ok)
    .parse(input)
}
