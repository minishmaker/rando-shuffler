use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{
        alpha1, char, line_ending, multispace1, not_line_ending, one_of, space0, u32,
    },
    combinator::{all_consuming, eof, not, opt, value},
    error::ParseError as NomParseError,
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, Parser,
};
use rando_core::algebra::{Ntgr, Oolean};

use crate::{
    common::{
        error::{
            fold_many0_accumulate, merge_tuple, separated_list0_accumulate,
            separated_list1_accumulate, ParseError, ParseExt,
        },
        parser::{full_ident, ident_part, keyword, relation_name},
        span::{span, span_ok, Span},
    },
    Ident,
};

use super::{
    ast::*,
    typecheck::{typecheck, Typecheck},
};

#[cfg(test)]
mod test;

type ParseResult<'a, T> =
    IResult<&'a str, Result<T, Vec<DescriptorError<'a>>>, ParseError<'a, DescriptorError<'a>>>;

pub fn rules(
    full: &str,
) -> Result<HashMap<&str, Vec<RuleDefUntyped>>, ParseError<DescriptorError>> {
    let mut map = Some(Ok(HashMap::new()));
    all_consuming(fold_many0_accumulate(
        cs(|input| rule(full, input)),
        move || map.take().unwrap(),
        |mut m, r| {
            let keyword = r.reference.keyword.inner();
            m.entry(keyword).or_insert_with(Vec::new).push(r);
            m
        },
    ))(full)
    .finish()
    .map(|(_, e)| e.map_err(Into::into))?
}

fn rule<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleDefUntyped<'a>> {
    pair(
        |input| reference(full, input),
        delimited(
            cs(char('=')),
            |input| expr(full, input),
            preceded(space0, alt((line_ending, eof))),
        ),
    )
    .map(|(a, b)| merge_tuple(a, b))
    .map_ok(|(reference, body)| RuleDefUntyped { reference, body })
    .parse(input)
}

fn expr<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    separated_list1_accumulate(cs(char(';')).map(Ok), |input| and::<T>(full, input))
        .map_ok(|mut v| {
            if v.len() == 1 {
                v.swap_remove(0)
            } else {
                T::or(v)
            }
        })
        .parse(input)
}

fn and<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    separated_list1_accumulate(cs(char(',')).map(Ok), |input| compare::<T>(full, input))
        .map_ok(|mut v| {
            if v.len() == 1 {
                v.swap_remove(0)
            } else {
                T::and(v)
            }
        })
        .parse(input)
}

fn compare<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    pair(
        |input| comb_start::<T>(full, input),
        opt(preceded(cs(tag(">=")), ntgr)),
    )
    .parse(input)
    .and_then(|(i, (t, c))| match c {
        Some(_) => {
            let reparse = pair(
                |input| comb_start::<RuleBodyCounty>(full, input),
                preceded(cs(tag(">=")), ntgr),
            )
            .map(|(c, n)| c.map(|c| (c, n.unwrap())))
            .map_ok(|(c, n)| RuleBodyTruthy::Compare(Box::new(c), n))
            .map_ok(RuleBodyUntyped::Truthy);

            span_ok(full, reparse)
                .map(|r| r.and_then(typecheck::<T>))
                .parse(input)
        }
        None => Ok((i, t)),
    })
}

fn comb_start<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    let mut initial = pair(
        span_ok(full, |input| item::<T>(full, input)),
        opt(cs(one_of("*+"))),
    );

    match initial.parse(input) {
        // Without * or +, just pass the value along
        Ok((input, (item, None))) => Ok((input, item.map(Span::inner))),
        // Restart parsing for a linear combination
        Ok(_) => county_comb(full, input).map(|(i, r)| (i, r.and_then(typecheck::<T>))),
        Err(e) => Err(e),
    }
}

fn county_comb<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Span<RuleBodyUntyped<'a>>> {
    span_ok(
        full,
        separated_list1_accumulate(
            cs(char('+')).map(Ok),
            pair(
                |input| item(full, input),
                opt(preceded(cs(char('*')), ntgr)),
            )
            .map(|(i, v)| i.map(|i| (i, v.unwrap_or(Ok(Ntgr::Num(1))).unwrap()))),
        ),
    )
    .map_ok(|c| c.map(RuleBodyCounty::LinearComb))
    .map_ok(|c| c.map(RuleBodyUntyped::County))
    .parse(input)
}

fn item<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    span_ok(
        full,
        alt((
            delimited(char('('), cs(|input| expr::<T>(full, input)), char(')'))
                .map_ok(T::to_rule_body),
            (|input| truthy_item(full, input)).map_ok(RuleBodyUntyped::Truthy),
            (|input| county_item(full, input)).map_ok(RuleBodyUntyped::County),
            (|input| reference(full, input))
                .map_ok(T::reference)
                .map_ok(T::to_rule_body),
        )),
    )
    .map(|r| r.and_then(typecheck::<T>))
    .parse(input)
}

fn county_item<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyCounty<'a>> {
    alt((
        ntgr.map(Result::unwrap)
            .map(RuleBodyCounty::Constant)
            .map(Ok),
        |input| count(full, input),
    ))(input)
}

fn truthy_item<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    alt((
        oolean.map_ok(RuleBodyTruthy::Constant),
        |input| access(full, input),
        |input| exists(full, input),
        |input| state_check(full, input),
    ))(input)
}

fn exists<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    preceded(
        one_of("?∃"),
        tuple((
            cs(span_ok(full, value_name)),
            |input| relation(full, input),
            cs(span_ok(full, value_name)),
            delimited(char('('), |input| expr(full, input), char(')')),
        )),
    )
    .map(|(a, r, b, rule)| merge_tuple(merge_tuple(a, r), merge_tuple(b, rule)))
    .map_ok(|((a, r), (b, rule))| RuleBodyTruthy::Exists(a, r, b, Box::new(rule)))
    .parse(input)
}

fn count<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyCounty<'a>> {
    preceded(
        char('+'),
        tuple((
            cs(span_ok(full, value_name)),
            |input| relation(full, input),
            cs(span_ok(full, value_name)),
            delimited(char('('), |input| expr(full, input), char(')')),
        )),
    )
    .map(|(var, r, val, rule)| merge_tuple(merge_tuple(var, r), merge_tuple(val, rule)))
    .map_ok(|((var, r), (val, rule))| RuleBodyCounty::Count(var, r, val, Box::new(rule)))
    .parse(input)
}

fn relation<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Relation<'a>> {
    alt((
        delimited(tag("<-"), cs(span(full, relation_name)), char('-')).map(|s| (s, false)),
        delimited(char('-'), cs(span(full, relation_name)), tag("->")).map(|s| (s, true)),
    ))
    .map(|(rel, ltr)| {
        Ok(Relation {
            name: rel,
            left_to_right: ltr,
        })
    })
    .parse(input)
}

fn state_check<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    pair(
        one_of("?!"),
        delimited(
            char('['),
            separated_list0_accumulate(char(',').map(Ok), cs(span_ok(full, state_body))),
            char(']'),
        ),
    )
    .map(|(a, b)| b.map(|b| (a, b)))
    .map_ok(|(op, body)| {
        if op == '?' {
            RuleBodyTruthy::CheckPrior(body)
        } else {
            RuleBodyTruthy::CheckPosterior(body)
        }
    })
    .parse(input)
}

fn access<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    delimited(char('['), cs(|input| reference(full, input)), char(']'))
        .map_ok(RuleBodyTruthy::Access)
        .parse(input)
}

fn oolean(input: &str) -> ParseResult<Oolean> {
    terminated(
        alt((
            value(Oolean::True, tag("true")),
            value(Oolean::Ool, tag("ool")),
            value(Oolean::False, tag("false")),
        )),
        not(alpha1),
    )
    .map(Ok)
    .parse(input)
}

fn ntgr(input: &str) -> ParseResult<Ntgr> {
    alt((tag("inf").map(|_| Ntgr::Infinity), u32.map(Ntgr::Num)))
        .map(Ok)
        .parse(input)
}

fn state_body(input: &str) -> ParseResult<StateBody> {
    pair(opt(one_of("~¬")), value_name)
        .map(|(n, v)| v.map(|v| (n, v)))
        .map_ok(|(n, v)| {
            if n.is_some() {
                StateBody::NotSet(v)
            } else {
                StateBody::Set(v)
            }
        })
        .parse(input)
}

fn reference<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Reference<'a>> {
    pair(
        span(full, keyword),
        delimited(
            cs(char('(')),
            separated_list0_accumulate(char(',').map(Ok), cs(span_ok(full, value_name))),
            char(')'),
        ),
    )
    .map(|(keyword, values)| values.map(|v| (keyword, v)))
    .map_ok(|(keyword, values)| Reference { keyword, values })
    .parse(input)
}

fn value_name(input: &str) -> ParseResult<Value> {
    alt((
        variable.map_ok(Value::Var),
        full_ident.map(Value::Const).map(Ok),
    ))
    .parse(input)
}

fn variable(input: &str) -> ParseResult<Ident> {
    preceded(char('v'), ident_part).map(Ok).parse(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace and comments, returning the output of `inner`.
pub fn cs<'a: 'b, 'b, F: 'b, O, E: NomParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    fn comment<'a, E: NomParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
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
