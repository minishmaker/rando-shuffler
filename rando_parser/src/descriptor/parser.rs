use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, one_of, u32},
    combinator::{not, opt, value},
    multi::{separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};

use crate::{
    common::{full_ident, ident_part, keyword, ls, relation_name, span},
    Ident,
};

use super::ast::{
    Oolean, Reference, Relation, RuleBody, RuleBodyCounty, RuleBodyTruthy, StateBody, Value,
};

fn county_max<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyCounty<'a>> {
    separated_list1(char(';'), |input| county_min(full, input))
        .map(|mut l| {
            if l.len() == 1 {
                l.swap_remove(0)
            } else {
                RuleBodyCounty::Max(l)
            }
        })
        .parse(input)
}

fn county_min<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyCounty<'a>> {
    separated_list1(char(','), |input| county_comb(full, input))
        .map(|mut l| {
            if l.len() == 1 {
                l.swap_remove(0)
            } else {
                RuleBodyCounty::Min(l)
            }
        })
        .parse(input)
}

fn county_comb<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyCounty<'a>> {
    separated_list1(
        char('+'),
        pair(
            |input| county_item(full, input),
            opt(preceded(char('*'), u32)),
        ),
    )
    .map(|mut l| {
        if l.len() == 0 && !l[0].1.is_some() {
            l.swap_remove(0).0
        } else {
            RuleBodyCounty::LinearComb(l.into_iter().map(|(i, m)| (i, m.unwrap_or(0))).collect())
        }
    })
    .parse(input)
}

fn county_item<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyCounty<'a>> {
    alt((
        u32.map(RuleBodyCounty::Constant),
        (|input| reference(full, input)).map(RuleBodyCounty::Reference),
        delimited(char('('), ls(|input| county_max(full, input)), char(')')),
    ))(input)
}

fn truthy_or<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    separated_list1(char(';'), |input| truthy_and(full, input))
        .map(|mut l| {
            if l.len() == 1 {
                l.swap_remove(0)
            } else {
                RuleBodyTruthy::Or(l)
            }
        })
        .parse(input)
}

fn truthy_and<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    separated_list1(char(','), |input| truthy_item(full, input))
        .map(|mut l| {
            if l.len() == 1 {
                l.swap_remove(0)
            } else {
                RuleBodyTruthy::And(l)
            }
        })
        .parse(input)
}

fn truthy_item<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    alt((
        oolean.map(RuleBodyTruthy::Constant),
        (|input| reference(full, input)).map(RuleBodyTruthy::Reference),
        |input| access(full, input),
        delimited(char('('), ls(|input| truthy_or(full, input)), char(')')),
    ))(input)
}

fn compare<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    separated_pair(|input| county_item(full, input), tag(">="), u32)
        .map(|(rule, val)| RuleBodyTruthy::Compare(Box::new(rule), val))
        .parse(input)
}

fn exists<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    preceded(
        one_of("?∃"),
        tuple((
            ls(span(full, variable)),
            |input| relation(full, input),
            ls(span(full, value_name)),
            delimited(char('('), |input| truthy_or(full, input), char(')')),
        )),
    )
    .map(|(var, r, val, rule)| RuleBodyTruthy::Exists(var, r, val, Box::new(rule)))
    .parse(input)
}

fn count<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyCounty<'a>> {
    preceded(
        char('+'),
        tuple((
            ls(span(full, variable)),
            |input| relation(full, input),
            ls(span(full, value_name)),
            delimited(char('('), |input| truthy_and(full, input), char(')')),
        )),
    )
    .map(|(var, r, val, rule)| RuleBodyCounty::Count(var, r, val, Box::new(rule)))
    .parse(input)
}

fn relation<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, Relation<'a>> {
    alt((
        delimited(tag("<-"), span(full, relation_name), char('-')).map(|s| (s, false)),
        delimited(char('-'), span(full, relation_name), tag("->")).map(|s| (s, true)),
    ))
    .map(|(rel, ltr)| Relation {
        name: rel,
        left_to_right: ltr,
    })
    .parse(input)
}

fn state_check<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    pair(
        one_of("?!"),
        delimited(
            char('['),
            separated_list0(char(','), ls(span(full, state_body))),
            char(']'),
        ),
    )
    .map(|(op, body)| {
        if op == '?' {
            RuleBodyTruthy::CheckPrior(body)
        } else {
            RuleBodyTruthy::CheckPosterior(body)
        }
    })
    .parse(input)
}

fn access<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, RuleBodyTruthy<'a>> {
    delimited(char('['), ls(|input| reference(full, input)), char(']'))
        .map(RuleBodyTruthy::Access)
        .parse(input)
}

fn oolean(input: &str) -> IResult<&str, Oolean> {
    terminated(
        alt((
            value(Oolean::True, tag("true")),
            value(Oolean::Ool, tag("ool")),
            value(Oolean::False, tag("false")),
        )),
        not(alpha1),
    )(input)
}

fn state_body(input: &str) -> IResult<&str, StateBody> {
    pair(opt(one_of("~¬")), value_name)
        .map(|(n, v)| {
            if n.is_some() {
                StateBody::Set(v)
            } else {
                StateBody::NotSet(v)
            }
        })
        .parse(input)
}

fn reference<'a>(full: &'a str, input: &'a str) -> IResult<&'a str, Reference<'a>> {
    pair(
        span(full, keyword),
        preceded(
            char('('),
            pair(
                separated_list0(char(','), ls(span(full, value_name))),
                span(full, char(')')), // Included to get the end of the reference
            ),
        ),
    )
    .map(|(keyword, (values, (.., end)))| Reference {
        keyword,
        values,
        end,
    })
    .parse(input)
}

fn value_name(input: &str) -> IResult<&str, Value> {
    alt((variable.map(Value::Var), full_ident.map(Value::Const)))(input)
}

fn variable(input: &str) -> IResult<&str, Ident> {
    preceded(char('v'), ident_part)(input)
}

fn typecheck_truthy(rule: RuleBody) -> Result<RuleBodyTruthy, RuleBodyCounty> {
    todo!()
}
