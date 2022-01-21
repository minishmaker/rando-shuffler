use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{
        alpha1, char, line_ending, multispace1, not_line_ending, one_of, space0, u32,
    },
    combinator::{all_consuming, eof, not, opt, peek, value},
    error::{ErrorKind, FromExternalError, ParseError as NomParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Err, Finish, IResult, Parser,
};

use crate::{
    common::{
        error::{
            fold_many0_accumulate, separated_list1_accumulate, vec_merge, ParseError, ParseExt,
        },
        parser::{full_ident, ident_part, keyword, relation_name},
        span::{span, Span},
    },
    Ident,
};

use super::{
    ast::{
        Oolean, Reference, Relation, RuleBody, RuleBodyCounty, RuleBodyTruthy, RuleDef, StateBody,
        Value,
    },
    typecheck::{self, Typecheck},
    DescriptorError,
};

#[cfg(test)]
mod test;

type ParseResult<'a, T> = IResult<&'a str, T, ParseError<'a, DescriptorError<'a>>>;

pub fn rules<'a>(
    full: &'a str,
) -> Result<HashMap<&str, Vec<RuleDef>>, ParseError<DescriptorError>> {
    let mut map = Some(Ok(HashMap::new()));
    all_consuming(fold_many0_accumulate(
        cs(|input| rule(full, input)),
        move || map.take().unwrap(),
        |mut m, r| {
            let keyword = r.reference.keyword.inner();
            m.entry(keyword).or_insert(Vec::new()).push(r);
            m
        },
    ))(full)
    .finish()
    .map(|(_, m)| m)
}

fn rule<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleDef<'a>> {
    (|input| reference(full, input))
        .then_accumulate(delimited(
            cs(char('=')),
            |input| expr(full, input),
            preceded(space0, alt((line_ending, eof))),
        ))
        .map(|(reference, body)| RuleDef { reference, body })
        .parse(input)
}

fn expr<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    separated_list1_accumulate(cs(char(';')), |input| and::<T>(full, input))
        .map(|mut v| {
            if v.len() == 1 {
                v.swap_remove(0)
            } else {
                T::or(v)
            }
        })
        .parse(input)
}

fn and<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    separated_list1_accumulate(cs(char(',')), |input| compare::<T>(full, input))
        .map(|mut v| {
            if v.len() == 1 {
                v.swap_remove(0)
            } else {
                T::and(v)
            }
        })
        .parse(input)
}

fn compare<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    span(full, |input| comb_start::<T>(full, input))
        .then_accumulate(opt(preceded(cs(tag(">=")), u32)))
        .parse(input)
        .and_then(|(i, (t, c))| match c {
            Some(n) => {
                let r = t.range();
                typecheck::<RuleBodyCounty>((i, t.map(T::to_rule_body)))
                    .map(|(i, t)| (i, RuleBody::Truthy(RuleBodyTruthy::Compare(Box::new(t), n))))
                    .map(|(i, t)| (i, Span(r.start, t, r.end)))
                    .and_then(typecheck::<T>)
            }
            None => Ok((i, t.inner())),
        })
}

fn comb_start<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    let mut initial = (span(full, |input| item::<T>(full, input))).then_accumulate(pair(
        opt(preceded(cs(char('*')), u32)),
        opt(peek(cs(char('+')))),
    ));
    let (input, result) = match initial.parse(input) {
        // Without * or +, just pass the value along
        Ok((input, (item, (None, None)))) => return Ok((input, item.inner())),
        // Typecheck and start linear combination
        Ok((input, (item, (mult, _)))) => {
            (input, Ok((item.map(T::to_rule_body), mult.unwrap_or(1))))
        }
        Err(Err::Failure(f)) => (f.remaining(), Err(f)),
        Err(e) => return Err(e),
    };

    county_comb(full, input, result).and_then(typecheck::<T>)
}

fn county_comb<'a>(
    full: &'a str,
    input: &'a str,
    first: Result<(Span<RuleBody<'a>>, u32), ParseError<'a, DescriptorError<'a>>>,
) -> ParseResult<'a, Span<RuleBody<'a>>> {
    let mut start = 0;
    let first = first.and_then(|(b, m)| {
        start = b.0;
        typecheck::<RuleBodyCounty>((input, b))
            .finish()
            .map(|(_, c)| vec![(c, m)])
    });
    let mut first = Some(first);
    span(
        full,
        fold_many0_accumulate(
            preceded(
                cs(char('+')),
                (|input| item(full, input))
                    .then_accumulate(opt(preceded(cs(char('*')), u32)))
                    .map(|(i, v)| (i, v.unwrap_or(1))),
            ),
            move || first.take().unwrap(),
            vec_merge,
        ),
    )
    .map(|c| c.map(RuleBodyCounty::LinearComb))
    .map(|c| c.map(RuleBody::County))
    .parse(input)
}

fn item<'a, T: Typecheck<'a>>(full: &'a str, input: &'a str) -> ParseResult<'a, T> {
    span(
        full,
        alt((
            delimited(char('('), cs(|input| expr::<T>(full, input)), char(')'))
                .map(T::to_rule_body),
            (|input| truthy_item(full, input)).map(RuleBody::Truthy),
            (|input| county_item(full, input)).map(RuleBody::County),
            (|input| reference(full, input))
                .map(T::reference)
                .map(T::to_rule_body),
        )),
    )(input)
    .and_then(typecheck::<T>)
}

fn county_item<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyCounty<'a>> {
    alt((u32.map(RuleBodyCounty::Constant), |input| {
        count(full, input)
    }))(input)
}

fn truthy_item<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    alt((
        oolean.map(RuleBodyTruthy::Constant),
        |input| access(full, input),
        |input| exists(full, input),
        |input| state_check(full, input),
    ))(input)
}

fn exists<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    preceded(
        one_of("?∃"),
        tuple((
            cs(span(full, variable)),
            |input| relation(full, input),
            cs(span(full, value_name)),
            delimited(char('('), |input| expr(full, input), char(')')),
        )),
    )
    .map(|(var, r, val, rule)| RuleBodyTruthy::Exists(var, r, val, Box::new(rule)))
    .parse(input)
}

fn count<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyCounty<'a>> {
    preceded(
        char('+'),
        tuple((
            cs(span(full, variable)),
            |input| relation(full, input),
            cs(span(full, value_name)),
            delimited(char('('), |input| expr(full, input), char(')')),
        )),
    )
    .map(|(var, r, val, rule)| RuleBodyCounty::Count(var, r, val, Box::new(rule)))
    .parse(input)
}

fn relation<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, Relation<'a>> {
    alt((
        delimited(tag("<-"), cs(span(full, relation_name)), char('-')).map(|s| (s, false)),
        delimited(char('-'), cs(span(full, relation_name)), tag("->")).map(|s| (s, true)),
    ))
    .map(|(rel, ltr)| Relation {
        name: rel,
        left_to_right: ltr,
    })
    .parse(input)
}

fn state_check<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    pair(
        one_of("?!"),
        delimited(
            char('['),
            separated_list0(char(','), cs(span(full, state_body))),
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

fn access<'a>(full: &'a str, input: &'a str) -> ParseResult<'a, RuleBodyTruthy<'a>> {
    delimited(char('['), cs(|input| reference(full, input)), char(']'))
        .map(RuleBodyTruthy::Access)
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
    )(input)
}

fn state_body(input: &str) -> ParseResult<StateBody> {
    pair(opt(one_of("~¬")), value_name)
        .map(|(n, v)| {
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
            separated_list0(char(','), cs(span(full, value_name))),
            char(')'),
        ),
    )
    .map(|(keyword, values)| Reference { keyword, values })
    .parse(input)
}

fn value_name(input: &str) -> ParseResult<Value> {
    alt((variable.map(Value::Var), full_ident.map(Value::Const)))(input)
}

fn variable(input: &str) -> ParseResult<Ident> {
    preceded(char('v'), ident_part)(input)
}

fn typecheck<'a, T: Typecheck<'a>>((i, t): (&'a str, Span<RuleBody<'a>>)) -> ParseResult<'a, T> {
    typecheck::typecheck(t)
        .map(|t| (i, t))
        .map_err(|e| ParseError::from_external_error(i, ErrorKind::Verify, e))
        .map_err(|e| e.to_recoverable())
        .map_err(Err::Failure)
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
