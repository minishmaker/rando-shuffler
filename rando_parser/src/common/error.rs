use std::{convert::identity, marker::PhantomData, ops::Range};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use nom::{
    bytes::complete::take_while,
    error::{
        ContextError, ErrorKind as NomErrorKind, FromExternalError, ParseError as NomParseError,
    },
    multi::fold_many0,
    sequence::pair,
    Err as NomErr, IResult, Parser,
};

use super::{
    parser::ls,
    span::{self, span},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CommonError<'a> {
    ExpectedKeyword(&'a str),
    ExpectedIdent(&'a str),
}

impl CommonError<'_> {
    fn diagnostic_message(&self) -> String {
        match self {
            Self::ExpectedKeyword(i) => format!("Expected keyword, found identifier \"{}\"", i),
            Self::ExpectedIdent(k) => format!("Expected identifier, found keyword \"{}\"", k),
        }
    }

    fn diagnostic_notes(&self) -> Vec<String> {
        match self {
            Self::ExpectedKeyword(_) => {
                vec!["Keywords must begin with a lowercase letter".into()]
            }
            Self::ExpectedIdent(_) => vec![
                "Identifiers cannot start with lowercase letters unless enclosed in quotes (\")"
                    .into(),
            ],
        }
    }

    fn range(&self, input: &str) -> Range<usize> {
        let (Self::ExpectedIdent(i) | Self::ExpectedKeyword(i)) = self;
        let start =
            span::substr_index(input, i).expect("Input source is different from provided input");
        start..start + i.len()
    }

    pub fn diagnostic<F: Clone>(&self, file: &F, input: &str) -> Diagnostic<F> {
        Diagnostic::error()
            .with_labels(vec![Label::primary(file.clone(), self.range(input))])
            .with_message(self.diagnostic_message())
            .with_notes(self.diagnostic_notes())
    }
}

pub trait RandoError<'a> {
    fn from_common(common: CommonError<'a>) -> Self;
    fn diagnostic<F: Clone>(&self, file: &F, input: &str) -> Diagnostic<F>;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ParseError<'a, E: RandoError<'a>> {
    custom: Vec<E>,
    nom: Option<Result<char, NomErrorKind>>,
    remaining: &'a str,
    context: Option<&'static str>,
}

impl<'a, E: RandoError<'a>> ParseError<'a, E> {
    pub fn custom(&self) -> &[E] {
        &self.custom[..]
    }

    pub fn diagnostics<'b, F: Clone>(
        &'b self,
        file: &'b F,
        input: &'a str,
    ) -> impl Iterator<Item = Diagnostic<F>> + 'b {
        self.custom
            .iter()
            .map(|e| e.diagnostic(file, input))
            .chain(self.nom_diagnostic(file, input).into_iter())
    }

    fn nom_diagnostic<F: Clone>(&self, file: &F, input: &str) -> Option<Diagnostic<F>> {
        if let Some(d) = self.nom {
            let actual = ls(span(input, take_while(|c: char| !c.is_whitespace())))(self.remaining)
                .map(|(_, n)| n)
                .map_err(|u: NomErr<()>| u)
                .unwrap();

            let message = if self.remaining.is_empty() {
                "Unexpected end of input".into()
            } else {
                format!("Unexpected error at \"{}\"", actual.1)
            };

            let labels = vec![Label::primary(file.clone(), actual.range())];

            let notes = vec![format!("Nom kind: {:?}", d)];

            Some(
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes),
            )
        } else {
            None
        }
    }
}

impl<'a, E: RandoError<'a>> NomParseError<&'a str> for ParseError<'a, E> {
    fn from_error_kind(input: &'a str, kind: NomErrorKind) -> Self {
        Self {
            custom: Vec::new(),
            nom: Some(Err(kind)),
            context: None,
            remaining: input,
        }
    }

    fn append(input: &'a str, kind: NomErrorKind, other: Self) -> Self {
        Self {
            nom: other.nom.or(Some(Err(kind))),
            remaining: input,
            ..other
        }
    }

    fn from_char(input: &'a str, c: char) -> Self {
        Self {
            custom: Vec::new(),
            nom: Some(Ok(c)),
            remaining: input,
            context: None,
        }
    }
}

impl<'a, E: RandoError<'a>> ContextError<&'a str> for ParseError<'a, E> {
    fn add_context(_input: &'a str, ctx: &'static str, mut other: Self) -> Self {
        other.context.get_or_insert(ctx);
        other
    }
}

impl<'a, E: RandoError<'a>> FromExternalError<&'a str, E> for ParseError<'a, E> {
    fn from_external_error(input: &'a str, _: NomErrorKind, kind: E) -> Self {
        Self {
            custom: vec![kind],
            remaining: input,
            nom: None,
            context: None,
        }
    }
}

impl<'a, E: RandoError<'a>> FromExternalError<&'a str, CommonError<'a>> for ParseError<'a, E> {
    fn from_external_error(input: &'a str, _: NomErrorKind, common: CommonError<'a>) -> Self {
        Self {
            custom: vec![E::from_common(common)],
            remaining: input,
            nom: None,
            context: None,
        }
    }
}

impl<'a, E: RandoError<'a>> From<Vec<E>> for ParseError<'a, E> {
    fn from(custom: Vec<E>) -> Self {
        Self {
            custom,
            remaining: "",
            nom: None,
            context: None,
        }
    }
}

pub struct MapOk<P, F, T, U>(P, F, PhantomData<T>, PhantomData<U>)
where
    F: FnMut(T) -> U;

impl<'a, P, F, T, U, E> Parser<&'a str, Result<U, Vec<E>>, ParseError<'a, E>> for MapOk<P, F, T, U>
where
    E: RandoError<'a>,
    P: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
    F: FnMut(T) -> U,
{
    fn parse(&mut self, input: &'a str) -> IResult<&'a str, Result<U, Vec<E>>, ParseError<'a, E>> {
        self.0.parse(input).map(|(i, r)| (i, r.map(&mut self.1)))
    }
}

pub trait ParseExt<'a, T, E: RandoError<'a>>:
    Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>
{
    fn map_ok<F, U>(self, f: F) -> MapOk<Self, F, T, U>
    where
        Self: Sized,
        F: FnMut(T) -> U,
    {
        MapOk(self, f, PhantomData, PhantomData)
    }
}

impl<'a, P, T, E: RandoError<'a>> ParseExt<'a, T, E> for P where
    P: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>
{
}

pub fn vec_merge<T>(mut acc: Vec<T>, t: T) -> Vec<T> {
    acc.push(t);
    acc
}

pub fn accumulate_errors<A, T, B, E>(
    acc: Result<A, B>,
    result: Result<T, E>,
    merge_ok: impl FnOnce(A, T) -> A,
    merge_err: impl FnOnce(B, E) -> B,
    init_err: impl FnOnce(E) -> B,
) -> Result<A, B> {
    match result {
        Ok(t) => acc.map(move |acc| merge_ok(acc, t)),
        Err(e) => {
            if acc.is_ok() {
                Err(init_err(e))
            } else {
                acc.map_err(move |acc| merge_err(acc, e))
            }
        }
    }
}

pub fn merge_tuple<T, U, E>(
    first: Result<T, Vec<E>>,
    second: Result<U, Vec<E>>,
) -> Result<(T, U), Vec<E>> {
    match (first, second) {
        (Ok(t), Ok(u)) => Ok((t, u)),
        (Err(mut t), Err(mut u)) => {
            t.append(&mut u);
            Err(t)
        }
        (Err(e), _) | (_, Err(e)) => Err(e),
    }
}

pub fn fold_many0_accumulate<'a, A, T, E, F, G, H>(
    mut f: F,
    mut init: H,
    mut g: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<A, Vec<E>>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
    G: FnMut(A, T) -> A,
    H: FnMut() -> Result<A, Vec<E>>,
{
    move |i| accumulate_inner(|i| f.parse(i), init(), &mut g, i)
}

pub fn many0_accumulate<'a, T, E, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<Vec<T>, Vec<E>>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
{
    move |i| accumulate_inner(|i| f.parse(i), Ok(Vec::new()), vec_merge, i)
}

pub fn many1_accumulate<'a, T, E, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<Vec<T>, Vec<E>>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
{
    move |i| match f.parse(i) {
        Err(NomErr::Error(_)) => Err(NomErr::Error(ParseError::from_error_kind(
            i,
            NomErrorKind::Many1,
        ))),
        Err(e) => Err(e),
        Ok((i, first)) => {
            let acc = first.map(|f| vec![f]);
            accumulate_inner(|i| f.parse(i), acc, vec_merge, i)
        }
    }
}

pub fn separated_list0_accumulate<'a, T, O, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<Vec<T>, Vec<E>>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
    G: Parser<&'a str, Result<O, Vec<E>>, ParseError<'a, E>>,
{
    move |i| match f.parse(i) {
        Err(NomErr::Error(_)) => Ok((i, Ok(Vec::new()))),
        Err(e) => Err(e),
        Ok((i, first)) => {
            let acc = first.map(|f| vec![f]);
            let parser = pair(|i| sep.parse(i), |i| f.parse(i))
                .map(|(a, b)| merge_tuple(a, b))
                .map_ok(|(_, b)| b);
            accumulate_inner(parser, acc, vec_merge, i)
        }
    }
}

pub fn separated_list1_accumulate<'a, T, O, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<Vec<T>, Vec<E>>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
    G: Parser<&'a str, Result<O, Vec<E>>, ParseError<'a, E>>,
{
    move |i| match f.parse(i) {
        Err(NomErr::Error(_)) => Err(NomErr::Error(ParseError::from_error_kind(
            i,
            NomErrorKind::SeparatedList,
        ))),
        Err(e) => Err(e),
        Ok((i, first)) => {
            let acc = first.map(|f| vec![f]);
            let parser = pair(|i| sep.parse(i), |i| f.parse(i))
                .map(|(a, b)| merge_tuple(a, b))
                .map_ok(|(_, b)| b);
            accumulate_inner(parser, acc, vec_merge, i)
        }
    }
}

fn accumulate_inner<'a, A, T, E, F, G>(
    f: F,
    acc: Result<A, Vec<E>>,
    mut merge: G,
    i: &'a str,
) -> IResult<&'a str, Result<A, Vec<E>>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, Result<T, Vec<E>>, ParseError<'a, E>>,
    G: FnMut(A, T) -> A,
{
    let mut acc = Some(acc);
    fold_many0(
        f,
        move || acc.take().unwrap(),
        |acc, result| {
            accumulate_errors(
                acc,
                result,
                &mut merge,
                move |mut a, mut b| {
                    a.append(&mut b);
                    a
                },
                identity,
            )
        },
    )(i)
}
