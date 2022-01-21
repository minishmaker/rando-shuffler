use std::{convert::identity, ops::Range};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use nom::{
    bytes::complete::take_while,
    error::{
        ContextError, ErrorKind as NomErrorKind, FromExternalError, ParseError as NomParseError,
    },
    sequence::preceded,
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
    recoverable: bool,
}

impl<'a, E: RandoError<'a>> ParseError<'a, E> {
    /// Merges two parse errors. Takes the custom errors from both.
    /// The error is recoverable if both errors are recoverable.
    /// Otherwise, `other`'s fields take priority (as it is expected to be later)
    fn merge(mut self, mut other: Self) -> Self {
        self.custom.append(&mut other.custom);
        Self {
            custom: self.custom,
            nom: other.nom.or(self.nom),
            context: other.context.or(self.context),
            recoverable: self.recoverable && other.recoverable,
            remaining: other.remaining,
        }
    }

    pub fn to_recoverable(self) -> Self {
        Self {
            recoverable: !self.nom.is_some(),
            ..self
        }
    }

    pub fn recoverable(&self) -> bool {
        self.recoverable
    }

    pub fn remaining(&self) -> &'a str {
        self.remaining
    }

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
                format!("Unexpected end of input")
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
            recoverable: false,
        }
    }

    fn append(input: &'a str, kind: NomErrorKind, other: Self) -> Self {
        Self {
            nom: other.nom.or(Some(Err(kind))),
            recoverable: false,
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
            recoverable: false,
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
            recoverable: false,
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
            recoverable: false,
        }
    }
}

pub struct ThenAccumulate<P, Q> {
    p: P,
    q: Q,
}

impl<'a, P, Q, T, O, E> Parser<&'a str, (T, O), ParseError<'a, E>> for ThenAccumulate<P, Q>
where
    E: RandoError<'a>,
    P: Parser<&'a str, T, ParseError<'a, E>>,
    Q: Parser<&'a str, O, ParseError<'a, E>>,
{
    fn parse(&mut self, input: &'a str) -> IResult<&'a str, (T, O), ParseError<'a, E>> {
        let (input, res1) = self.p.parse_accumulate(input)?;
        let (input, res2) = self.q.parse_accumulate(input)?;
        match (res1, res2) {
            (Ok(t), Ok(o)) => Ok((input, (t, o))),
            (Err(e1), Err(e2)) => Err(NomErr::Failure(e1.merge(e2))),
            (Err(e), _) | (_, Err(e)) => Err(NomErr::Failure(e)),
        }
    }
}

pub trait ParseExt<'a, T, E: RandoError<'a>>: Parser<&'a str, T, ParseError<'a, E>> {
    fn parse_accumulate(
        &mut self,
        input: &'a str,
    ) -> IResult<&'a str, Result<T, ParseError<'a, E>>, ParseError<'a, E>> {
        match self.parse(input) {
            Err(NomErr::Failure(f)) => Ok((f.remaining, Err(f))),
            Err(e) => Err(e),
            Ok((i, t)) => Ok((i, Ok(t))),
        }
    }

    fn then_accumulate<Q, O>(self, q: Q) -> ThenAccumulate<Self, Q>
    where
        Self: Sized,
        Q: Parser<&'a str, O, ParseError<'a, E>>,
    {
        ThenAccumulate { p: self, q }
    }
}

impl<'a, P, T, E: RandoError<'a>> ParseExt<'a, T, E> for P where
    P: Parser<&'a str, T, ParseError<'a, E>>
{
}

pub fn vec_merge<T>(mut acc: Vec<T>, t: T) -> Vec<T> {
    acc.push(t);
    acc
}

pub fn accumulate_errors<'a, A, T, B, E>(
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

pub fn recoverable<'a: 'b, 'b, T, E, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, ParseError<'a, E>> + 'b
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>> + 'b,
{
    move |i| {
        f.parse(i).map_err(|mut e| {
            if let NomErr::Error(err) | NomErr::Failure(err) = &mut e {
                err.recoverable = err.nom.is_none();
            }
            e
        })
    }
}

pub fn cut_custom<'a: 'b, 'b, T, E, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, ParseError<'a, E>> + 'b
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>> + 'b,
{
    move |i| {
        f.parse(i).map_err(|e| match e {
            NomErr::Error(e) if e.nom.is_none() => NomErr::Failure(e),
            e => e,
        })
    }
}

pub fn throw<'a: 'b, 'b, T, O, E, F, G>(
    mut f: F,
    mut g: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, ParseError<'a, E>> + 'b
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>> + 'b,
    G: FnMut(T) -> E + 'b,
{
    move |i| match f.parse(i) {
        Ok((i, t)) => Err(NomErr::Error(ParseError::from_external_error(
            i,
            NomErrorKind::MapRes,
            g(t),
        ))),
        Err(e) => Err(e),
    }
}

pub fn fold_many0_accumulate<'a, A, T, E, F, G, H>(
    mut f: F,
    mut init: H,
    mut g: G,
) -> impl FnMut(&'a str) -> IResult<&'a str, A, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>>,
    G: FnMut(A, T) -> A,
    H: FnMut() -> Result<A, ParseError<'a, E>>,
{
    move |i| accumulate_inner(|i| f.parse(i), init(), &mut g, NomErrorKind::Many0, i)
}

pub fn many0_accumulate<'a, T, E, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<T>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>>,
{
    move |i| {
        accumulate_inner(
            |i| f.parse(i),
            Ok(Vec::new()),
            vec_merge,
            NomErrorKind::Many0,
            i,
        )
    }
}

pub fn many1_accumulate<'a, T, E, F>(
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<T>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>>,
{
    move |i| {
        let (input, acc) = match f.parse(i) {
            Err(NomErr::Error(e)) => {
                return Err(NomErr::Error(
                    // Keep any previous semantic errors
                    e.merge(ParseError::from_error_kind(i, NomErrorKind::Many1)),
                ));
            }
            Err(NomErr::Failure(e)) => (e.remaining, Err(e)),
            Err(e) => return Err(e),
            Ok((i, f)) => (i, Ok(vec![f])),
        };

        accumulate_inner(|i| f.parse(i), acc, vec_merge, NomErrorKind::Many1, input)
    }
}

pub fn separated_list0_accumulate<'a, T, O, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<T>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>>,
    G: Parser<&'a str, O, ParseError<'a, E>>,
{
    move |i| {
        let (input, acc) = match f.parse(i) {
            Err(NomErr::Error(_)) => return Ok((i, Vec::new())),
            Err(NomErr::Failure(e)) => (e.remaining, Err(e)),
            Err(e) => return Err(e),
            Ok((i, f)) => (i, Ok(vec![f])),
        };

        let mut f = preceded(|i| sep.parse(i), |i| f.parse(i));
        accumulate_inner(&mut f, acc, vec_merge, NomErrorKind::SeparatedList, input)
    }
}

pub fn separated_list1_accumulate<'a, T, O, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<T>, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>>,
    G: Parser<&'a str, O, ParseError<'a, E>>,
{
    move |i| {
        let (input, acc) = match f.parse(i) {
            Err(NomErr::Error(e)) => {
                return Err(NomErr::Error(
                    // Keep any semantic errors from below
                    e.merge(ParseError::from_error_kind(i, NomErrorKind::SeparatedList)),
                ));
            }
            Err(NomErr::Failure(e)) => (e.remaining, Err(e)),
            Err(e) => return Err(e),
            Ok((i, f)) => (i, Ok(vec![f])),
        };

        accumulate_inner(
            |i| {
                (|i| sep.parse(i))
                    .then_accumulate(|i| f.parse(i))
                    .map(|(_, f)| f)
                    .parse(i)
            },
            acc,
            vec_merge,
            NomErrorKind::SeparatedList,
            input,
        )
    }
}

fn accumulate_inner<'a, A, T, E, F, G>(
    mut f: F,
    mut acc: Result<A, ParseError<'a, E>>,
    mut merge: G,
    kind: NomErrorKind,
    mut i: &'a str,
) -> IResult<&'a str, A, ParseError<'a, E>>
where
    E: RandoError<'a>,
    F: Parser<&'a str, T, ParseError<'a, E>>,
    G: FnMut(A, T) -> A,
{
    loop {
        let (i1, r) = match f.parse(i) {
            Err(NomErr::Error(_)) => return acc.map(|v| (i, v)).map_err(NomErr::Failure),
            Err(NomErr::Failure(f)) => (f.remaining, Err(f)),
            Err(e) => return Err(e),
            Ok((i1, t)) => (i1, Ok(t)),
        };

        if i1.len() == i.len() {
            return Err(NomErr::Error(ParseError::from_error_kind(i, kind)));
        }
        i = i1;

        acc = accumulate_errors(acc, r, &mut merge, ParseError::merge, identity);

        if let Err(ParseError {
            recoverable: false, ..
        }) = acc
        {
            return acc.map(|_| unreachable!()).map_err(NomErr::Failure);
        }
    }
}
