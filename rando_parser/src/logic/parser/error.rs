use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use nom::{
    bytes::complete::take_while,
    error::{
        ContextError, ErrorKind as NomErrorKind, FromExternalError, ParseError as NomParseError,
    },
};

use crate::common::{
    error::CommonError,
    parser::ls,
    span::{self, span, Span},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ParseError<'a> {
    input: &'a str,
    kind: Result<ParseErrorKind<'a>, NomErrorKind>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ParseErrorKind<'a> {
    Common(CommonError),
    Char(char),
    WrongIndent { base: &'a str, actual: &'a str },
    Arrow,
    OpMix { expected: char },
}

impl<'a> ParseError<'a> {
    pub fn diagnostic<F>(&self, full: &'a str, file: F) -> Diagnostic<F> {
        let actual =
            ls::<_, _, ()>(span(full, take_while(|c: char| !c.is_whitespace())))(self.input)
                .map(|(_, n)| n)
                .unwrap();

        let message = self.diagnostic_message();
        let label = self.diagnostic_label(full, self.input, actual, file);
        let notes = self.diagnostic_notes();

        Diagnostic::error()
            .with_message(message)
            .with_labels(vec![label])
            .with_notes(notes)
    }

    fn diagnostic_message(&self) -> String {
        match self.kind {
            Ok(kind) => match kind {
                ParseErrorKind::Arrow => "Expected arrow".into(),
                ParseErrorKind::WrongIndent { actual, base } => String::from(if actual == base {
                    "Expected a new level of indentation"
                } else {
                    "Illegal indentation"
                }),
                ParseErrorKind::OpMix { expected } => {
                    format!("Expected '{}'", expected)
                }
                ParseErrorKind::Common(c) => c.diagnostic_message(),
                ParseErrorKind::Char(c) => format!("Expected '{}'", c),
            },
            Err(_) => String::from("Unexpected characters"),
        }
    }

    fn diagnostic_label<F>(
        &self,
        full: &'a str,
        remaining: &'a str,
        actual: Span<&'a str>,
        file: F,
    ) -> Label<F> {
        let range = match self.kind {
            Ok(ParseErrorKind::WrongIndent { actual, .. }) => {
                let start = span::substr_index(full, actual).unwrap();
                start..start + actual.len()
            }
            Ok(ParseErrorKind::Char(_)) => {
                let char_size = actual.1.chars().next().unwrap_or(' ').len_utf8();
                actual.0..actual.0 + char_size
            }
            Ok(ParseErrorKind::Common(c)) => c.range(remaining, full),
            _ => actual.0..actual.2,
        };

        Label::new(LabelStyle::Primary, file, range)
    }

    fn diagnostic_notes(&self) -> Vec<String> {
        match self.kind {
            Ok(ParseErrorKind::OpMix { .. }) => {
                vec!["'&' and '|' can't be mixed without parentheses".into()]
            }
            Ok(ParseErrorKind::Common(c)) => c.diagnostic_notes(),
            _ => Vec::new(),
        }
    }
}

impl<'a> NomParseError<&'a str> for ParseError<'a> {
    fn from_error_kind(input: &'a str, kind: NomErrorKind) -> Self {
        Self {
            input,
            kind: Err(kind),
        }
    }

    fn append(_input: &'a str, _kind: NomErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: &'a str, c: char) -> Self {
        Self {
            input,
            kind: Ok(ParseErrorKind::Char(c)),
        }
    }
}

impl<'a> ContextError<&'a str> for ParseError<'a> {}

impl<'a> FromExternalError<&'a str, ParseErrorKind<'a>> for ParseError<'a> {
    fn from_external_error(input: &'a str, _: NomErrorKind, kind: ParseErrorKind<'a>) -> Self {
        Self {
            input,
            kind: Ok(kind),
        }
    }
}

impl<'a> FromExternalError<&'a str, CommonError> for ParseError<'a> {
    fn from_external_error(input: &'a str, _: NomErrorKind, kind: CommonError) -> Self {
        Self {
            input,
            kind: Ok(ParseErrorKind::Common(kind)),
        }
    }
}
