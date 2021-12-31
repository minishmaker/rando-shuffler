use std::ops::Range;

use nom::{combinator::recognize, multi::many0, branch::alt, character::complete::alphanumeric1, bytes::complete::tag};

use super::span::{span, Span};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CommonError {
    ExpectedKeyword,
    ExpectedIdent,
}

impl CommonError {
    pub fn diagnostic_message(&self) -> String {
        match self {
            CommonError::ExpectedKeyword => "Expected keyword, found identifier".into(),
            CommonError::ExpectedIdent => "Expected identifier, found keyword".into(),
        }
    }

    pub fn diagnostic_notes(&self) -> Vec<String> {
        match self {
            CommonError::ExpectedKeyword => vec!["Keywords must begin with a lowercase letter".into()],
            CommonError::ExpectedIdent => vec![
                "Identifiers cannot start with lowercase letters unless enclosed in quotes (\")"
                    .into()
            ]
        }
    }

    pub fn range(&self, remaining: &str, full: &str) -> Range<usize> {
        span::<_, _, ()>(full, recognize(many0(alt((alphanumeric1, tag("_"))))))(remaining)
            .map(|(_, Span(s, _, e))| s..e)
            .unwrap()
    }
}
