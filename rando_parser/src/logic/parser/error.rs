use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};

use crate::{
    common::{
        error::{CommonError, RandoError},
        span,
    },
    Span,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LogicParseError<'a> {
    Common(CommonError<'a>),
    WrongIndent { base: &'a str, actual: &'a str },
    OpMix { expected: Span<char> },
}

impl<'a> RandoError<'a> for LogicParseError<'a> {
    fn diagnostic<F: Clone>(&self, file: &F, input: &str) -> Diagnostic<F> {
        if let LogicParseError::Common(c) = self {
            return c.diagnostic(file, input);
        }

        let message = self.diagnostic_message();
        let labels = self.diagnostic_labels(input, file);
        let notes = self.diagnostic_notes();

        Diagnostic::error()
            .with_message(message)
            .with_labels(labels)
            .with_notes(notes)
    }

    fn from_common(common: CommonError<'a>) -> Self {
        Self::Common(common)
    }
}

impl<'a> LogicParseError<'a> {
    fn diagnostic_message(&self) -> String {
        match self {
            LogicParseError::WrongIndent { actual, base } => String::from(if actual == base {
                "Expected a new level of indentation"
            } else {
                "Illegal indentation"
            }),
            LogicParseError::OpMix { expected } => {
                format!("Expected '{}'", expected.1)
            }
            _ => panic!("No diagnostic message available"),
        }
    }

    fn diagnostic_labels<F: Clone>(&self, input: &'a str, file: &F) -> Vec<Label<F>> {
        let range = match self {
            LogicParseError::WrongIndent { actual, .. } => {
                let start = span::substr_index(input, actual).unwrap();
                Some(start..start + actual.len())
            }
            LogicParseError::OpMix { expected } => Some(expected.range()),
            _ => None,
        };

        range
            .map(|r| Label::new(LabelStyle::Primary, file.clone(), r))
            .into_iter()
            .collect()
    }

    fn diagnostic_notes(&self) -> Vec<String> {
        match self {
            LogicParseError::OpMix { .. } => {
                vec!["'&' and '|' can't be mixed without parentheses".into()]
            }
            _ => Vec::new(),
        }
    }
}
