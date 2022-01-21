use std::collections::HashMap;

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::common::{
    self,
    error::{CommonError, ParseError, RandoError},
    span::Span,
};

use self::{ast::RuleDef, parser::rules, typecheck::DescriptorType};

pub mod ast;
mod parser;
mod typecheck;

pub fn parse(input: &str) -> Result<HashMap<&str, Vec<RuleDef>>, ParseError<DescriptorError>> {
    rules(input)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DescriptorError<'a> {
    Common(CommonError<'a>),
    Type {
        expected: DescriptorType,
        actual: Span<DescriptorType>,
    },
}

impl<'a> RandoError<'a> for DescriptorError<'a> {
    fn from_common(common: common::error::CommonError<'a>) -> Self {
        DescriptorError::Common(common)
    }

    fn diagnostic<F: Clone>(&self, file: &F, input: &str) -> Diagnostic<F> {
        match self {
            DescriptorError::Common(c) => c.diagnostic(file, input),
            DescriptorError::Type { expected, actual } => {
                let range = actual.range();
                let labels = vec![Label::primary(file.clone(), range)];
                Diagnostic::error()
                    .with_message(format!(
                        "Type error: expected {}, found {}",
                        expected,
                        actual.inner()
                    ))
                    .with_labels(labels)
            }
        }
    }
}
