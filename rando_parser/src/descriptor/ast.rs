use codespan_reporting::diagnostic::{Diagnostic, Label};
use rando_core::algebra::Oolean;

use crate::{
    common::{
        error::{CommonError, RandoError},
        span::Span,
    },
    FullIdent, Ident,
};

use super::typecheck::DescriptorType;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct RuleDefUntyped<'a> {
    pub reference: Reference<'a>,
    pub body: RuleBodyUntyped<'a>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Value<'a> {
    Var(Ident<'a>),
    Const(FullIdent<'a>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Reference<'a> {
    pub keyword: Span<&'a str>,
    pub values: Vec<Span<Value<'a>>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Relation<'a> {
    pub left_to_right: bool,
    pub name: Span<&'a str>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyUntyped<'a> {
    County(RuleBodyCounty<'a>),
    Truthy(RuleBodyTruthy<'a>),
    Reference(Reference<'a>),
    And(Vec<RuleBodyUntyped<'a>>),
    Or(Vec<RuleBodyUntyped<'a>>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyCounty<'a> {
    Constant(u32),
    Reference(Reference<'a>),
    LinearComb(Vec<(RuleBodyCounty<'a>, u32)>),
    Min(Vec<RuleBodyCounty<'a>>),
    Max(Vec<RuleBodyCounty<'a>>),
    Count(
        Span<Ident<'a>>,
        Relation<'a>,
        Span<Value<'a>>,
        Box<RuleBodyTruthy<'a>>,
    ),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyTruthy<'a> {
    Constant(Oolean),
    Reference(Reference<'a>),
    Access(Reference<'a>),
    Compare(Box<RuleBodyCounty<'a>>, u32),
    Exists(
        Span<Ident<'a>>,
        Relation<'a>,
        Span<Value<'a>>,
        Box<RuleBodyTruthy<'a>>,
    ),
    And(Vec<RuleBodyTruthy<'a>>),
    Or(Vec<RuleBodyTruthy<'a>>),
    CheckPosterior(Vec<Span<StateBody<'a>>>),
    CheckPrior(Vec<Span<StateBody<'a>>>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum StateBody<'a> {
    Set(Value<'a>),
    NotSet(Value<'a>),
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
    fn from_common(common: CommonError<'a>) -> Self {
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
