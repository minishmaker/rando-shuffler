use std::collections::HashMap;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use rando_core::{
    algebra::{Ntgr, Oolean},
    descriptor::Descriptor,
    shuffles::ShufflePattern,
};

use crate::{
    common::{
        self,
        error::{CommonError, ParseError, RandoError},
        span::Span,
    },
    FullIdent,
};

use self::{
    ast::{
        DescriptorError, Reference, RuleBodyCounty, RuleBodyTruthy, RuleBodyUntyped, RuleDefUntyped,
    },
    parser::rules,
    typecheck::DescriptorType,
};

pub mod ast;
mod parser;
mod typecheck;

pub fn parse(
    input: &str,
) -> Result<HashMap<&str, Vec<RuleDefUntyped>>, ParseError<DescriptorError>> {
    rules(input)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBody<'a> {
    County(RuleBodyCounty<'a>),
    Truthy(RuleBodyTruthy<'a>),
}

impl RuleBody<'_> {
    pub fn from_untyped(
        r: Span<RuleBodyUntyped<'_>>,
        ty: DescriptorType,
    ) -> Result<RuleBody<'_>, Vec<DescriptorError<'_>>> {
        match ty {
            DescriptorType::Truthy => typecheck::typecheck(r).map(RuleBody::Truthy),
            DescriptorType::County => typecheck::typecheck(r).map(RuleBody::County),
            DescriptorType::Unknown => panic!("Cannot cast to unknown type"),
        }
    }
}
