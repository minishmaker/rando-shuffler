use std::fmt::{self, Display, Formatter};

use crate::Span;

use super::ast::{DescriptorError, Reference, RuleBodyCounty, RuleBodyTruthy, RuleBodyUntyped};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DescriptorType {
    Truthy,
    County,
    Unknown,
}

impl Display for DescriptorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = match self {
            DescriptorType::Truthy => "truthy",
            DescriptorType::County => "county",
            DescriptorType::Unknown => "an unknown type",
        };

        write!(f, "{}", name)
    }
}

impl RuleBodyUntyped<'_> {
    fn descriptor_type(&self) -> DescriptorType {
        match self {
            Self::Truthy(_) => DescriptorType::Truthy,
            Self::County(_) => DescriptorType::County,
            _ => DescriptorType::Unknown,
        }
    }
}

pub trait Typecheck<'a> {
    fn descriptor_type() -> DescriptorType;
    fn extract(val: RuleBodyUntyped<'a>) -> Result<Self, RuleBodyUntyped<'a>>
    where
        Self: Sized;
    fn to_rule_body(self) -> RuleBodyUntyped<'a>;
    fn and(v: Vec<Self>) -> Self
    where
        Self: Sized;
    fn or(v: Vec<Self>) -> Self
    where
        Self: Sized;
    fn reference(r: Reference<'a>) -> Self
    where
        Self: Sized;
}

impl<'a> Typecheck<'a> for RuleBodyTruthy<'a> {
    fn descriptor_type() -> DescriptorType {
        DescriptorType::Truthy
    }

    fn extract(val: RuleBodyUntyped<'a>) -> Result<Self, RuleBodyUntyped<'a>> {
        if let RuleBodyUntyped::Truthy(t) = val {
            Ok(t)
        } else {
            Err(val)
        }
    }

    fn to_rule_body(self) -> RuleBodyUntyped<'a> {
        RuleBodyUntyped::Truthy(self)
    }

    fn and(v: Vec<Self>) -> Self {
        Self::And(v)
    }

    fn or(v: Vec<Self>) -> Self {
        Self::Or(v)
    }

    fn reference(r: Reference<'a>) -> Self {
        Self::Reference(r)
    }
}

impl<'a> Typecheck<'a> for RuleBodyCounty<'a> {
    fn descriptor_type() -> DescriptorType {
        DescriptorType::County
    }

    fn extract(val: RuleBodyUntyped<'a>) -> Result<Self, RuleBodyUntyped<'a>> {
        match val {
            RuleBodyUntyped::County(c) => Ok(c),
            RuleBodyUntyped::Reference(r) => Ok(RuleBodyCounty::Reference(r)),
            val => Err(val),
        }
    }

    fn to_rule_body(self) -> RuleBodyUntyped<'a> {
        RuleBodyUntyped::County(self)
    }

    fn and(v: Vec<Self>) -> Self {
        Self::Min(v)
    }

    fn or(v: Vec<Self>) -> Self {
        Self::Max(v)
    }

    fn reference(r: Reference<'a>) -> Self {
        Self::Reference(r)
    }
}

impl<'a> Typecheck<'a> for RuleBodyUntyped<'a> {
    fn descriptor_type() -> DescriptorType {
        DescriptorType::Unknown
    }

    fn extract(val: RuleBodyUntyped<'a>) -> Result<Self, RuleBodyUntyped<'a>> {
        Ok(val)
    }

    fn to_rule_body(self) -> RuleBodyUntyped<'a> {
        self
    }

    fn and(v: Vec<Self>) -> Self {
        Self::And(v)
    }

    fn or(v: Vec<Self>) -> Self {
        Self::Or(v)
    }

    fn reference(r: Reference<'a>) -> Self {
        Self::Reference(r)
    }
}

pub fn typecheck<'a, T: Typecheck<'a>>(
    r: Span<RuleBodyUntyped<'a>>,
) -> Result<T, Vec<DescriptorError<'a>>> {
    let Span(s, r, l) = r;

    T::extract(r).map_err(|e| {
        vec![DescriptorError::Type {
            actual: Span(s, e.descriptor_type(), l),
            expected: T::descriptor_type(),
        }]
    })
}
