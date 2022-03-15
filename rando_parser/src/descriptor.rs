use std::{collections::HashMap, rc::Rc};

use rando_core::{
    algebra::{Ntgr, Oolean},
    descriptor::Descriptor,
    shuffles::ShufflePattern,
};

use crate::{
    common::{error::ParseError, span::Span},
    descriptor::ast::Value,
    FullIdent, Ident,
};

use self::{
    ast::{
        DescriptorError, Reference, Relation, RuleBodyCounty, RuleBodyTruthy, RuleBodyUntyped,
        RuleDefUntyped,
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct RuleDef<'a> {
    pub pattern: Reference<'a>,
    pub var_map: Rc<VarMap<'a, FullIdent<'a>>>,
    pub body: RuleBody<'a>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VarMap<'a, V> {
    Full(HashMap<Ident<'a>, usize>),
    Added(Rc<VarMap<'a, V>>, Ident<'a>, V),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum ConcreteValue<'a, V> {
    Index(usize),
    Reference(&'a V),
}

impl<'a, V> VarMap<'a, V> {
    fn lookup_value<'b>(&'b self, ident: Ident) -> Option<ConcreteValue<'b, V>> {
        match self {
            VarMap::Full(m) => m.get(&ident).copied().map(ConcreteValue::Index),
            VarMap::Added(p, i, v) => {
                if ident == *i {
                    Some(ConcreteValue::Reference(v))
                } else {
                    p.lookup_value(ident)
                }
            }
        }
    }
}

impl<'a, V> ConcreteValue<'a, V> {
    fn get_value<'b, 'c>(self, values: &'b [V]) -> &'c V
    where
        'a: 'c,
        'b: 'c,
    {
        match self {
            Self::Index(i) => &values[i],
            Self::Reference(v) => v,
        }
    }
}

impl<'a> Value<'a> {
    fn resolve(
        self,
        map: &VarMap<'a, FullIdent<'a>>,
        values: &[FullIdent<'a>],
    ) -> Result<FullIdent<'a>, Ident<'a>> {
        match self {
            Value::Const(v) => Ok(v),
            Value::Var(i) => map
                .lookup_value(i)
                .map(|v| v.get_value(values).clone())
                .ok_or(i),
        }
    }
}

impl<'a> Reference<'a> {
    fn resolve_values(
        &self,
        map: &VarMap<'a, FullIdent<'a>>,
        values: &[FullIdent<'a>],
    ) -> Vec<FullIdent<'a>> {
        self.values
            .iter()
            .cloned()
            .map(Span::into_inner)
            .map(|v| v.resolve(map, values).unwrap())
            .collect::<Vec<_>>()
    }
}

impl<'a: 'b, 'b> Descriptor<FullIdent<'a>> for &'b RuleDef<'a> {
    type Truthy = (Rc<VarMap<'a, FullIdent<'a>>>, &'b RuleBodyTruthy<'a>);
    type County = (Rc<VarMap<'a, FullIdent<'a>>>, &'b RuleBodyCounty<'a>);

    fn eval<R, T, C>(&self, v: &[FullIdent<'a>], truthy_callback: T, county_callback: C) -> R
    where
        T: Fn(&Self::Truthy, &[FullIdent<'a>]) -> R,
        C: Fn(&Self::County, &[FullIdent<'a>]) -> R,
    {
        match &&self.body {
            RuleBody::Truthy(t) => truthy_callback(&(Rc::clone(&self.var_map), t), v),
            RuleBody::County(c) => county_callback(&(Rc::clone(&self.var_map), c), v),
        }
    }

    fn eval_truthy<R>(
        (map, body): &Self::Truthy,
        values: &[FullIdent<'a>],
        constant: impl Fn(Oolean) -> R,
        reference: impl Fn(&str, &[FullIdent<'a>]) -> R,
        access: impl Fn(&str, &[FullIdent<'a>]) -> R,
        compare: impl Fn(&Self::County, Ntgr) -> R,
        exists: impl Fn(
            &str,
            &ShufflePattern<FullIdent<'a>, FullIdent<'a>>,
            &dyn Fn(FullIdent<'a>) -> Self::Truthy,
        ) -> R,
        conjunction: impl Fn(R, &Self::Truthy) -> R,
        disjunction: impl Fn(R, &Self::Truthy) -> R,
        _prior: impl Fn(&[(bool, FullIdent<'a>)]) -> R,
        _posterior: impl Fn(&[(bool, FullIdent<'a>)]) -> R,
    ) -> R {
        match body {
            RuleBodyTruthy::And(v) => {
                eval_combine(map, constant(Oolean::False), v.iter(), conjunction)
            }
            RuleBodyTruthy::Or(v) => {
                eval_combine(map, constant(Oolean::False), v.iter(), disjunction)
            }
            RuleBodyTruthy::Access(r) => {
                let values = r.resolve_values(map, values);
                access(*r.keyword, &values[..])
            }
            RuleBodyTruthy::CheckPosterior(_) => todo!(),
            RuleBodyTruthy::CheckPrior(_) => todo!(),
            RuleBodyTruthy::Compare(r, v) => compare(&(Rc::clone(map), r), *v),
            RuleBodyTruthy::Constant(c) => constant(*c),
            RuleBodyTruthy::Exists(a, r, b, t) => {
                let a = a.clone().into_inner();
                let b = b.clone().into_inner();
                eval_qualified(values, a, r, b, t, map, exists)
            }
            RuleBodyTruthy::Reference(r) => {
                let values = r.resolve_values(map, values);
                reference(*r.keyword, &values[..])
            }
        }
    }

    fn eval_county<R>(
        (map, body): &Self::County,
        values: &[FullIdent<'a>],
        constant: impl Fn(Ntgr) -> R,
        reference: impl Fn(&str, &[FullIdent<'a>]) -> R,
        combination: impl Fn(R, &Self::County, Ntgr) -> R,
        min: impl Fn(R, &Self::County) -> R,
        max: impl Fn(R, &Self::County) -> R,
        count: impl Fn(
            &str,
            &ShufflePattern<FullIdent<'a>, FullIdent<'a>>,
            &dyn Fn(FullIdent<'a>) -> Self::Truthy,
        ) -> R,
    ) -> R {
        match body {
            RuleBodyCounty::Constant(c) => constant(*c),
            RuleBodyCounty::Count(a, r, b, t) => {
                let a = a.clone().into_inner();
                let b = b.clone().into_inner();
                eval_qualified(values, a, r, b, t, map, count)
            }
            RuleBodyCounty::LinearComb(v) => v
                .iter()
                .map(|(v, n)| ((Rc::clone(map), v), *n))
                .fold(constant(Ntgr::Num(0)), |r, (v, n)| combination(r, &v, n)),
            RuleBodyCounty::Max(v) => eval_combine(map, constant(Ntgr::Num(0)), v.iter(), max),
            RuleBodyCounty::Min(v) => eval_combine(map, constant(Ntgr::Num(0)), v.iter(), min),
            RuleBodyCounty::Reference(r) => {
                let values = r.resolve_values(map, values);
                reference(*r.keyword, &values[..])
            }
        }
    }
}

fn eval_combine<'a, T, R, V>(
    map: &Rc<VarMap<'a, V>>,
    start: R,
    items: impl Iterator<Item = T>,
    combine: impl Fn(R, &(Rc<VarMap<'a, V>>, T)) -> R,
) -> R {
    items
        .map(|t| (Rc::clone(map), t))
        .fold(start, |r, v| combine(r, &v))
}

fn eval_qualified<'a: 'b, 'b, R>(
    values: &[FullIdent<'a>],
    a: Value<'a>,
    r: &Relation<'a>,
    b: Value<'a>,
    t: &'b RuleBodyTruthy<'a>,
    map: &Rc<VarMap<'a, FullIdent<'a>>>,
    callback: impl Fn(
        &str,
        &ShufflePattern<FullIdent<'a>, FullIdent<'a>>,
        &dyn Fn(FullIdent<'a>) -> (Rc<VarMap<'a, FullIdent<'a>>>, &'b RuleBodyTruthy<'a>),
    ) -> R,
) -> R {
    let a = a.resolve(map, values);
    let b = b.resolve(map, values);

    let (pattern, ident) = match (a, b) {
        (Ok(a), Ok(b)) => (ShufflePattern::Both(a, b), None),
        (Err(a), Err(b)) => panic!("Both {} and {} are variables", a, b),
        (Ok(a), Err(i)) => {
            if r.left_to_right {
                (ShufflePattern::A(a), Some(i))
            } else {
                (ShufflePattern::B(a), Some(i))
            }
        }
        (Err(i), Ok(b)) => {
            if r.left_to_right {
                (ShufflePattern::B(b), Some(i))
            } else {
                (ShufflePattern::A(b), Some(i))
            }
        }
    };

    callback(*r.name, &pattern, &|v| {
        let map = match ident {
            Some(i) => Rc::new(VarMap::Added(Rc::clone(map), i, v)),
            None => Rc::clone(map),
        };

        (map, t)
    })
}
