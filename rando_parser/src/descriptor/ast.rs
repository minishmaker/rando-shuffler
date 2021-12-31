use crate::{common::Span, FullIdent, Ident};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct RuleDef<'a> {
    pub name: &'a str,
    pub values: Vec<Value<'a>>,
    pub body: RuleBody<'a>,
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
    pub end: usize,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Relation<'a> {
    pub left_to_right: bool,
    pub name: Span<&'a str>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBody<'a> {
    County(RuleBodyCounty<'a>),
    Truthy(RuleBodyTruthy<'a>),
    Reference(Reference<'a>),
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Oolean {
    False,
    Ool,
    True,
}
