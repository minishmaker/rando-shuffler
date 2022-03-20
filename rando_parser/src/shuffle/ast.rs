use std::collections::HashMap;

use crate::{FullIdent, Span};

// To be fixed
pub enum Ntgr {
    Num(u32),
    Infinity,
}

pub enum RelationExpr<'a> {
    Num(Ntgr),
    Reference(Span<&'a str>),
    From(Box<RelationExpr<'a>>),
    Repeat {
        individual: bool,
        rel: Box<RelationExpr<'a>>,
    },
    Set(SetCtor<'a>),
    Constructor(RelationCtor<'a>),
    Then(Vec<RelationExpr<'a>>),
    Map {
        from: Box<RelationExpr<'a>>,
        to: Box<RelationExpr<'a>>,
        including: Vec<RelationExpr<'a>>,
    },
    Connect(Vec<RelationExpr<'a>>),
    Data(DataExpr<'a>),
    Match {
        input: SetCtor<'a>,
        relation: Box<RelationExpr<'a>>,
        output: Vec<(SetCtor<'a>, RelationExpr<'a>)>,
    },
    Extension,
}

pub struct SetCtor<'a>(pub HashMap<Value<'a>, u32>);
pub struct RelationCtor<'a>(pub HashMap<(Value<'a>, Value<'a>), u32>);

pub struct DataExpr<'a>(&'a ());

pub enum DataItem<'a> {
    Access {
        field: &'a str,
        tag: Option<Box<DataItem<'a>>>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value<'a> {
    Unit,
    FullIdent(FullIdent<'a>),
}
