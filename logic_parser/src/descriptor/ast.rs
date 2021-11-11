use crate::FullIdent;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct RuleDef<'a> {
    pub name: &'a str,
    pub values: Vec<Value<'a>>,
    pub body: RuleBody<'a>
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Value<'a> {
    Var(&'a str),
    Const(FullIdent<'a>)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Relation {

}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBody<'a> {
    County(RuleBodyCounty<'a>),
    Truthy(RuleBodyTruthy<'a>),
    Reference(&'a str, Vec<Value<'a>>)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyCounty<'a> {
    Constant(u32),
    Reference(&'a str, Vec<Value<'a>>),
    LinearComb(Vec<(RuleBodyCounty<'a>, u32)>),
    Count(&'a str, &'a str, Value<'a>, Box<RuleBodyTruthy<'a>>)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyTruthy<'a> {
    Constant(Oolean),
    Reference(&'a str, Vec<Value<'a>>),
    Access(&'a str, Vec<Value<'a>>),
    Compare(Box<RuleBodyCounty<'a>>, u32),
    Exists(&'a str, &'a str, Value<'a>, Box<RuleBodyTruthy<'a>>),
    All(Vec<RuleBodyTruthy<'a>>),
    CheckPosterior(Vec<StateBody<'a>>),
    CheckPrior(Vec<StateBody<'a>>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum StateBody<'a> {
    Set(Value<'a>),
    NotSet(Value<'a>)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Oolean {
    False,
    Ool,
    True
}