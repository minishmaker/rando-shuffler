use crate::FullIdent;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct RuleDecl<'a> {
    name: &'a str,
    data_type: DataType,
    stateful: bool,
    arity: u32,
    roles: BitFlags<Role>
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct ConsumerDecl {
    name: &'a str,
    lock: &'a str,
    key: &'a str
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct RuleDef<'a, S> {
    name: &'a str,
    values: Vec<&'a str>,
    body: RuleBody<'a, S>
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
pub enum RuleBody<'a, S> {
    County(RuleBodyCounty<'a, S>),
    Truthy(RuleBodyTruthy<'a, S>),
    Reference(&'a str, Vec<Value<'a>>)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyCounty<'a, S> {
    Constant(Literal<County>),
    Reference(&'a str, Vec<Value<'a>>),
    LinearComb(Vec<(RuleBodyCounty<'a, S>, u32)>),
    Count(&'a str, Relation, Value<'a>, Box<RuleBodyTruthy<'a, S>>)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyTruthy<'a, S> {
    Constant(Literal<Truthy>),
    Reference(&'a str, Vec<Value<'a>>),
    Access(&'a str, Value<'a>),
    Compare(Box<RuleBodyCounty<'a, S>>, u32),
    Exists(&'a str, Relation, Value<'a>, Box<RuleBodyTruthy<'a, S>>),
    All(Vec<RuleBodyTruthy<'a, S>>),
    CheckPosterior(Vec<StateBody>),
    CheckPrior(Vec<StateBody>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum StateBody<'a> {
    Set(Value<'a>),
    NotSet(Value<'a>)
}