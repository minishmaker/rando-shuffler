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
pub struct RuleDef<'a> {
    name: &'a str,
    values: Vec<&'a str>,
    body: RuleBody<'a>
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
    Constant(Literal<County>),
    Reference(&'a str, Vec<Value<'a>>),
    LinearComb(Vec<(RuleBodyCounty<'a>, u32)>),
    Count(&'a str, Relation, Value<'a>, Box<RuleBodyTruthy<'a>>)
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RuleBodyTruthy<'a> {
    Constant(Literal<Truthy>),
    Reference(&'a str, Vec<Value<'a>>),
    Access(&'a str, Value<'a>),
    Compare(Box<RuleBodyCounty<'a>>, u32),
    Exists(&'a str, Relation, Value<'a>, Box<RuleBodyTruthy<'a>>),
    All(Vec<RuleBodyTruthy<'a>>),
    CheckPosterior(Vec<StateBody>),
    CheckPrior(Vec<StateBody>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum StateBody<'a> {
    Set(Value<'a>),
    NotSet(Value<'a>)
}