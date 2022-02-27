use std::borrow::Cow;
use std::hash::Hash;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum DescriptorType {
    Truthy,
    County,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Query<'a, V: Hash + Eq + Clone, LN: Hash + Eq + Clone> {
    Descriptor(Cow<'a, str>, Cow<'a, [V]>, DescriptorType),
    Node(LN),
    Access(Cow<'a, str>, Cow<'a, [V]>),
}

impl<'a, V: Hash + Eq + Clone, LN: Hash + Eq + Clone> Query<'a, V, LN> {
    pub fn upgrade<'b: 'a>(&self) -> Query<'b, V, LN>
    where
        V: 'b,
    {
        match self {
            Query::Descriptor(a, b, c) => Query::Descriptor(
                Cow::Owned(a.clone().into_owned()),
                Cow::Owned(b.clone().into_owned()),
                *c,
            ),
            Query::Node(n) => Query::Node(n.clone()),
            Query::Access(a, b) => Query::Access(
                Cow::Owned(a.clone().into_owned()),
                Cow::Owned(b.clone().into_owned()),
            ),
        }
    }
}
