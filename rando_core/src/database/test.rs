use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::EdgeRef,
    Graph,
};

use crate::{
    algebra::{County, Ntgr, Oolean, Sphery, Statey, Truthy},
    descriptor::{Descriptor, Edge, EdgeTy, Logic},
    shuffles::{Shuffle, ShuffleDelta, ShufflePattern},
};
use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
    hash::Hash,
};

use super::{DBImpl, Database};

/// Provides a basic database backed by the given logic. There is only one relation, `""`, and it
/// starts out empty.
///
/// The builtin descriptors are:
///     - `"true"`: always top
///     - `"ool"`: always ool
///     - `"false"`: always bottom
///     - `"a->b"`: top if parameter 1 maps to parameter 2
///     - `"a->?"`: top if parameter 1 maps to anything
///     - `"?->b"`: top if anything maps to parameter 1
fn build_test_database<'a, T, C, L>(
    logic: L,
) -> impl Database<'a, RelationDescriptor<'a, &'a str>, L, &'a str, T, C, TestShuffle<'a>, Err = ()>
where
    T: Clone + Eq + Truthy + Sphery + Statey,
    C: Clone + Eq + County<T> + Sphery + Statey,
    L: Logic<&'a str>,
    L::Node: Clone + Hash + Eq,
{
    let descriptors = {
        let mut descriptors = HashMap::new();
        descriptors.insert("true", vec![RelationDescriptor::Constant(Oolean::True)]);
        descriptors.insert("ool", vec![RelationDescriptor::Constant(Oolean::Ool)]);
        descriptors.insert("false", vec![RelationDescriptor::Constant(Oolean::False)]);
        descriptors.insert("a->b", vec![RelationDescriptor::Both("")]);
        descriptors.insert("a->?", vec![RelationDescriptor::A("")]);
        descriptors.insert("?->b", vec![RelationDescriptor::B("")]);
        descriptors
    };
    let shuffles = {
        let mut shuffles = HashMap::new();
        shuffles.insert("", TestShuffle::default());
        shuffles
    };

    DBImpl::initialize(shuffles, logic, HashMap::new(), descriptors, HashMap::new())
}

#[test]
fn test_direct_query() {
    let logic = TestLogic {
        graph: DiGraph::new(),
    };
    let database = build_test_database(logic);

    assert_eq!(
        Ok(Oolean::True),
        database.query_descriptor_truthy("true", &[])
    );
    assert_eq!(
        Ok(Oolean::Ool),
        database.query_descriptor_truthy("ool", &[])
    );
    assert_eq!(
        Ok(Oolean::False),
        database.query_descriptor_truthy("false", &[])
    );

    assert_eq!(
        Err::<TestCounty, _>(()),
        database.query_descriptor_county("true", &[])
    )
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
struct TestShuffle<'a> {
    to: HashMap<&'a str, HashSet<&'a str>>,
    from: HashMap<&'a str, HashSet<&'a str>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TestDelta<'a> {
    Add(&'a str, &'a str),
}

impl<'a> ShuffleDelta<&'a str, &'a str> for TestDelta<'a> {
    fn affects(&self, pattern: &ShufflePattern<&'a str, &'a str>) -> bool {
        match self {
            TestDelta::Add(a0, b0) => match pattern {
                ShufflePattern::A(a1) => a0 == a1,
                ShufflePattern::B(b1) => b0 == b1,
                ShufflePattern::Both(a1, b1) => a0 == a1 && b0 == b1,
            },
        }
    }

    fn is_destructive(&self) -> bool {
        false
    }
}

impl<'a> Shuffle<&'a str, &'a str> for TestShuffle<'a> {
    type Delta = TestDelta<'a>;

    fn to(&self, a: &&'a str) -> HashSet<&'a str> {
        self.to[a].clone()
    }

    fn from(&self, b: &&'a str) -> HashSet<&'a str> {
        self.from[b].clone()
    }

    fn modify(&mut self, delta: &Self::Delta) {
        match delta {
            TestDelta::Add(a, b) => {
                self.to.entry(a).or_insert(HashSet::new()).insert(b);
                self.from.entry(b).or_insert(HashSet::new()).insert(a);
            }
        }
    }
}

#[derive(Clone, Default, Debug)]
struct TestLogic<'a, V: Hash + Eq> {
    graph: DiGraph<(), RelationDescriptor<'a, V>>,
}

impl<'a, V: Clone + Hash + Eq> Logic<V> for TestLogic<'a, V> {
    type Node = NodeIndex<u32>;
    type Descriptor = RelationDescriptor<'a, V>;

    fn list_nodes<'b>(&'b self) -> Box<dyn Iterator<Item = Self::Node> + 'b> {
        Box::new(self.graph.node_indices())
    }

    fn edges<'b>(
        &'b self,
        source: &Self::Node,
    ) -> Box<dyn Iterator<Item = Edge<'b, Self::Descriptor, Self::Node>> + 'b> {
        Box::new(self.graph.edges(*source).map(|e| Edge {
            descriptor: e.weight(),
            ty: EdgeTy::FromNode(e.target()),
        }))
    }

    fn access_nodes<'b>(
        &'b self,
        _name: &str,
        _values: &[V],
    ) -> Box<dyn Iterator<Item = Self::Node> + 'b> {
        Box::new(std::iter::empty())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum RelationDescriptor<'a, V: Hash + Eq> {
    Constant(Oolean),
    Ref(&'a str, &'a [V]),
    A(&'a str),
    B(&'a str),
    Both(&'a str),
    And(Box<(RelationDescriptor<'a, V>, RelationDescriptor<'a, V>)>),
    Or(Box<(RelationDescriptor<'a, V>, RelationDescriptor<'a, V>)>),
}

impl<V: Clone + Hash + Eq> Descriptor<V> for RelationDescriptor<'_, V> {
    type Truthy = Self;
    type County = Infallible;

    fn eval<R, T, C>(&self, v: &[V], t: T, c: C) -> R
    where
        T: Fn(&Self::Truthy, &[V]) -> R,
        C: Fn(&Self::County, &[V]) -> R,
    {
        t(self, v)
    }

    fn eval_truthy<R>(
        body: &Self::Truthy,
        values: &[V],
        c: impl Fn(Oolean) -> R,
        r: impl Fn(&str, &[V]) -> R,
        _a: impl Fn(&str, &[V]) -> R,
        _comp: impl Fn(&Self::County, Ntgr) -> R,
        ex: impl Fn(&str, &ShufflePattern<V, V>, &dyn Fn(&V) -> Self::Truthy) -> R,
        conj: impl Fn(&Self::Truthy, &Self::Truthy) -> R,
        disj: impl Fn(&Self::Truthy, &Self::Truthy) -> R,
        _prio: impl Fn(&[(bool, V)]) -> R,
        _post: impl Fn(&[(bool, V)]) -> R,
    ) -> R {
        match body {
            Self::Constant(ool) => c(*ool),
            Self::Ref(name, vals) => r(name, vals),
            Self::A(name) => ex(*name, &ShufflePattern::A(values[0].clone()), &|_| {
                Self::Constant(Oolean::True)
            }),
            Self::B(name) => ex(*name, &ShufflePattern::B(values[0].clone()), &|_| {
                Self::Constant(Oolean::True)
            }),
            Self::Both(name) => ex(
                *name,
                &ShufflePattern::Both(values[0].clone(), values[1].clone()),
                &|_| Self::Constant(Oolean::True),
            ),
            Self::And(b) => {
                let (a, b) = &**b;
                conj(a, b)
            }
            Self::Or(b) => {
                let (a, b) = &**b;
                disj(a, b)
            }
        }
    }

    fn eval_county<R>(
        _body: &Self::County,
        _values: &[V],
        c: impl Fn(Ntgr) -> R,
        _r: impl Fn(&str, &[V]) -> R,
        _comb: impl Fn(&Self::County, Ntgr, &Self::County) -> R,
        _min: impl Fn(&Self::County, &Self::County) -> R,
        _max: impl Fn(&Self::County, &Self::County) -> R,
        _ct: impl Fn(&str, ShufflePattern<V, V>, &dyn Fn(&V) -> Self::Truthy) -> R,
    ) -> R {
        c(Ntgr::Infinity)
    }
}

impl Truthy for Oolean {
    fn top() -> Self {
        Oolean::True
    }

    fn ool() -> Self {
        Oolean::Ool
    }

    fn bottom() -> Self {
        Oolean::False
    }

    fn join(&self, other: &Self) -> Self {
        std::cmp::max(*self, *other)
    }

    fn meet(&self, other: &Self) -> Self {
        std::cmp::min(*self, *other)
    }
}

impl Sphery for Oolean {
    fn increment(&self) -> Self {
        *self
    }
}

impl Statey for Oolean {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct TestCounty {
    access: Ntgr,
    ool: Ntgr,
}

impl Truthy for TestCounty {
    fn top() -> Self {
        TestCounty {
            access: Ntgr::Infinity,
            ool: Ntgr::Infinity,
        }
    }

    fn ool() -> Self {
        TestCounty {
            access: Ntgr::Num(0),
            ool: Ntgr::Infinity,
        }
    }

    fn bottom() -> Self {
        TestCounty {
            access: Ntgr::Num(0),
            ool: Ntgr::Num(0),
        }
    }

    fn join(&self, other: &Self) -> Self {
        TestCounty {
            access: std::cmp::max(self.access, other.access),
            ool: std::cmp::max(self.ool, other.ool),
        }
    }

    fn meet(&self, other: &Self) -> Self {
        TestCounty {
            access: std::cmp::min(self.access, other.access),
            ool: std::cmp::min(self.ool, other.ool),
        }
    }
}

impl County<Oolean> for TestCounty {
    fn add(&self, other: &Self) -> Self {
        TestCounty {
            access: self.access + other.access,
            ool: self.ool + other.ool,
        }
    }

    fn scale(&self, n: Ntgr) -> Self {
        TestCounty {
            access: self.access * n,
            ool: self.ool * n,
        }
    }

    fn lift(truthy: &Oolean) -> Self {
        match truthy {
            Oolean::False => Self::bottom(),
            Oolean::Ool => Self::ool(),
            Oolean::True => Self::top(),
        }
    }

    fn ge(&self, n: Ntgr) -> Oolean {
        if self.access >= n {
            Oolean::True
        } else if self.ool >= n {
            Oolean::Ool
        } else {
            Oolean::False
        }
    }
}

impl Sphery for TestCounty {
    fn increment(&self) -> Self {
        *self
    }
}

impl Statey for TestCounty {}
