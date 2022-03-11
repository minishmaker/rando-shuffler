use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::EdgeRef,
};

use crate::{
    algebra::{County, Ntgr, Oolean, Sphery, Statey, Truthy},
    descriptor::{Descriptor, Edge, EdgeTy, Logic},
    shuffles::{Shuffle, ShuffleDelta, ShufflePattern},
};
use std::{
    cmp::{self, Ordering},
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
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
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
fn test_relations() {
    // (true) -> a -(0->1)> b -> c -> a
    // a -(0->?)> d -ool> c
    let mut graph = DiGraph::new();
    let a = graph.add_node(());
    let b = graph.add_node(());
    let c = graph.add_node(());
    let d = graph.add_node(());

    graph.add_edge(b, a, RelationDescriptor::Ref("a->b", &["0", "1"]));
    graph.add_edge(c, b, RelationDescriptor::Constant(Oolean::True));
    graph.add_edge(d, a, RelationDescriptor::Ref("a->?", &["0"]));
    graph.add_edge(c, d, RelationDescriptor::Constant(Oolean::Ool));
    graph.add_edge(a, c, RelationDescriptor::Constant(Oolean::True));

    let logic = TestLogic {
        graph,
        truthy_edges: vec![vec![RelationDescriptor::Constant(Oolean::True)]],
    };

    let mut database = build_test_database::<Oolean, TestCounty, _>(logic);

    assert_eq!(Ok(Oolean::False), database.query_logic_node(&c));
    assert_eq!(Ok(()), database.mod_shuffle("", &TestDelta::Add("0", "2")));
    assert_eq!(Ok(Oolean::Ool), database.query_logic_node(&c));
    assert_eq!(Ok(Oolean::False), database.query_logic_node(&b));
    assert_eq!(Ok(()), database.mod_shuffle("", &TestDelta::Add("0", "1")));
    assert_eq!(Ok(Oolean::True), database.query_logic_node(&c));
    assert_eq!(
        Ok(()),
        database.mod_shuffle("", &TestDelta::Remove("0", "2"))
    );
    assert_eq!(Ok(Oolean::True), database.query_logic_node(&c));
    assert_eq!(Ok(Oolean::True), database.query_logic_node(&d));
    assert_eq!(
        Ok(()),
        database.mod_shuffle("", &TestDelta::Remove("0", "1"))
    );
    assert_eq!(Ok(Oolean::False), database.query_logic_node(&c));
    assert_eq!(Ok(Oolean::False), database.query_logic_node(&b));
    assert_eq!(Ok(Oolean::False), database.query_logic_node(&d));
}

#[test]
fn test_graph() {
    // Graph:
    // (true) -> a -> b -ool> c -> d -false> e
    // a -ool> f
    // b -> f
    // b -> a
    let mut graph = DiGraph::new();
    let a = graph.add_node(());
    let b = graph.add_node(());
    let c = graph.add_node(());
    let d = graph.add_node(());
    let e = graph.add_node(());
    let f = graph.add_node(());

    graph.add_edge(b, a, RelationDescriptor::Constant(Oolean::True));
    graph.add_edge(c, b, RelationDescriptor::Constant(Oolean::Ool));
    graph.add_edge(d, c, RelationDescriptor::Constant(Oolean::True));
    graph.add_edge(e, d, RelationDescriptor::Constant(Oolean::False));
    graph.add_edge(f, a, RelationDescriptor::Constant(Oolean::Ool));
    graph.add_edge(f, b, RelationDescriptor::Constant(Oolean::True));
    graph.add_edge(a, b, RelationDescriptor::Constant(Oolean::True));

    let logic = TestLogic {
        graph,
        truthy_edges: vec![vec![RelationDescriptor::Constant(Oolean::True)]],
    };

    let database = build_test_database::<Oolean, TestCounty, _>(logic);

    assert_eq!(Ok(Oolean::True), database.query_logic_node(&a));
    assert_eq!(Ok(Oolean::Ool), database.query_logic_node(&d));
    assert_eq!(Ok(Oolean::False), database.query_logic_node(&e));
    assert_eq!(Ok(Oolean::True), database.query_logic_node(&f));
}

#[test]
fn test_direct_query() {
    let logic = TestLogic {
        graph: DiGraph::new(),
        truthy_edges: Vec::new(),
    };
    let database = build_test_database::<Oolean, TestCounty, _>(logic);

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
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
struct TestShuffle<'a> {
    to: HashMap<&'a str, HashSet<&'a str>>,
    from: HashMap<&'a str, HashSet<&'a str>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TestDelta<'a> {
    Add(&'a str, &'a str),
    Remove(&'a str, &'a str),
}

impl<'a> ShuffleDelta<&'a str, &'a str> for TestDelta<'a> {
    fn affects(&self, pattern: &ShufflePattern<&'a str, &'a str>) -> bool {
        let (TestDelta::Add(a0, b0) | TestDelta::Remove(a0, b0)) = self;
        match pattern {
            ShufflePattern::A(a1) => a0 == a1,
            ShufflePattern::B(b1) => b0 == b1,
            ShufflePattern::Both(a1, b1) => a0 == a1 && b0 == b1,
        }
    }
}

impl<'a> Shuffle<&'a str, &'a str> for TestShuffle<'a> {
    type Delta = TestDelta<'a>;

    fn to(&self, a: &&'a str) -> HashSet<&'a str> {
        self.to.get(a).map(|a| a.clone()).unwrap_or_default()
    }

    fn from(&self, b: &&'a str) -> HashSet<&'a str> {
        self.from.get(b).map(|b| b.clone()).unwrap_or_default()
    }

    fn modify(&mut self, delta: &Self::Delta) {
        match delta {
            TestDelta::Add(a, b) => {
                self.to.entry(a).or_insert(HashSet::new()).insert(b);
                self.from.entry(b).or_insert(HashSet::new()).insert(a);
            }
            TestDelta::Remove(a, b) => {
                self.to.get_mut(a).map(|a| a.remove(b));
                self.from.get_mut(b).map(|b| b.remove(a));
            }
        }
    }
}

#[derive(Clone, Default, Debug)]
struct TestLogic<'a, V: Hash + Eq> {
    graph: DiGraph<(), RelationDescriptor<'a, V>>,
    truthy_edges: Vec<Vec<RelationDescriptor<'a, V>>>,
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
        Box::new(
            self.graph
                .edges(*source)
                .map(|e| Edge {
                    descriptor: e.weight(),
                    ty: EdgeTy::FromNode(e.target()),
                })
                .chain(
                    self.truthy_edges
                        .get(source.index())
                        .into_iter()
                        .flat_map(|v| v.iter())
                        .map(|d| Edge {
                            descriptor: d,
                            ty: EdgeTy::FromTrue,
                        }),
                ),
        )
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
}

impl<V: Clone + Hash + Eq> Descriptor<V> for RelationDescriptor<'_, V> {
    type Truthy = Self;
    type County = Infallible;

    fn eval<R, T, C>(&self, v: &[V], t: T, _c: C) -> R
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
        _conj: impl Fn(R, &Self::Truthy) -> R,
        _disj: impl Fn(R, &Self::Truthy) -> R,
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
        }
    }

    fn eval_county<R>(
        _body: &Self::County,
        _values: &[V],
        c: impl Fn(Ntgr) -> R,
        _r: impl Fn(&str, &[V]) -> R,
        _comb: impl Fn(R, &Self::County, Ntgr) -> R,
        _min: impl Fn(R, &Self::County) -> R,
        _max: impl Fn(R, &Self::County) -> R,
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
        cmp::max(*self, *other)
    }

    fn meet(&self, other: &Self) -> Self {
        cmp::min(*self, *other)
    }
}

impl Sphery for Oolean {
    fn increment(&self) -> Self {
        *self
    }
}

impl Statey for Oolean {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
            access: cmp::max(self.access, other.access),
            ool: cmp::max(self.ool, other.ool),
        }
    }

    fn meet(&self, other: &Self) -> Self {
        TestCounty {
            access: cmp::min(self.access, other.access),
            ool: cmp::min(self.ool, other.ool),
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

impl PartialOrd for TestCounty {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let access = self.access.cmp(&other.access);
        let ool = self.ool.cmp(&other.ool);

        match (access, ool) {
            (Ordering::Greater, Ordering::Less) => None,
            (Ordering::Less, Ordering::Greater) => None,
            (a, Ordering::Equal) => Some(a),
            (Ordering::Equal, b) => Some(b),
            (a, _) => Some(a),
        }
    }
}
