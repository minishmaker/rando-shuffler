use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use petgraph::graph::DiGraph;

use crate::{
    algebra::{County, Sphery, Statey, Truthy},
    descriptor::{Descriptor, EdgeTy, Logic},
    shuffles::Shuffle,
    Ntgr, Oolean, Relation,
};

trait Database<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    V: Hash + Eq,
    T: Truthy + Sphery + Statey,
    C: County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    type Err;

    fn initialize(
        shuffles: HashMap<&'a str, R>,
        logic: L,
        descriptors_keysy: HashMap<&'a str, (&'a str, &'a str)>,
        descriptors_truthy: HashMap<&'a str, Vec<D>>,
        descriptors_county: HashMap<&'a str, Vec<D>>,
    ) -> Self;

    fn query_descriptor_truthy(&self, name: &str, values: &[V]) -> Result<T, Self::Err>;
    fn query_descriptor_county(&self, name: &str, values: &[V]) -> Result<C, Self::Err>;
    fn query_logic_node(&self, ln: &L::Node) -> Result<T, Self::Err>;
    fn query_access(&self, name: &str, values: &[V]) -> Result<T, Self::Err>;

    fn mod_shuffle(&mut self, delta: &R::Delta);
}

struct DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Clone + Hash + Eq,
    V: Clone + Hash + Eq,
    T: Clone + Truthy + Sphery + Statey,
    C: Clone + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    shuffles: HashMap<&'a str, R>,
    logic: L,
    descriptors_keysy: HashMap<&'a str, (&'a str, &'a str)>,
    descriptors_truthy: HashMap<&'a str, Vec<D>>,
    descriptors_county: HashMap<&'a str, Vec<D>>,
    cache: DBCache<T, C, V, L::Node>,
}

struct DBCache<
    T: Truthy + Sphery + Statey,
    C: County<T> + Statey,
    V: Hash + Eq + Clone,
    LN: Hash + Eq + Clone,
> where
    T: Clone + Truthy + Sphery + Statey,
    C: Clone + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    truthy_cache: RefCell<HashMap<Query<V, LN>, T>>,
    county_cache: RefCell<HashMap<Query<V, LN>, C>>,
    dependencies: DiGraph<Query<V, LN>, Query<V, LN>>,
}

impl<'a, T, C, V, LN> DBCache<T, C, V, LN>
where
    T: Clone + Truthy + Sphery + Statey,
    C: Clone + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    fn query_truthy(&self, query: &Query<V, LN>) -> Option<T> {
        self.truthy_cache.borrow().get(query).map(Clone::clone)
    }

    fn set_truthy(&self, query: Query<V, LN>, value: T) {
        self.truthy_cache.borrow_mut().insert(query, value);
    }

    fn query_county(&self, query: &Query<V, LN>) -> Option<C> {
        self.county_cache.borrow().get(query).map(Clone::clone)
    }

    fn set_county(&self, query: Query<V, LN>, value: C) {
        self.county_cache.borrow_mut().insert(query, value);
    }
}

impl<T, C, V, LN> Default for DBCache<T, C, V, LN>
where
    T: Clone + Truthy + Sphery + Statey,
    C: Clone + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    fn default() -> Self {
        Self {
            truthy_cache: Default::default(),
            county_cache: Default::default(),
            dependencies: Default::default(),
        }
    }
}

impl<'a, D, L, V, T, C, R> Database<'a, D, L, V, T, C, R> for DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Clone + Truthy + Sphery + Statey,
    C: Clone + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    type Err = ();

    fn initialize(
        shuffles: HashMap<&'a str, R>,
        logic: L,
        descriptors_keysy: HashMap<&'a str, (&'a str, &'a str)>,
        descriptors_truthy: HashMap<&'a str, Vec<D>>,
        descriptors_county: HashMap<&'a str, Vec<D>>,
    ) -> Self {
        Self {
            shuffles,
            logic,
            descriptors_keysy,
            descriptors_truthy,
            descriptors_county,
            cache: Default::default(),
        }
    }

    fn query_descriptor_truthy(&self, name: &str, values: &[V]) -> Result<T, Self::Err> {
        self.query_inner_truthy(
            &Query::Descriptor(name.into(), values.into()),
            &mut HashSet::new(),
        )
    }

    fn query_descriptor_county(&self, name: &str, values: &[V]) -> Result<C, Self::Err> {
        match self.descriptors_county.get(name) {
            Some(rules) => Ok(rules
                .iter()
                .map(|d| d.eval(values, |_| unreachable!(), |_| todo!()))
                .fold(C::bottom(), |a, b| a.join(&b))),
            None => Err(()),
        }
    }

    fn query_logic_node(&self, ln: &L::Node) -> Result<T, Self::Err> {
        self.query_inner_truthy(&Query::Node(ln.clone()), &mut HashSet::new())
    }

    fn query_access(&self, name: &str, values: &[V]) -> Result<T, Self::Err> {
        self.query_inner_truthy(
            &Query::Access(name.into(), values.into()),
            &mut HashSet::new(),
        )
    }

    fn mod_shuffle(&mut self, delta: &R::Delta) {}
}

impl<'a, D, L, V, T, C, R> DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Clone + Hash + Eq,
    V: Hash + Eq + Clone,
    T: Clone + Truthy + Sphery + Statey,
    C: Clone + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    fn query_inner_truthy(
        &self,
        query: &Query<V, L::Node>,
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<T, ()> {
        // Can't depend on itself
        if visited.contains(query) {
            return Ok(T::bottom());
        }

        match self.cache.query_truthy(query) {
            Some(t) => Ok(t),
            None => {
                visited.insert(query.clone());
                let value = match query {
                    Query::Descriptor(name, values) => {
                        self.eval_descriptor_ref_truthy(&name, values, visited)
                    }
                    Query::Access(name, values) => self.eval_access(name, values, visited),
                    Query::Node(node) => self.eval_node(node, visited),
                }?;
                let query = visited.take(query).unwrap();
                self.cache.set_truthy(query, value.clone());
                Ok(value)
            }
        }
    }

    fn eval_node(&self, node: &L::Node, visited: &mut HashSet<Query<V, L::Node>>) -> Result<T, ()> {
        let mut access = T::bottom();
        for edge in self.logic.edges(node) {
            let edge_access =
                self.eval_descriptor(edge.descriptor, &[], |t| Ok(t), |_| Err(()), visited)?;

            let node_access = match &edge.ty {
                EdgeTy::FromTrue => T::top(),
                EdgeTy::FromNode(source) => {
                    self.query_inner_truthy(&Query::Node(source.clone()), visited)?
                }
            };

            access = access.join(&edge_access).join(&node_access);
        }

        Ok(access.increment())
    }

    fn eval_access(
        &self,
        name: &str,
        values: &[V],
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<T, ()> {
        let mut access = T::bottom();
        for node in self.logic.access_nodes(name, values) {
            access = access.join(&self.eval_node(&node, visited)?)
        }

        Ok(access.increment())
    }

    fn eval_descriptor_ref_truthy(
        &self,
        descriptor: &str,
        values: &[V],
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<T, ()> {
        match self.descriptors_truthy.get(descriptor) {
            Some(descriptors) => {
                let mut value = T::bottom();
                for descriptor in descriptors {
                    value = value.join(&self.eval_descriptor(
                        descriptor,
                        values,
                        |t| Ok(t),
                        |_| Err(()),
                        visited,
                    )?);
                }
                Ok(value)
            }
            None => Err(()),
        }
    }

    fn eval_descriptor<D1: Descriptor<V>, R1>(
        &self,
        descriptor: &D1,
        values: &[V],
        truthy: impl Fn(T) -> Result<R1, ()>,
        county: impl Fn(C) -> Result<R1, ()>,
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<R1, ()> {
        let visited = RefCell::new(visited);
        descriptor.eval(
            values, 
            |t| self.eval_descriptor_truthy::<D1>(t, &mut visited.borrow_mut())
                .and_then(&truthy),
            |c| self.eval_descriptor_county::<D1>(c, &mut visited.borrow_mut())
                .and_then(&county)
        )
    }

    fn eval_descriptor_truthy<D1: Descriptor<V>>(
        &self,
        t: &D1::Truthy,
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<T, ()> {
        let visited = RefCell::new(visited);
        let oolean = |ool| match ool {
            Oolean::False => Ok(T::bottom()),
            Oolean::Ool => Ok(T::ool()),
            Oolean::True => Ok(T::top()),
        };
        let reference = |name: &str, values: &[_]| {
            let query = Query::Descriptor(name.into(), values.into());
            self.query_inner_truthy(&query, &mut visited.borrow_mut())
        };
        let access = |name: &str, values: &[_]| {
            let query = Query::Access(name.into(), values.into());
            self.query_inner_truthy(&query, &mut visited.borrow_mut())
        };
        let compare = |c: &D1::County, n: Ntgr| {
            self.eval_descriptor_county::<D1>(c, &mut visited.borrow_mut())
                .map(|c| c.ge(n))
        };
        let exists = |r: Relation, v: &V, f: &dyn Fn(&_) -> _| todo!();
        let conj = |a: &_, b: &_| {
            let a = self.eval_descriptor_truthy::<D1>(a, &mut visited.borrow_mut())?;
            let b = self.eval_descriptor_truthy::<D1>(b, &mut visited.borrow_mut())?;
            Ok(a.meet(&b))
        };
        let disj = |a: &_, b: &_| {
            let a = self.eval_descriptor_truthy::<D1>(a, &mut visited.borrow_mut())?;
            let b = self.eval_descriptor_truthy::<D1>(b, &mut visited.borrow_mut())?;
            Ok(a.join(&b))
        };
        let prior = |_: &_| todo!();
        let posterior = |_: &_| todo!();

        D1::eval_truthy(
            t, oolean, reference, access, compare, exists, conj, disj, prior, posterior,
        )
    }

    fn eval_descriptor_county<D1: Descriptor<V>>(
        &self,
        c: &D1::County,
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<C, ()> {
        let visited = RefCell::new(visited);
        let ntgr = |n| Ok(C::lift(&T::top()).scale(n));
        let reference = |name: &str, values: &[_]| {
            let query = Query::Descriptor(name.into(), values.into());
            self.query_inner_county(&query, &mut visited.borrow_mut())
        };
        let comb = |a: &_, n, b: &_| {
            let a = self.eval_descriptor_county::<D1>(a, &mut visited.borrow_mut())?;
            let b = self.eval_descriptor_county::<D1>(b, &mut visited.borrow_mut())?;
            Ok(a.scale(n).add(&b))
        };
        let min = |a: &_, b: &_| {
            let a = self.eval_descriptor_county::<D1>(a, &mut visited.borrow_mut())?;
            let b = self.eval_descriptor_county::<D1>(b, &mut visited.borrow_mut())?;
            Ok(a.meet(&b))
        };
        let max = |a: &_, b: &_| {
            let a = self.eval_descriptor_county::<D1>(a, &mut visited.borrow_mut())?;
            let b = self.eval_descriptor_county::<D1>(b, &mut visited.borrow_mut())?;
            Ok(a.join(&b))
        };
        let count = |r: Relation, v: &_, f: &dyn Fn(&_) -> _| todo!();

        D1::eval_county(c, ntgr, reference, comb, min, max, count)
    }

    fn query_inner_county(
        &self,
        query: &Query<V, L::Node>,
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<C, ()> {
        if visited.contains(&query) {
            return Ok(C::bottom());
        }

        match self.cache.query_county(query) {
            Some(t) => Ok(t),
            None => {
                visited.insert(query.clone());
                let value = match query {
                    Query::Descriptor(name, values) => {
                        self.eval_descriptor_ref_county(&name, values, visited)
                    }
                    _ => return Err(()),
                }?;
                let query = visited.take(query).unwrap();
                self.cache.set_county(query, value.clone());
                Ok(value)
            }
        }
    }

    fn eval_descriptor_ref_county(
        &self,
        descriptor: &str,
        values: &[V],
        visited: &mut HashSet<Query<V, L::Node>>,
    ) -> Result<C, ()> {
        match self.descriptors_truthy.get(descriptor) {
            Some(descriptors) => {
                let mut value = C::bottom();
                for descriptor in descriptors {
                    value = value.join(&self.eval_descriptor(
                        descriptor,
                        values,
                        |_| Err(()),
                        |c| Ok(c),
                        visited,
                    )?);
                }
                Ok(value)
            }
            None => Err(()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Query<V: Hash + Eq + Clone, LN: Hash + Eq + Clone> {
    Descriptor(String, Vec<V>),
    Node(LN),
    Access(String, Vec<V>),
}
