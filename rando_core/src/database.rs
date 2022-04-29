use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use either::Either;

use crate::{
    algebra::{County, Sphery, Truthy},
    descriptor::{DescRef, Descriptor, Logic},
    shuffles::{Shuffle, ShuffleDelta, ShufflePattern},
    statey::{EdgeStatey, Statey},
};

use self::descriptor_eval::DatabaseDescriptorEval;

mod descriptor_eval;
mod update;

#[cfg(any())]
#[cfg(test)]
mod test;

pub trait Database<D, L, R, K, V, T, C>
where
    D: Descriptor<K, V>,
    L: Logic<K, V>,
    R: Shuffle<V, V>,
    K: Hash + Eq,
    V: Hash + Eq,
    T: Truthy + Sphery,
    C: County<T> + Sphery,
{
    type Err;

    fn initialize(
        shuffles: HashMap<K, R>,
        logic: L,
        locks: HashSet<K>,
        descriptors: HashMap<K, D>,
    ) -> Self;

    fn query_descriptor_truthy(
        &self,
        reference: &DescRef<K, V>,
    ) -> Result<EdgeStatey<T, V>, Self::Err>;
    fn query_descriptor_county(
        &self,
        reference: &DescRef<K, V>,
    ) -> Result<EdgeStatey<C, V>, Self::Err>;
    fn query_logic_node(&self, ln: &L::Node) -> Result<Statey<T, V>, Self::Err>;
    fn query_edge(&self, edge: &Edge<L::Node>) -> Result<EdgeStatey<T, V>, Self::Err>;
    fn query_access(&self, reference: &DescRef<K, V>) -> Result<Statey<T, V>, Self::Err>;

    fn mod_shuffle(&mut self, shuffle: &K, delta: &R::Delta) -> Result<(), Self::Err>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DBImplErr {
    Type,
    UnknownShuffle,
    UnknownDescriptor,
    Uncached,
    CountyCycle,
}

// edge -> node (implied)
// node -> node, access
// access -> descriptor
// descriptor -> descriptor, edge

pub struct DBImpl<D, L, R, K, V, T, C>
where
    L: Logic<K, V>,
{
    data: DBData<D, L, R, K, V>,
    cache: DBCache<L::Node, K, V, T, C, DBImplErr>,
}

#[derive(Clone, Debug)]
struct DBData<D, L, R, K, V>
where
    L: Logic<K, V>,
{
    shuffles: HashMap<K, R>,
    logic: L,
    locks: HashSet<K>,
    descriptors: HashMap<K, D>,
    node_node_dependants: HashMap<L::Node, Vec<L::Node>>,
    descriptor_dependants: HashMap<DescRef<K, V>, Vec<(Option<L::Node>, L::Node)>>,
}

type Edge<LN> = (Option<LN>, LN);
type NodeValue<T, V, E = DBImplErr> = Result<Statey<T, V>, E>;
type EdgeValue<T, V, E = DBImplErr> = Result<EdgeStatey<T, V>, E>;
type DescValue<T, C, V, E = DBImplErr> = Result<Either<EdgeStatey<T, V>, EdgeStatey<C, V>>, E>;

#[derive(Default)]
struct DBCache<LN, K, V, T, C, E> {
    node_value: HashMap<LN, NodeValue<T, V, E>>,
    edge_value: HashMap<Edge<LN>, EdgeValue<T, V, E>>,
    access_value: HashMap<DescRef<K, V>, NodeValue<T, V, E>>,
    descriptor_value: HashMap<DescRef<K, V>, DescValue<T, C, V, E>>,
    shuffle_dependants: HashMap<K, HashMap<ShufflePattern<V, V>, HashSet<DescRef<K, V>>>>,
    dynamic_descriptor_dependants: HashMap<DescRef<K, V>, HashSet<DescRef<K, V>>>,
    node_access_dependants: HashMap<LN, HashSet<DescRef<K, V>>>,
    access_dependants: HashMap<DescRef<K, V>, HashSet<DescRef<K, V>>>,
}

impl<LN, K, V, T, C, E> DBCache<LN, K, V, T, C, E> {
    fn default() -> Self {
        DBCache {
            node_value: Default::default(),
            edge_value: Default::default(),
            access_value: Default::default(),
            descriptor_value: Default::default(),
            shuffle_dependants: Default::default(),
            dynamic_descriptor_dependants: Default::default(),
            node_access_dependants: Default::default(),
            access_dependants: Default::default(),
        }
    }
}

impl<D, L, R, K, V, T, C> Database<D, L, R, K, V, T, C> for DBImpl<D, L, R, K, V, T, C>
where
    D: Descriptor<K, V>,
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    R: Shuffle<V, V>,
{
    type Err = DBImplErr;

    fn initialize(
        shuffles: HashMap<K, R>,
        logic: L,
        locks: HashSet<K>,
        descriptors: HashMap<K, D>,
    ) -> Self {
        let node_node_dependants = logic
            .list_nodes()
            .flat_map(|dest| {
                logic
                    .adjacent_nodes(dest.clone())
                    .filter_map(move |src| Some((src?, dest.clone())))
            })
            .fold(HashMap::new(), |mut map, (src, dest)| {
                map.entry(src).or_insert_with(Vec::new).push(dest);
                map
            });

        let descriptor_edge_dependants = logic
            .list_nodes()
            .flat_map(|dest| {
                logic
                    .adjacent_nodes(dest.clone())
                    .map(move |src| (src, dest.clone()))
            })
            .fold(HashMap::<_, Vec<Edge<L::Node>>>::new(), |mut map, edge| {
                logic.eval_edge(
                    edge.clone(),
                    |k| {
                        if let Some(deps) = map.get_mut(k) {
                            deps.push(edge.clone());
                        } else {
                            map.insert(k.clone(), vec![edge.clone()]);
                        }
                    },
                    |_| (),
                    |_| (),
                );
                map
            });

        let queue = descriptor_edge_dependants
            .keys()
            .cloned()
            .collect::<Vec<_>>();

        let mut cache = DBCache::default();

        let data = DBData {
            shuffles,
            logic,
            locks,
            descriptors,
            node_node_dependants,
            descriptor_dependants: descriptor_edge_dependants,
        };

        update::update_queue(&data, &mut cache, queue);

        Self { data, cache }
    }

    fn query_descriptor_truthy(&self, reference: &DescRef<K, V>) -> EdgeValue<T, V> {
        self.cache
            .descriptor_value
            .get(reference)
            .cloned()
            .unwrap_or_else(|| Err(DBImplErr::Uncached))
            .and_then(|v| v.left().ok_or(DBImplErr::Type))
    }

    fn query_descriptor_county(&self, reference: &DescRef<K, V>) -> EdgeValue<C, V> {
        self.cache
            .descriptor_value
            .get(reference)
            .cloned()
            .unwrap_or_else(|| Err(DBImplErr::Uncached))
            .and_then(|v| v.right().ok_or(DBImplErr::Type))
    }

    fn query_logic_node(&self, ln: &L::Node) -> Result<Statey<T, V>, Self::Err> {
        self.cache
            .node_value
            .get(ln)
            .cloned()
            .unwrap_or_else(|| Ok(Statey::bottom()))
    }

    fn query_edge(&self, edge: &Edge<L::Node>) -> Result<EdgeStatey<T, V>, Self::Err> {
        self.cache
            .edge_value
            .get(edge)
            .cloned()
            .unwrap_or_else(|| Ok(EdgeStatey::bottom()))
    }

    fn query_access(&self, reference: &DescRef<K, V>) -> Result<Statey<T, V>, Self::Err> {
        self.cache
            .access_value
            .get(reference)
            .cloned()
            .unwrap_or_else(|| Err(DBImplErr::Uncached))
    }

    fn mod_shuffle(&mut self, name: &K, delta: &R::Delta) -> Result<(), Self::Err> {
        let shuffle = self
            .data
            .shuffles
            .get(name)
            .ok_or(DBImplErr::UnknownShuffle)?;
        if let Some(dependants) = self.cache.shuffle_dependants.get(&name) {
            let updates = dependants
                .iter()
                .filter_map(|(pat, keys)| delta.affects(pat, shuffle).then(|| keys))
                .flatten()
                .cloned()
                .collect::<Vec<_>>();

            update::update_queue(&self.data, &mut self.cache, updates);
        }

        Ok(())
    }
}

fn eval_access<'a, T, E, K, V, L>(
    reference: &DescRef<K, V>,
    logic: &L,
    mut node_cache: impl FnMut(L::Node) -> Result<Statey<T, V>, E>,
) -> Result<Statey<T, V>, E>
where
    L: Logic<K, V>,
    T: Truthy + PartialOrd + Clone + 'a,

    V: Clone + Hash + Eq + 'a,
{
    TryIterator::new(logic.access_nodes(reference).map(|s| node_cache(s)))
        .try_pass(|i| Statey::or_all(i))
}

fn eval_node<'a, T, E, K, V, L>(
    node: L::Node,
    logic: &L,
    node_cache: impl Fn(L::Node) -> Result<&'a Statey<T, V>, E>,
    edge_cache: impl Fn(Edge<L::Node>) -> Result<&'a EdgeStatey<T, V>, E>,
) -> Result<Statey<T, V>, E>
where
    T: Truthy + Sphery + PartialOrd + Clone + 'a,
    L: Logic<K, V>,
    L::Node: Clone,
    V: Clone + Hash + Eq + 'a,
{
    TryIterator::new(logic.adjacent_nodes(node.clone()).map(|s| -> Result<_, E> {
        let source_truthy = s
            .as_ref()
            .map(|s| node_cache(s.clone()).map(Cow::Borrowed))
            .transpose()
            .map(|o| o.unwrap_or_else(|| Cow::Owned(Statey::lift(T::top()))));
        Ok(source_truthy?.and_edge(edge_cache((s, node.clone()))?))
    }))
    .try_pass(|i| Statey::or_all(i.map(|e| e.map(Sphery::increment))))
}

fn eval_edge<'a, T, E, K, V, L>(
    edge: Edge<L::Node>,
    logic: &L,
    descriptor_cache: impl Fn(&DescRef<K, V>) -> Result<&'a EdgeStatey<T, V>, E>,
) -> Result<EdgeStatey<T, V>, E>
where
    L: Logic<K, V>,
    T: Truthy + PartialOrd + Clone + 'a,
    V: Clone + Hash + Eq + 'a,
{
    logic
        .eval_edge(
            edge,
            |k| descriptor_cache(k).map(Cow::Borrowed),
            |r| {
                r.into_iter()
                    .try_fold(EdgeStatey::lift(T::top()), |a, b| Ok(a.and(b?.as_ref())))
                    .map(Cow::Owned)
            },
            |r| {
                r.into_iter()
                    .map(|r| r.map(Cow::into_owned))
                    .collect::<Result<_, _>>()
                    .map(Cow::Owned)
            },
        )
        .map(Cow::into_owned)
}

fn eval_descriptor<'a, T, C, K, V, R, D, F>(
    descriptor: &D,
    values: &[V],
    descriptor_cache: F,
    shuffles: impl FnMut(&K, &ShufflePattern<V, V>) -> Option<&'a R> + 'a,
    access_cache: impl FnMut(&DescRef<K, V>) -> Result<Statey<T, V>, DBImplErr>,
) -> DescValue<T, C, V>
where
    R: Shuffle<V, V> + 'a,
    F: FnMut(
        &DescRef<K, V>,
    ) -> Result<Option<Either<EdgeStatey<T, V>, EdgeStatey<C, V>>>, DBImplErr>,
    D: Descriptor<K, V>,
    T: Truthy + PartialOrd + Clone + 'a,
    C: County<T> + PartialOrd + Clone + 'a,
    K: Hash + Eq,
    V: Clone + Hash + Eq + 'a,
{
    let mut eval = DatabaseDescriptorEval {
        access_cache,
        descriptor_cache,
        shuffles,
    };
    descriptor.eval(values, &mut eval)
}

/// TODO: Move this somewhere that makes more sense
struct TryIterator<I, E>(Result<I, E>);

impl<T, E, I> TryIterator<I, E>
where
    I: Iterator<Item = Result<T, E>>,
{
    fn new(i: I) -> Self {
        Self(Ok(i))
    }

    fn try_pass<O, F>(mut self, f: F) -> Result<O, E>
    where
        I: IntoIterator<Item = Result<T, E>>,
        F: FnOnce(&mut Self) -> O,
    {
        let value = f(&mut self);
        self.0.map(|_| value)
    }
}

impl<T, E, I> Iterator for TryIterator<I, E>
where
    I: Iterator<Item = Result<T, E>>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            Ok(i) => {
                let result = i.next()?;
                match result {
                    Ok(t) => Some(t),
                    Err(e) => {
                        self.0 = Err(e);
                        None
                    }
                }
            }
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            Ok(i) => (0, i.size_hint().1),
            Err(_) => (0, Some(0)),
        }
    }
}
