use crate::{
    algebra::{County, Sphery, Statey, Truthy},
    shuffles::{ShuffleDelta, ShufflePattern},
};
use either::Either;
use petgraph::graph::{self, DiGraph};
use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
    rc::Rc,
};

use super::Query;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CacheRef(NodeIndex);

type NodeIndex = graph::NodeIndex<u32>;

#[derive(Clone, Debug)]
pub struct DBCache<'a, T, C, V, LN>
where
    T: Clone + Eq + Truthy + Sphery + Statey,
    C: Clone + Eq + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    cache: Vec<Option<Either<T, C>>>,
    dependencies: DiGraph<Rc<Query<'a, V, LN>>, ()>,
    query_nodes: HashMap<Rc<Query<'a, V, LN>>, NodeIndex>,
    patterns: HashMap<String, HashMap<ShufflePattern<V, V>, Vec<NodeIndex>>>,
}

impl<'a, T, C, V, LN> DBCache<'a, T, C, V, LN>
where
    T: Clone + Eq + Truthy + Sphery + Statey,
    C: Clone + Eq + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    pub fn register(&mut self, query: &Query<V, LN>) -> CacheRef {
        match self.query_nodes.get(query) {
            Some(&n) => CacheRef(n),
            None => self.register_inner(query),
        }
    }

    fn register_inner(&mut self, query: &Query<V, LN>) -> CacheRef {
        let query = Rc::new(query.upgrade());
        let index = self.dependencies.add_node(Rc::clone(&query));
        assert_eq!(index.index(), self.cache.len());

        self.query_nodes.insert(query, index);
        self.cache.push(None);

        CacheRef(index)
    }

    pub fn get_query(&self, cache_ref: CacheRef) -> Rc<Query<'a, V, LN>> {
        Rc::clone(&self.dependencies[cache_ref.0])
    }

    pub fn cached_value(&self, cache_ref: CacheRef) -> Option<Either<T, C>> {
        self.cache[cache_ref.0.index()].clone()
    }

    pub fn set_cache(&mut self, cache_ref: CacheRef, value: Either<T, C>) {
        self.cache[cache_ref.0.index()] = Some(value);
    }

    pub(super) fn add_dependency(&mut self, from: CacheRef, to: CacheRef) {
        self.dependencies.update_edge(to.0, from.0, ());
    }

    fn cached_patterns<'b>(
        &'b self,
        shuffle: &str,
    ) -> impl Iterator<Item = (&'b ShufflePattern<V, V>, &'b [NodeIndex])> + 'b {
        self.patterns
            .get(shuffle)
            .into_iter()
            .flat_map(|l| l.iter())
            .map(|(a, b)| (a, &b[..]))
    }

    pub(super) fn mod_shuffle<E, D: ShuffleDelta<V, V>>(
        &mut self,
        shuffle: &str,
        delta: &D,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) -> Result<(), E> {
        let update_queue = self
            .cached_patterns(shuffle)
            .filter(|(p, _)| delta.affects(p))
            .flat_map(|(_, n)| n.iter())
            .copied()
            .collect::<VecDeque<_>>();

        if delta.is_destructive() == false {
            self.update_constructive(update_queue, eval)
        } else {
            self.update_destructive(update_queue, eval)
        }
    }

    fn update_constructive<E>(
        &mut self,
        mut update_queue: VecDeque<NodeIndex>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) -> Result<(), E> {
        while let Some(n) = update_queue.pop_front() {
            let new = eval(CacheRef(n), self)?;
            let old = self.cache[n.index()]
                .as_ref()
                .expect("Attempted to modify shuffle with unevaluated cache!");

            if &new != old {
                self.cache[n.index()] = Some(new);
                update_queue.extend(self.dependencies.neighbors(n));
            }
        }

        Ok(())
    }

    fn update_destructive<E>(
        &mut self,
        mut update_queue: VecDeque<NodeIndex>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) -> Result<(), E> {
        while let Some(n) = update_queue.pop_front() {
            let new = eval(CacheRef(n), self)?;
            let old = self.cache[n.index()]
                .as_ref()
                .expect("Attempted to modify shuffle with unevaluated cache!");

            todo!()
        }

        Ok(())
    }
}

impl<T, C, V, LN> Default for DBCache<'_, T, C, V, LN>
where
    T: Clone + Eq + Truthy + Sphery + Statey,
    C: Clone + Eq + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    fn default() -> Self {
        Self {
            cache: Default::default(),
            dependencies: Default::default(),
            query_nodes: Default::default(),
            patterns: Default::default(),
        }
    }
}
