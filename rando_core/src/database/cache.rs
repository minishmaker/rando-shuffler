use crate::{
    algebra::{County, Sphery, Statey, Truthy},
    shuffles::{ShuffleDelta, ShufflePattern},
};
use either::Either;
use petgraph::graph::{self, DiGraph};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    rc::Rc,
};

use super::{query::DescriptorType, Query};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CacheRef(NodeIndex);

type NodeIndex = graph::NodeIndex<u32>;

#[derive(Clone, Debug)]
pub struct DBCache<'a, T, C, V, LN, E>
where
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
    E: Clone,
{
    cache: Vec<Option<Result<Either<T, C>, E>>>,
    dependencies: DiGraph<Rc<Query<'a, V, LN>>, ()>,
    query_nodes: HashMap<Rc<Query<'a, V, LN>>, NodeIndex>,
    patterns: HashMap<String, HashMap<ShufflePattern<V, V>, HashSet<NodeIndex>>>,
    cycles: HashMap<NodeIndex, Vec<NodeIndex>>,
}

impl<'a, T, C, V, LN, E> DBCache<'a, T, C, V, LN, E>
where
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
    E: Clone,
{
    pub fn register(
        &mut self,
        query: &Query<V, LN>,
        parent: Option<CacheRef>,
    ) -> Result<Result<Either<T, C>, E>, CacheRef> {
        match self.query_nodes.get(query) {
            Some(n) => {
                parent.map(|p| self.dependencies.update_edge(*n, p.0, ()));
                match &self.cache[n.index()] {
                    Some(v) => Ok(v.clone()),
                    None => {
                        // The cache isn't set, so either it's a cycle or something wasn't set
                        let parent = parent.expect("Top-level cache entry registered but not set");
                        self.cycles.entry(*n).or_default().push(parent.0);
                        Ok(Ok(query.expected_type().make_bottom()))
                    }
                }
            }
            None => {
                let n = self.register_inner(query);
                parent.map(|p| self.dependencies.update_edge(n.0, p.0, ()));
                Err(n)
            }
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

    pub fn set_cache(
        &mut self,
        cache_ref: CacheRef,
        value: Result<Either<T, C>, E>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) {
        self.cache[cache_ref.0.index()] = Some(value);

        if let Some(cycles) = self.cycles.remove(&cache_ref.0) {
            self.increase_dependants(cycles.into(), eval);
        }
    }

    pub fn add_shuffle_dependency(
        &mut self,
        shuffle: String,
        pattern: ShufflePattern<V, V>,
        cache_ref: CacheRef,
    ) {
        let patterns = self.patterns.entry(shuffle).or_default();
        patterns.entry(pattern).or_default().insert(cache_ref.0);
    }

    fn cached_patterns<'b>(
        &'b self,
        shuffle: &str,
    ) -> impl Iterator<Item = (&'b ShufflePattern<V, V>, &'b HashSet<NodeIndex>)> + 'b {
        self.patterns
            .get(shuffle)
            .into_iter()
            .flat_map(|l| l.iter())
    }

    pub fn mod_shuffle<D: ShuffleDelta<V, V>>(
        &mut self,
        shuffle: &str,
        delta: &D,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) {
        let update_queue = self
            .cached_patterns(shuffle)
            .filter(|(p, _)| delta.affects(p))
            .flat_map(|(_, n)| n.iter())
            .copied()
            .collect::<VecDeque<_>>();

        self.update_dependants(update_queue, eval)
    }

    fn increase_dependants(
        &mut self,
        mut update_queue: VecDeque<NodeIndex>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) {
        while let Some(n) = update_queue.pop_front() {
            let old = self.cache[n.index()]
                .take()
                .expect("Attempted to update dependencies while cache is unevaluated");

            let new = eval(CacheRef(n), self);

            let (value, change) = updated_value(old, new);
            self.cache[n.index()] = Some(value);
            if let Some(late) = change {
                update_queue.extend(self.dependencies.neighbors(n));
                assert!(late.is_none(), "Attempted to decrease cached value")
            }
        }
    }

    fn update_dependants(
        &mut self,
        mut update_queue: VecDeque<NodeIndex>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) {
        let mut late_update = Vec::new();

        while let Some(n) = update_queue.pop_front() {
            let old = self.cache[n.index()]
                .take()
                .expect("Attempted to update dependencies while cache is unevaluated");

            let new = eval(CacheRef(n), self);

            let (value, change) = updated_value(old, new);
            self.cache[n.index()] = Some(value);
            if let Some(late) = change {
                update_queue.extend(self.dependencies.neighbors(n));
                if let Some(late) = late {
                    late_update.push((n, late));
                }
            }
        }

        for (n, v) in late_update {
            self.cache[n.index()] = Some(Ok(v));
            update_queue.extend(self.dependencies.neighbors(n));
        }

        self.increase_dependants(update_queue, eval)
    }
}

/// Finds the new value for the cache, given the old and new values.
/// If new is unchanged from old, returns None
/// If new is strictly greater than old, returns Some(None)
/// If new otherwise does not equal old, returns Some(Some(new))
fn updated_value<T, C, E>(
    old: Result<Either<T, C>, E>,
    new: Result<Either<T, C>, E>,
) -> (Result<Either<T, C>, E>, Option<Option<Either<T, C>>>)
where
    T: PartialEq + PartialOrd + Truthy,
    C: PartialEq + PartialOrd + County<T>,
{
    if old.is_err() || new.is_err() {
        // err -> ok is always increase
        // ok -> err is always decrease to bottom
        return (new, Some(None));
    }

    if let (Ok(old), Ok(new)) = (old, new) {
        if new > old {
            (Ok(new), Some(None))
        } else if new != old {
            let bottom = DescriptorType::get_type(&new).make_bottom();
            (Ok(bottom), Some(Some(new)))
        } else {
            (Ok(new), None)
        }
    } else {
        unreachable!()
    }
}

impl<T, C, V, LN, E> Default for DBCache<'_, T, C, V, LN, E>
where
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
    E: Clone,
{
    fn default() -> Self {
        Self {
            cache: Default::default(),
            dependencies: Default::default(),
            query_nodes: Default::default(),
            patterns: Default::default(),
            cycles: Default::default(),
        }
    }
}
