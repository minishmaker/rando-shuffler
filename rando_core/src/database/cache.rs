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

use super::Query;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CacheRef(NodeIndex);

type NodeIndex = graph::NodeIndex<u32>;

#[derive(Clone, Debug)]
pub struct DBCache<'a, T, C, V, LN>
where
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    cache: Vec<Option<Either<T, C>>>,
    dependencies: DiGraph<Rc<Query<'a, V, LN>>, ()>,
    query_nodes: HashMap<Rc<Query<'a, V, LN>>, NodeIndex>,
    patterns: HashMap<String, HashMap<ShufflePattern<V, V>, HashSet<NodeIndex>>>,
    cycles: HashMap<NodeIndex, HashSet<NodeIndex>>,
}

impl<'a, T, C, V, LN> DBCache<'a, T, C, V, LN>
where
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
{
    pub fn register(
        &mut self,
        query: &Query<V, LN>,
        parent: Option<CacheRef>,
    ) -> Result<Either<T, C>, CacheRef> {
        match self.query_nodes.get(query) {
            Some(n) => {
                parent.map(|p| self.dependencies.update_edge(*n, p.0, ()));
                match &self.cache[n.index()] {
                    Some(v) => Ok(v.clone()),
                    None => {
                        // The cache isn't set, so either it's a cycle or the
                        let parent = parent.expect("Cache entry registered but not set");
                        self.cycles.entry(*n).or_default().insert(parent.0);
                        Ok(query.expected_type().make_bottom())
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

    pub fn cached_value(&self, cache_ref: CacheRef) -> Option<Either<T, C>> {
        self.cache[cache_ref.0.index()].clone()
    }

    pub fn set_cache<E>(
        &mut self,
        cache_ref: CacheRef,
        value: Either<T, C>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) -> Result<(), E> {
        self.cache[cache_ref.0.index()] = Some(value);

        if let Some(cycles) = self.cycles.get(&cache_ref.0) {
            let cycles = cycles.iter().copied().collect();
            self.update_dependants(cycles, eval)?;
        }

        Ok(())
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

    pub fn mod_shuffle<E, D: ShuffleDelta<V, V>>(
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

        self.update_dependants(update_queue, eval)
    }

    fn update_dependants<E>(
        &mut self,
        mut update_queue: VecDeque<NodeIndex>,
        eval: impl Fn(CacheRef, &mut Self) -> Result<Either<T, C>, E>,
    ) -> Result<(), E> {
        let mut late_queue = Vec::new();

        while let Some(n) = update_queue.pop_front() {
            let old = self.cache[n.index()]
                .clone()
                .expect("Attempted to modify shuffle with unevaluated cache!");

            // Invalidate cache entry
            self.cache[n.index()] = None;
            let new = eval(CacheRef(n), self)?;

            if new > old {
                self.cache[n.index()] = Some(new);
                update_queue.extend(self.dependencies.neighbors(n));
            } else if new != old {
                self.cache[n.index()] = if new.is_left() {
                    Some(Either::Left(T::bottom()))
                } else {
                    Some(Either::Right(C::bottom()))
                };

                late_queue.push((n, new));
                update_queue.extend(self.dependencies.neighbors(n));
            } else {
                self.cache[n.index()] = Some(old);
            }
        }

        for (n, v) in late_queue {
            self.cache[n.index()] = Some(v);
            update_queue.extend(self.dependencies.neighbors(n));
        }

        while let Some(n) = update_queue.pop_front() {
            let new = eval(CacheRef(n), self)?;
            let old = self.cache[n.index()]
                .as_ref()
                .expect("Attempted to modify shuffle with unevaluated cache!");

            if &new > old {
                self.cache[n.index()] = Some(new);
                update_queue.extend(self.dependencies.neighbors(n));
            }
        }

        Ok(())
    }
}

impl<T, C, V, LN> Default for DBCache<'_, T, C, V, LN>
where
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    V: Clone + Hash + Eq,
    LN: Clone + Hash + Eq,
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
