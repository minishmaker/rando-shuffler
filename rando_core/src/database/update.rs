use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    rc::Rc,
};

use either::Either;

use crate::{
    algebra::{County, Sphery, Truthy},
    descriptor::{DescRef, Descriptor, Logic},
    shuffles::Shuffle,
    statey::{EdgeStatey, Statey},
};

use super::{DBCache, DBData, DBImplErr, DescValue, Edge};

pub(super) fn update_queue<'a, D, L, R, K, V, T, C>(
    data: &DBData<D, L, R, K, V>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    queue: impl IntoIterator<Item = DescRef<K, V>>,
) where
    D: Descriptor<K, V>,
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    R: Shuffle<V, V>,
{
    let queue = queue
        .into_iter()
        .map(|r| Query::Descriptor(r))
        .collect::<UniqueQueue<_>>();

    update_queue_inner(data, cache, queue)
}

fn update_queue_inner<D, L, R, K, V, T, C, Q>(
    data: &DBData<D, L, R, K, V>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    mut queue: Q,
) where
    D: Descriptor<K, V>,
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    R: Shuffle<V, V>,
    Q: Queue<Item = Query<K, V, L::Node>>,
{
    while let Some(query) = queue.dequeue() {
        match query {
            Query::Access(r) => {
                update_access(r, data, cache, &mut queue);
            }
            Query::Node(n) => {
                update_node(n, data, cache, &mut queue);
            }
            Query::Edge(edge) => update_edge(data, edge, cache, &mut queue),
            Query::Descriptor(main_ref) => {
                descriptor::update_descriptors_recursive(data, main_ref, cache, &mut queue);
            }
        }
    }
}

fn update_edge<D, L, R, K, V, T, C, Q>(
    data: &DBData<D, L, R, K, V>,
    edge: Edge<L::Node>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    queue: &mut Q,
) where
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    Q: Queue<Item = Query<K, V, L::Node>>,
{
    let bottom = EdgeStatey::bottom();
    let new = super::eval_edge(edge.clone(), &data.logic, |k| {
        descriptor_cache_truthy(&cache.descriptor_value, &bottom, k)
    });
    let old = cache.edge_value.get(&edge);
    if Some(&new) != old {
        queue.enqueue(Query::Node(edge.1.clone()));
        if decreased(&new, old) {
            queue.enqueue(Query::Edge(edge.clone()));
            cache.edge_value.insert(edge, Ok(EdgeStatey::bottom()));
        } else {
            cache.edge_value.insert(edge, new);
        }
    }
}

mod descriptor;

fn update_access<D, L, R, K, V, T, C, Q>(
    r: DescRef<K, V>,
    data: &DBData<D, L, R, K, V>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    queue: &mut Q,
) where
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    Q: Queue<Item = Query<K, V, L::Node>>,
{
    let new = super::eval_access(&r, &data.logic, |n| {
        cache
            .node_access_dependants
            .entry(n.clone())
            .or_default()
            .insert(r.clone());
        cache
            .node_value
            .get(&n)
            .cloned()
            .unwrap_or_else(|| Ok(Statey::bottom()))
    });
    let old = cache.access_value.get(&r);
    if Some(&new) != old {
        let dependants = cache.access_dependants[&r].iter().cloned();
        queue.extend(dependants.map(|r| Query::Descriptor(r)));
        if decreased(&new, old) {
            queue.enqueue(Query::Access(r.clone()));
            cache.access_value.insert(r, Ok(Statey::bottom()));
        } else {
            cache.access_value.insert(r, new);
        }
    }
}

fn update_node<D, L, R, K, V, T, C, Q>(
    n: L::Node,
    data: &DBData<D, L, R, K, V>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    queue: &mut Q,
) where
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    Q: Queue<Item = Query<K, V, L::Node>>,
{
    let bottom_node = Statey::bottom();
    let bottom_edge = EdgeStatey::bottom();
    let new = super::eval_node(
        n.clone(),
        &data.logic,
        |n| {
            cache
                .node_value
                .get(&n)
                .map(|r| r.as_ref().map_err(DBImplErr::clone))
                .unwrap_or(Ok(&bottom_node))
        },
        |edge| {
            cache
                .edge_value
                .get(&edge)
                .map(|r| r.as_ref().map_err(DBImplErr::clone))
                .unwrap_or(Ok(&bottom_edge))
        },
    );

    let old = cache.node_value.get(&n);
    if Some(&new) != old {
        let node_dependants = data.node_node_dependants[&n].iter().cloned();
        let access_dependants = cache.node_access_dependants[&n].iter();

        if decreased(&new, old) {
            // Don't reevaluate self until after dependencies are reevaluated
            queue.extend(node_dependants.filter(|m| &n != m).map(|n| Query::Node(n)));
            queue.extend(access_dependants.cloned().map(|r| Query::Access(r)));
            queue.enqueue(Query::Node(n.clone()));
            cache.node_value.insert(n, Ok(Statey::bottom()));
        } else {
            queue.extend(node_dependants.map(|n| Query::Node(n)));
            queue.extend(
                access_dependants
                    .filter(|r| decreased(&new, cache.access_value.get(&r)))
                    .cloned()
                    .map(|r| Query::Access(r)),
            );
            cache.node_value.insert(n, new);
        }
    }
}

fn decreased<T, E>(new: &Result<T, E>, old: Option<&Result<T, E>>) -> bool
where
    T: PartialOrd,
{
    if let (Ok(new), Some(Ok(old))) = (new, old) {
        new < old
    } else {
        false
    }
}

fn descriptor_cache_truthy<'a, K: Clone + Hash + Eq, V: Clone + Hash + Eq, T, C>(
    cache: &'a HashMap<DescRef<K, V>, DescValue<T, C, V>>,
    bottom_truthy: &'a EdgeStatey<T, V>,
    k: &DescRef<K, V>,
) -> Result<&'a EdgeStatey<T, V>, DBImplErr> {
    descriptor_cache(cache, k)
        .transpose()
        .map(|r| r.and_then(|v| v.as_ref().left().ok_or(DBImplErr::Type)))
        .unwrap_or(Ok(&bottom_truthy))
}

fn descriptor_cache<'a: 'b, 'b, K: Clone + Hash + Eq, V: Clone + Hash + Eq, T, C>(
    cache: &'a HashMap<DescRef<K, V>, DescValue<T, C, V>>,
    k: &'b DescRef<K, V>,
) -> Result<Option<&'a Either<EdgeStatey<T, V>, EdgeStatey<C, V>>>, DBImplErr> {
    cache
        .get(k)
        .map(|r| r.as_ref().map_err(|e| e.clone()))
        .transpose()
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Query<K, V, LN> {
    Node(LN),
    Access(DescRef<K, V>),
    Edge(Edge<LN>),
    Descriptor(DescRef<K, V>),
}

trait Queue: Extend<Self::Item> {
    type Item;
    fn enqueue(&mut self, t: Self::Item);
    fn dequeue(&mut self) -> Option<Self::Item>;
}

impl<T> Queue for VecDeque<T> {
    type Item = T;

    fn enqueue(&mut self, t: Self::Item) {
        self.push_back(t);
    }

    fn dequeue(&mut self) -> Option<Self::Item> {
        self.pop_front()
    }
}

struct UniqueQueue<T> {
    set: HashSet<Rc<T>>,
    order: VecDeque<Rc<T>>,
}

impl<T> UniqueQueue<T> {
    fn new() -> Self {
        Self {
            set: HashSet::new(),
            order: VecDeque::new(),
        }
    }
}

impl<T: Hash + Eq> Queue for UniqueQueue<T> {
    type Item = T;
    fn enqueue(&mut self, t: Self::Item) {
        if !self.set.contains(&t) {
            let t = Rc::new(t);
            self.order.push_back(t.clone());
            self.set.insert(t);
        }
    }

    fn dequeue(&mut self) -> Option<Self::Item> {
        let removed = self.order.pop_front()?;
        self.set.remove(&removed);
        Rc::try_unwrap(removed).ok()
    }
}

impl<T: Hash + Eq> Extend<T> for UniqueQueue<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        iter.into_iter().for_each(|t| self.enqueue(t));
    }
}

impl<T: Hash + Eq> FromIterator<T> for UniqueQueue<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut this = Self::new();

        this.extend(iter);

        this
    }
}
