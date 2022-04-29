use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use either::Either;

use crate::{
    algebra::{County, Sphery, Truthy},
    database::{self, DBCache, DBData, DBImplErr, DescRef, DescValue, NodeValue},
    descriptor::{Descriptor, Logic},
    shuffles::Shuffle,
    statey::{EdgeStatey, Statey},
};

use super::{Query, Queue, UniqueQueue};

const COUNTY_UPDATE_LIMIT: usize = 10;

pub(super) fn update_descriptors_recursive<D, L, R, K, V, T, C, Q>(
    data: &DBData<D, L, R, K, V>,
    start: DescRef<K, V>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    queue: &mut Q,
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
    let mut update_counts = HashMap::new();
    let mut inner_queue = UniqueQueue::new();
    inner_queue.enqueue(start);

    while let Some(reference) = inner_queue.dequeue() {
        let new = update_descriptor(data, &reference, cache, queue, &mut inner_queue);
        let old = cache.descriptor_value.get(&reference);

        if Some(&new) != old {
            if let Some(edges) = data.descriptor_dependants.get(&reference) {
                queue.extend(edges.iter().cloned().map(Query::Edge));
            }

            if let Some(nodes) = cache.dynamic_descriptor_dependants.get(&reference) {
                inner_queue.extend(nodes.iter().cloned());
            }

            if super::decreased(&new, old) {
                inner_queue.enqueue(reference);
            } else {
                // Make sure county values don't loop too much
                if let Ok(Either::Right(_)) = &new {
                    let count = update_counts.entry(reference.clone()).or_insert(0);
                    if *count > COUNTY_UPDATE_LIMIT {
                        *cache.descriptor_value.get_mut(&reference).unwrap() =
                            Err(DBImplErr::CountyCycle);
                    } else {
                        *count += 1;
                    }
                }
            }
        }
    }
}

fn update_descriptor<D, L, R, K, V, T, C, Q1, Q2>(
    data: &DBData<D, L, R, K, V>,
    reference: &DescRef<K, V>,
    cache: &mut DBCache<L::Node, K, V, T, C, DBImplErr>,
    queue: &mut Q1,
    desc_queue: &mut Q2,
) -> DescValue<T, C, V>
where
    D: Descriptor<K, V>,
    L: Logic<K, V>,
    L::Node: Hash + Eq,
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + Sphery + PartialOrd + Clone,
    C: County<T> + Sphery + PartialOrd + Clone,
    R: Shuffle<V, V>,
    Q1: Queue<Item = Query<K, V, L::Node>>,
    Q2: Queue<Item = DescRef<K, V>>,
{
    let descriptor = data
        .descriptors
        .get(&reference.0)
        .ok_or(DBImplErr::UnknownDescriptor)?;

    database::eval_descriptor(
        descriptor,
        &reference.1,
        |r| {
            dependant_descriptor_cache(
                &reference,
                r,
                &cache.descriptor_value,
                &mut cache.dynamic_descriptor_dependants,
                desc_queue,
            )
        },
        |k, p| {
            let shuffle = data.shuffles.get(k)?;

            cache
                .shuffle_dependants
                .entry(k.clone())
                .or_default()
                .entry(p.clone())
                .or_default()
                .insert(reference.clone());

            Some(shuffle)
        },
        |r| {
            dependant_access_cache(
                &cache.access_value,
                &mut cache.access_dependants,
                &reference,
                queue,
                r,
            )
        },
    )
}

fn dependant_descriptor_cache<K, V, T, C, Q>(
    main_ref: &DescRef<K, V>,
    r: &DescRef<K, V>,
    cache: &HashMap<DescRef<K, V>, DescValue<T, C, V>>,
    dependants: &mut HashMap<DescRef<K, V>, HashSet<DescRef<K, V>>>,
    desc_queue: &mut Q,
) -> Result<Option<Either<EdgeStatey<T, V>, EdgeStatey<C, V>>>, DBImplErr>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + PartialOrd + Clone,
    C: County<T> + PartialOrd + Clone,
    Q: Queue<Item = DescRef<K, V>>,
{
    if !dependants.contains_key(main_ref) {
        dependants.insert(main_ref.clone(), HashSet::new());
    }
    dependants.get_mut(main_ref).unwrap().insert(r.clone());

    let value = cache.get(r).cloned();
    if value.is_none() {
        desc_queue.enqueue(r.clone());
    }

    value.transpose()
}

fn dependant_access_cache<K, V, T, Q, LN>(
    cache: &HashMap<DescRef<K, V>, NodeValue<T, V>>,
    dependants: &mut HashMap<DescRef<K, V>, HashSet<DescRef<K, V>>>,
    main_ref: &DescRef<K, V>,
    queue: &mut Q,
    r: &DescRef<K, V>,
) -> NodeValue<T, V>
where
    K: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Truthy + PartialOrd + Clone,
    Q: Queue<Item = Query<K, V, LN>>,
{
    if !dependants.contains_key(main_ref) {
        dependants.insert(main_ref.clone(), HashSet::new());
    }
    dependants.get_mut(main_ref).unwrap().insert(r.clone());

    let value = cache.get(r).cloned();
    if value.is_none() {
        queue.enqueue(Query::Access(main_ref.clone()))
    }
    value.unwrap_or(Ok(Statey::bottom()))
}
