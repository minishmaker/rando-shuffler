use std::{cell::RefCell, collections::HashMap, hash::Hash};

use either::Either;

use crate::{
    algebra::{County, Ntgr, Oolean, Sphery, Statey, Truthy},
    descriptor::{Descriptor, EdgeTy, Logic},
    shuffles::{Shuffle, ShufflePattern},
};

use self::cache::{CacheRef, DBCache};
use self::query::{DescriptorType, Query};

mod cache;
mod query;

#[cfg(test)]
mod test;

pub trait Database<'a, D, L, V, T, C, R>
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

    fn mod_shuffle(&mut self, shuffle: &str, delta: &R::Delta) -> Result<(), Self::Err>;
}

#[derive(Clone, Debug)]
pub struct DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Clone + Hash + Eq,
    V: Clone + Hash + Eq,
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    data: DBData<'a, R, L, D>,
    cache: RefCell<DBCache<'a, T, C, V, L::Node, DBImplErr>>,
}

#[derive(Clone, Debug)]
pub struct DBData<'a, R, L, D> {
    shuffles: HashMap<&'a str, R>,
    logic: L,
    _descriptors_keysy: HashMap<&'a str, (&'a str, &'a str)>,
    descriptors_truthy: HashMap<&'a str, Vec<D>>,
    descriptors_county: HashMap<&'a str, Vec<D>>,
}

impl<'a, D, L, V, T, C, R> Database<'a, D, L, V, T, C, R> for DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    type Err = ();

    fn initialize(
        shuffles: HashMap<&'a str, R>,
        logic: L,
        _descriptors_keysy: HashMap<&'a str, (&'a str, &'a str)>,
        descriptors_truthy: HashMap<&'a str, Vec<D>>,
        descriptors_county: HashMap<&'a str, Vec<D>>,
    ) -> Self {
        Self {
            data: DBData {
                shuffles,
                logic,
                _descriptors_keysy,
                descriptors_truthy,
                descriptors_county,
            },

            cache: Default::default(),
        }
    }

    fn query_descriptor_truthy(&self, name: &str, values: &[V]) -> Result<T, Self::Err> {
        self.start_query(&Query::Descriptor(
            name.into(),
            values.into(),
            DescriptorType::Truthy,
        ))
        .map(|v| v.left().unwrap())
    }

    fn query_descriptor_county(&self, name: &str, values: &[V]) -> Result<C, Self::Err> {
        self.start_query(&Query::Descriptor(
            name.into(),
            values.into(),
            DescriptorType::County,
        ))
        .map(|v| v.right().unwrap())
    }

    fn query_logic_node(&self, ln: &L::Node) -> Result<T, Self::Err> {
        self.start_query(&Query::Node(ln.clone()))
            .map(|v| v.left().unwrap())
    }

    fn query_access(&self, name: &str, values: &[V]) -> Result<T, Self::Err> {
        self.start_query(&Query::Access(name.into(), values.into()))
            .map(|v| v.left().unwrap())
    }

    fn mod_shuffle(&mut self, shuffle: &str, delta: &R::Delta) -> Result<(), Self::Err> {
        self.data.shuffles.get_mut(shuffle).ok_or(())?.modify(delta);

        let data = &self.data;
        self.cache
            .get_mut()
            .mod_shuffle(shuffle, delta, |n, c| Self::eval_query(data, c, n));

        Ok(())
    }
}

type DBImplErr = ();

impl<'a, D, L, V, T, C, R> DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Clone + Hash + Eq,
    V: 'a + Hash + Eq + Clone,
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    fn start_query(&self, query: &Query<V, L::Node>) -> Result<Either<T, C>, DBImplErr> {
        Self::query_inner(&self.data, &mut self.cache.borrow_mut(), query, None)
    }

    fn query_inner(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        query: &Query<V, L::Node>,
        parent: Option<CacheRef>,
    ) -> Result<Either<T, C>, DBImplErr> {
        let result = cache.register(query, parent);

        match result {
            Ok(v) => v,
            Err(cache_ref) => {
                let value = Self::eval_query(data, cache, cache_ref);
                cache.set_cache(cache_ref, value.clone(), |n, c| {
                    Self::eval_query(data, c, n)
                });
                value
            }
        }
    }

    fn eval_query(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
    ) -> Result<Either<T, C>, DBImplErr> {
        let query = cache.get_query(cache_ref);
        match &*query {
            Query::Descriptor(name, values, ty) => {
                Self::eval_descriptor_ref(data, cache, cache_ref, name, values, *ty)
            }
            Query::Access(name, values) => {
                Self::eval_access(data, cache, cache_ref, name, values).map(Either::Left)
            }
            Query::Node(node) => Self::eval_node(data, cache, cache_ref, node).map(Either::Left),
        }
    }

    fn eval_node(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
        node: &L::Node,
    ) -> Result<T, DBImplErr> {
        data.logic
            .edges(node)
            .map(|edge| {
                (
                    Self::eval_descriptor(data, cache, cache_ref, edge.descriptor, &[])
                        .and_then(|v| v.left().ok_or(())),
                    match &edge.ty {
                        EdgeTy::FromTrue => Ok(T::top()),
                        EdgeTy::FromNode(source) => Self::query_inner(
                            data,
                            cache,
                            &Query::Node(source.clone()),
                            Some(cache_ref),
                        )
                        .and_then(|v| v.left().ok_or(())),
                    },
                )
            })
            .try_fold(T::bottom(), |a, (b, c)| {
                b.and_then(|b| c.map(|c| a.join(&b.meet(&c))))
            })
            .map(|t| t.increment())
    }

    fn eval_access(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
        name: &str,
        values: &[V],
    ) -> Result<T, DBImplErr> {
        data.logic
            .access_nodes(name, values)
            .map(|n| {
                let query = Query::Node(n);
                Self::query_inner(data, cache, &query, Some(cache_ref)).map(|v| v.left().unwrap())
            })
            .try_fold(T::bottom(), |a, b| b.map(|b| a.join(&b)))
    }

    fn eval_descriptor_ref(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
        descriptor: &str,
        values: &[V],
        ty: DescriptorType,
    ) -> Result<Either<T, C>, DBImplErr> {
        let descriptors = match ty {
            DescriptorType::Truthy => data.descriptors_truthy.get(descriptor).ok_or(()),
            DescriptorType::County => data.descriptors_county.get(descriptor).ok_or(()),
        }?;

        let values = descriptors
            .iter()
            .map(|d| Self::eval_descriptor(data, cache, cache_ref, d, values));

        match ty {
            DescriptorType::Truthy => values
                .map(|v| v.and_then(|v| v.left().ok_or(())))
                .try_fold(T::bottom(), |a, b| b.map(|b| a.join(&b)))
                .map(Either::Left),
            DescriptorType::County => values
                .map(|v| v.and_then(|v| v.right().ok_or(())))
                .try_fold(C::bottom(), |a, b| b.map(|b| a.join(&b)))
                .map(Either::Right),
        }
    }

    fn eval_descriptor<D1: Descriptor<V>>(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
        descriptor: &D1,
        values: &[V],
    ) -> Result<Either<T, C>, DBImplErr> {
        let cache = RefCell::new(cache);
        descriptor.eval(
            values,
            |t, v| {
                let cache = &mut *cache.borrow_mut();
                Self::eval_descriptor_truthy::<D1>(data, cache, cache_ref, t, v).map(Either::Left)
            },
            |c, v| {
                let cache = &mut *cache.borrow_mut();
                Self::eval_descriptor_county::<D1>(data, cache, cache_ref, c, v).map(Either::Right)
            },
        )
    }

    fn eval_descriptor_truthy<D1: Descriptor<V>>(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
        t: &D1::Truthy,
        v: &[V],
    ) -> Result<T, DBImplErr> {
        let cache = RefCell::new(cache);
        let oolean = |ool: Oolean| Ok(ool.to_truthy());
        let reference = |name: &str, values: &[_]| {
            let query = Query::Descriptor(name.into(), values.into(), DescriptorType::Truthy);
            let cache = &mut *cache.borrow_mut();
            Self::query_inner(data, cache, &query, Some(cache_ref)).and_then(|v| v.left().ok_or(()))
        };
        let access = |name: &str, values: &[_]| {
            let query = Query::Access(name.into(), values.into());
            let cache = &mut *cache.borrow_mut();
            Self::query_inner(data, cache, &query, Some(cache_ref)).and_then(|v| v.left().ok_or(()))
        };
        let compare = |c: &D1::County, n: Ntgr| {
            let cache = &mut *cache.borrow_mut();
            Self::eval_descriptor_county::<D1>(data, cache, cache_ref, c, v)
                .map(|c| County::ge(&c, n))
        };
        let exists = |n: &str, p: &ShufflePattern<V, V>, f: &dyn Fn(_) -> _| {
            let shuffle = data.shuffles.get(n).ok_or(())?;
            let cache = &mut *cache.borrow_mut();
            cache.add_shuffle_dependency(n.to_string(), p.clone(), cache_ref);
            p.apply(shuffle)
                .into_inner()
                .into_iter()
                .map(f)
                .map(|t| Self::eval_descriptor_truthy::<D1>(data, cache, cache_ref, &t, v))
                .try_fold(T::bottom(), |a, b| b.map(|b| a.join(&b)))
        };
        let conj = |r, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a: T = r?;
            let b = Self::eval_descriptor_truthy::<D1>(data, cache, cache_ref, b, v)?;
            Ok(a.meet(&b))
        };
        let disj = |r, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a: T = r?;
            let b = Self::eval_descriptor_truthy::<D1>(data, cache, cache_ref, b, v)?;
            Ok(a.join(&b))
        };
        let prior = |_: &_| todo!();
        let posterior = |_: &_| todo!();

        D1::eval_truthy(
            t, v, oolean, reference, access, compare, exists, conj, disj, prior, posterior,
        )
    }

    fn eval_descriptor_county<D1: Descriptor<V>>(
        data: &DBData<R, L, D>,
        cache: &mut DBCache<T, C, V, L::Node, DBImplErr>,
        cache_ref: CacheRef,
        c: &D1::County,
        v: &[V],
    ) -> Result<C, DBImplErr> {
        let cache = RefCell::new(cache);
        let ntgr = |n: Ntgr| Ok(n.to_county(T::top()));
        let reference = |name: &str, values: &[_]| {
            let query = Query::Descriptor(name.into(), values.into(), DescriptorType::Truthy);
            let cache = &mut *cache.borrow_mut();
            Self::query_inner(data, cache, &query, Some(cache_ref))
                .and_then(|v| v.right().ok_or(()))
        };
        let comb = |r, b: &_, n| {
            let cache = &mut *cache.borrow_mut();
            let a: C = r?;
            let b = Self::eval_descriptor_county::<D1>(data, cache, cache_ref, b, v)?;
            Ok(a.add(&b.scale(n)))
        };
        let min = |r, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a: C = r?;
            let b = Self::eval_descriptor_county::<D1>(data, cache, cache_ref, b, v)?;
            Ok(a.meet(&b))
        };
        let max = |r, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a: C = r?;
            let b = Self::eval_descriptor_county::<D1>(data, cache, cache_ref, b, v)?;
            Ok(a.join(&b))
        };
        let count = |n: &str, p: &ShufflePattern<V, V>, f: &dyn Fn(_) -> _| {
            let shuffle = data.shuffles.get(n).ok_or(())?;
            let cache = &mut *cache.borrow_mut();
            cache.add_shuffle_dependency(n.to_string(), p.clone(), cache_ref);
            p.apply(shuffle)
                .into_inner()
                .into_iter()
                .map(f)
                .map(|t| Self::eval_descriptor_truthy::<D1>(data, cache, cache_ref, &t, v))
                .try_fold(C::top(), |a, b| b.map(|b| a.add(&C::lift(&b))))
        };

        D1::eval_county(c, v, ntgr, reference, comb, min, max, count)
    }
}
