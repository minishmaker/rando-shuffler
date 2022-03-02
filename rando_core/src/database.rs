use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

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
    shuffles: HashMap<&'a str, R>,
    logic: L,
    _descriptors_keysy: HashMap<&'a str, (&'a str, &'a str)>,
    descriptors_truthy: HashMap<&'a str, Vec<D>>,
    descriptors_county: HashMap<&'a str, Vec<D>>,
    cache: RefCell<DBCache<'a, T, C, V, L::Node>>,
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
            shuffles,
            logic,
            _descriptors_keysy,
            descriptors_truthy,
            descriptors_county,
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
        self.shuffles.get_mut(shuffle).ok_or(())?.modify(delta);

        self.cache
            .borrow_mut()
            .mod_shuffle(shuffle, delta, |n, c| self.eval_query(n, c))
    }
}

type DBImplErr = ();

impl<'a, D, L, V, T, C, R> DBImpl<'a, D, L, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<V>,
    L::Node: Clone + Hash + Eq,
    V: Hash + Eq + Clone,
    T: Clone + Eq + PartialOrd + Truthy + Sphery + Statey,
    C: Clone + Eq + PartialOrd + County<T> + Sphery + Statey,
    R: Shuffle<V, V>,
{
    fn start_query(&self, query: &Query<V, L::Node>) -> Result<Either<T, C>, DBImplErr> {
        self.query_inner(query, None, &mut self.cache.borrow_mut())
    }

    fn query_inner(
        &self,
        query: &Query<V, L::Node>,
        parent: Option<CacheRef>,
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<Either<T, C>, DBImplErr> {
        let result = cache.register(query, parent);

        match result {
            Ok(v) => Ok(v),
            Err(cache_ref) => {
                let value = self.eval_query(cache_ref, cache)?;
                cache.set_cache(cache_ref, value.clone(), |n, c| self.eval_query(n, c));
                Ok(value)
            }
        }
    }

    fn eval_query(
        &self,
        cache_ref: CacheRef,
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<Either<T, C>, DBImplErr> {
        let query = cache.get_query(cache_ref);
        let result = match &*query {
            Query::Descriptor(name, values, ty) => {
                self.eval_descriptor_ref(cache_ref, &name, values, *ty, cache)
            }
            Query::Access(name, values) => self
                .eval_access(cache_ref, name, values, cache)
                .map(Either::Left),
            Query::Node(node) => self.eval_node(cache_ref, node, cache).map(Either::Left),
        };
        result
    }

    fn eval_node(
        &self,
        cache_ref: CacheRef,
        node: &L::Node,
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<T, DBImplErr> {
        self.logic
            .edges(node)
            .map(|edge| {
                (
                    self.eval_descriptor(cache_ref, edge.descriptor, &[], cache)
                        .and_then(|v| v.left().ok_or(())),
                    match &edge.ty {
                        EdgeTy::FromTrue => Ok(T::top()),
                        EdgeTy::FromNode(source) => self
                            .query_inner(&Query::Node(source.clone()), Some(cache_ref), cache)
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
        &self,
        cache_ref: CacheRef,
        name: &str,
        values: &[V],
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<T, DBImplErr> {
        self.logic
            .access_nodes(name, values)
            .map(|n| {
                let query = Query::Node(n);
                self.query_inner(&query, Some(cache_ref), cache)
                    .map(|v| v.left().unwrap())
            })
            .try_fold(T::bottom(), |a, b| b.map(|b| a.join(&b)))
    }

    fn eval_descriptor_ref(
        &self,
        cache_ref: CacheRef,
        descriptor: &str,
        values: &[V],
        ty: DescriptorType,
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<Either<T, C>, DBImplErr> {
        let descriptors = match ty {
            DescriptorType::Truthy => self.descriptors_truthy.get(descriptor).ok_or(()),
            DescriptorType::County => self.descriptors_county.get(descriptor).ok_or(()),
        }?;

        let values = descriptors
            .into_iter()
            .map(|d| self.eval_descriptor(cache_ref, d, values, cache));

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
        &self,
        cache_ref: CacheRef,
        descriptor: &D1,
        values: &[V],
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<Either<T, C>, DBImplErr> {
        let cache = RefCell::new(cache);
        descriptor.eval(
            values,
            |t, v| {
                let cache = &mut *cache.borrow_mut();
                self.eval_descriptor_truthy::<D1>(cache_ref, t, v, cache)
                    .map(Either::Left)
            },
            |c, v| {
                let cache = &mut *cache.borrow_mut();
                self.eval_descriptor_county::<D1>(cache_ref, c, v, cache)
                    .map(Either::Right)
            },
        )
    }

    fn eval_descriptor_truthy<D1: Descriptor<V>>(
        &self,
        cache_ref: CacheRef,
        t: &D1::Truthy,
        v: &[V],
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<T, DBImplErr> {
        let cache = RefCell::new(cache);
        let oolean = |ool: Oolean| Ok(ool.to_truthy());
        let reference = |name: &str, values: &[_]| {
            let query = Query::Descriptor(name.into(), values.into(), DescriptorType::Truthy);
            let cache = &mut *cache.borrow_mut();
            self.query_inner(&query, Some(cache_ref), cache)
                .and_then(|v| v.left().ok_or(()))
        };
        let access = |name: &str, values: &[_]| {
            let query = Query::Access(name.into(), values.into());
            let cache = &mut *cache.borrow_mut();
            self.query_inner(&query, Some(cache_ref), cache)
                .and_then(|v| v.left().ok_or(()))
        };
        let compare = |c: &D1::County, n: Ntgr| {
            let cache = &mut *cache.borrow_mut();
            self.eval_descriptor_county::<D1>(cache_ref, c, v, cache)
                .map(|c| County::ge(&c, n))
        };
        let exists = |n: &str, p: &ShufflePattern<V, V>, f: &dyn Fn(&_) -> _| {
            let shuffle = self.shuffles.get(n).ok_or(())?;
            let cache = &mut *cache.borrow_mut();
            cache.add_shuffle_dependency(n.to_string(), p.clone(), cache_ref);
            p.apply(shuffle)
                .into_inner()
                .into_iter()
                .map(|v| f(&v))
                .map(|t| self.eval_descriptor_truthy::<D1>(cache_ref, &t, v, cache))
                .try_fold(T::bottom(), |a, b| b.map(|b| a.join(&b)))
        };
        let conj = |a: &_, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a = self.eval_descriptor_truthy::<D1>(cache_ref, a, v, cache)?;
            let b = self.eval_descriptor_truthy::<D1>(cache_ref, b, v, cache)?;
            Ok(a.meet(&b))
        };
        let disj = |a: &_, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a = self.eval_descriptor_truthy::<D1>(cache_ref, a, v, cache)?;
            let b = self.eval_descriptor_truthy::<D1>(cache_ref, b, v, cache)?;
            Ok(a.join(&b))
        };
        let prior = |_: &_| todo!();
        let posterior = |_: &_| todo!();

        D1::eval_truthy(
            t, v, oolean, reference, access, compare, exists, conj, disj, prior, posterior,
        )
    }

    fn eval_descriptor_county<D1: Descriptor<V>>(
        &self,
        cache_ref: CacheRef,
        c: &D1::County,
        v: &[V],
        cache: &mut DBCache<T, C, V, L::Node>,
    ) -> Result<C, DBImplErr> {
        let cache = RefCell::new(cache);
        let ntgr = |n: Ntgr| Ok(n.to_county(T::top()));
        let reference = |name: &str, values: &[_]| {
            let query = Query::Descriptor(name.into(), values.into(), DescriptorType::Truthy);
            let cache = &mut *cache.borrow_mut();
            self.query_inner(&query, Some(cache_ref), cache)
                .and_then(|v| v.right().ok_or(()))
        };
        let comb = |a: &_, n, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a = self.eval_descriptor_county::<D1>(cache_ref, a, v, cache)?;
            let b = self.eval_descriptor_county::<D1>(cache_ref, b, v, cache)?;
            Ok(a.scale(n).add(&b))
        };
        let min = |a: &_, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a = self.eval_descriptor_county::<D1>(cache_ref, a, v, cache)?;
            let b = self.eval_descriptor_county::<D1>(cache_ref, b, v, cache)?;
            Ok(a.meet(&b))
        };
        let max = |a: &_, b: &_| {
            let cache = &mut *cache.borrow_mut();
            let a = self.eval_descriptor_county::<D1>(cache_ref, a, v, cache)?;
            let b = self.eval_descriptor_county::<D1>(cache_ref, b, v, cache)?;
            Ok(a.join(&b))
        };
        let count = |n: &str, p: ShufflePattern<V, V>, f: &dyn Fn(&_) -> _| {
            let shuffle = self.shuffles.get(n).ok_or(())?;
            let cache = &mut *cache.borrow_mut();
            cache.add_shuffle_dependency(n.to_string(), p.clone(), cache_ref);
            p.apply(shuffle)
                .into_inner()
                .into_iter()
                .map(|v| f(&v))
                .map(|t| self.eval_descriptor_truthy::<D1>(cache_ref, &t, v, cache))
                .try_fold(C::top(), |a, b| b.map(|b| a.add(&C::lift(&b))))
        };

        D1::eval_county(c, v, ntgr, reference, comb, min, max, count)
    }
}
