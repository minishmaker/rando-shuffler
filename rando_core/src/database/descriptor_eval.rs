use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use either::Either;

use crate::{
    algebra::{County, Ntgr, Oolean, Truthy},
    descriptor::{DescRef, Descriptor, DescriptorEval},
    shuffles::{Shuffle, ShufflePattern},
    statey::{EdgeStatey, Statey},
};

use super::{DBImplErr, TryIterator};

pub struct DatabaseDescriptorEval<F, G, H> {
    pub access_cache: F,
    pub descriptor_cache: G,
    pub shuffles: H,
}

impl<'shuffles, D, K, V, T, C, R, F, G, L> DescriptorEval<D, K, V>
    for DatabaseDescriptorEval<F, G, L>
where
    K: Hash + Eq,
    V: Hash + Eq + Clone,
    R: Shuffle<V, V> + 'shuffles,
    T: Truthy + PartialOrd + Clone,
    C: County<T> + PartialOrd + Clone,
    D: Descriptor<K, V>,
    F: FnMut(&DescRef<K, V>) -> Result<Statey<T, V>, DBImplErr>,
    G: FnMut(
        &DescRef<K, V>,
    ) -> Result<Option<Either<EdgeStatey<T, V>, EdgeStatey<C, V>>>, DBImplErr>,
    L: FnMut(&K, &ShufflePattern<V, V>) -> Option<&'shuffles R> + 'shuffles,
{
    type Output = Result<Either<EdgeStatey<T, V>, EdgeStatey<C, V>>, DBImplErr>;

    fn from_oolean(&mut self, oolean: Oolean) -> Self::Output {
        Ok(Either::Left(EdgeStatey::lift(oolean.to_truthy())))
    }

    fn truthy_ref(&mut self, reference: &DescRef<K, V>) -> Self::Output {
        (self.descriptor_cache)(reference).and_then(|v| match v {
            Some(v @ Either::Left(_)) => Ok(v),
            None => Ok(Either::Left(EdgeStatey::bottom())),
            _ => Err(DBImplErr::Type),
        })
    }

    fn access(&mut self, reference: &DescRef<K, V>) -> Self::Output {
        (self.access_cache)(reference)
            .map(|c| Statey::to_edge(c, &HashSet::new()))
            .map(Either::Left)
    }

    fn compare(&mut self, county: &D::County<'_>, n: Ntgr) -> Self::Output {
        D::eval_county(county, self)
            .and_then(|c| c.right().ok_or(DBImplErr::Type))
            .map(|c| c.map(|c| County::ge(&c, n)))
            .map(Either::Left)
    }

    fn exists<'a, J>(
        &mut self,
        name: &K,
        pattern: &ShufflePattern<V, V>,
        generator: J,
    ) -> Self::Output
    where
        D: 'a,
        J: Fn(V) -> D::Truthy<'a> + 'a,
    {
        let shuffle = (self.shuffles)(name, pattern).ok_or(DBImplErr::UnknownShuffle)?;

        TryIterator::new(
            pattern
                .apply(shuffle)
                .into_inner()
                .into_iter()
                .map(generator)
                .map(|t| D::eval_truthy(&t, self).and_then(|e| e.left().ok_or(DBImplErr::Type))),
        )
        .try_pass(|i| EdgeStatey::or_all(i))
        .map(Either::Left)
    }

    fn conjunction<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::Truthy<'a>>,
        D: 'a,
    {
        items
            .into_iter()
            .map(|t| D::eval_truthy(&t, self).and_then(|e| e.left().ok_or(DBImplErr::Type)))
            .try_fold(EdgeStatey::lift(T::top()), |a, b| Ok(a.and(&b?)))
            .map(Either::Left)
    }

    fn disjunction<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::Truthy<'a>>,
        D: 'a,
    {
        TryIterator::new(
            items
                .into_iter()
                .map(|t| D::eval_truthy(&t, self).and_then(|e| e.left().ok_or(DBImplErr::Type))),
        )
        .try_pass(|i| EdgeStatey::or_all(i))
        .map(Either::Left)
    }

    fn prior<I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = (V, bool)>,
    {
        Ok(Either::Left(EdgeStatey::from_states(
            items.into_iter().collect(),
            HashMap::new(),
        )))
    }

    fn posterior<I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = (V, bool)>,
    {
        Ok(Either::Left(EdgeStatey::from_states(
            HashMap::new(),
            items.into_iter().collect(),
        )))
    }

    fn from_ntgr(&mut self, ntgr: Ntgr) -> Self::Output {
        Ok(Either::Right(EdgeStatey::lift(ntgr.to_county(T::top()))))
    }

    fn county_ref(&mut self, reference: &DescRef<K, V>) -> Self::Output {
        (self.descriptor_cache)(reference).and_then(|v| match v {
            Some(v @ Either::Right(_)) => Ok(v),
            None => Ok(Either::Right(EdgeStatey::bottom())),
            _ => Err(DBImplErr::Type),
        })
    }

    fn count<'a, J>(
        &mut self,
        name: &K,
        pattern: &ShufflePattern<V, V>,
        generator: J,
    ) -> Self::Output
    where
        D: 'a,
        J: Fn(V) -> D::Truthy<'a> + 'a,
    {
        let shuffle = (self.shuffles)(name, pattern).ok_or(DBImplErr::UnknownShuffle)?;

        let mut truthies = pattern
            .apply(shuffle)
            .into_inner()
            .into_iter()
            .map(generator)
            .map(|t| D::eval_truthy(&t, self).and_then(|e| e.left().ok_or(DBImplErr::Type)));

        truthies
            .try_fold(EdgeStatey::bottom(), |a, b| {
                let b = b?.map(|t| C::lift(&t));
                Ok(EdgeStatey::or_all([a.and(&b), a, b]))
            })
            .map(Either::Right)
    }

    fn min<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::County<'a>>,
        D: 'a,
    {
        items
            .into_iter()
            .map(|t| D::eval_county(&t, self).and_then(|e| e.right().ok_or(DBImplErr::Type)))
            .try_fold(EdgeStatey::lift(C::top()), |a, b| Ok(a.and(&b?)))
            .map(Either::Right)
    }

    fn max<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::County<'a>>,
        D: 'a,
    {
        TryIterator::new(
            items
                .into_iter()
                .map(|t| D::eval_county(&t, self).and_then(|e| e.right().ok_or(DBImplErr::Type))),
        )
        .try_pass(|i| EdgeStatey::or_all(i))
        .map(Either::Right)
    }

    fn linear_combination<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = (D::County<'a>, Ntgr)>,
        D: 'a,
    {
        items
            .into_iter()
            .map(|(c, n)| {
                D::eval_county(&c, self)
                    .and_then(|e| e.right().ok_or(DBImplErr::Type))
                    .map(|c| c.map(|c| c.scale(n)))
            })
            .try_fold(EdgeStatey::bottom(), |a, b| {
                let b = b?;
                Ok(EdgeStatey::or_all([a.and(&b), a, b]))
            })
            .map(Either::Right)
    }
}
