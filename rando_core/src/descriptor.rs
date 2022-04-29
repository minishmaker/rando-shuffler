use crate::shuffles::ShufflePattern;

use super::algebra::{Ntgr, Oolean};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DescRef<K, V>(pub K, pub Vec<V>);

pub trait Descriptor<K, V> {
    type Truthy<'a>
    where
        Self: 'a;
    type County<'a>;
    fn eval<D: DescriptorEval<Self, K, V> + ?Sized>(&self, v: &[V], evaluator: &mut D)
        -> D::Output;
    fn eval_truthy<D: DescriptorEval<Self, K, V> + ?Sized>(
        body: &Self::Truthy<'_>,
        evaluator: &mut D,
    ) -> D::Output;

    fn eval_county<D: DescriptorEval<Self, K, V> + ?Sized>(
        body: &Self::County<'_>,
        evaluator: &mut D,
    ) -> D::Output;
}

pub trait DescriptorEval<D: Descriptor<K, V> + ?Sized, K, V> {
    type Output;

    fn from_oolean(&mut self, oolean: Oolean) -> Self::Output;
    fn truthy_ref(&mut self, reference: &DescRef<K, V>) -> Self::Output;
    fn access(&mut self, reference: &DescRef<K, V>) -> Self::Output;
    fn compare(&mut self, county: &D::County<'_>, n: Ntgr) -> Self::Output;
    fn exists<'a, F>(
        &mut self,
        name: &K,
        pattern: &ShufflePattern<V, V>,
        generator: F,
    ) -> Self::Output
    where
        D: 'a,
        F: Fn(V) -> D::Truthy<'a> + 'a;
    fn conjunction<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::Truthy<'a>>,
        D: 'a;
    fn disjunction<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::Truthy<'a>>,
        D: 'a;
    fn prior<I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = (V, bool)>;
    fn posterior<I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = (V, bool)>;

    fn from_ntgr(&mut self, ntgr: Ntgr) -> Self::Output;
    fn county_ref(&mut self, reference: &DescRef<K, V>) -> Self::Output;
    fn count<'a, F>(
        &mut self,
        name: &K,
        pattern: &ShufflePattern<V, V>,
        generator: F,
    ) -> Self::Output
    where
        D: 'a,
        F: Fn(V) -> D::Truthy<'a> + 'a;
    fn min<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::County<'a>>,
        D: 'a;
    fn max<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = D::County<'a>>,
        D: 'a;
    fn linear_combination<'a, I>(&mut self, items: I) -> Self::Output
    where
        I: IntoIterator<Item = (D::County<'a>, Ntgr)>,
        D: 'a;
}

/// Iterator associated types in these funcitons will eventually be replaced with trait method `impl Trait`
pub trait Logic<K, V> {
    type Node: Clone;
    type Descriptor: Descriptor<K, V>;

    type ListNodes<'a>: Iterator<Item = Self::Node>
    where
        Self: 'a;
    fn list_nodes(&self) -> Self::ListNodes<'_>;

    type AdjacentNodes<'a>: Iterator<Item = Option<Self::Node>>
    where
        Self: 'a;
    /// Returns an iterator over all source nodes
    fn adjacent_nodes(&self, dest: Self::Node) -> Self::AdjacentNodes<'_>;

    type AccessNodes<'a>: Iterator<Item = Self::Node>
    where
        Self: 'a;
    fn access_nodes<'a>(&'a self, reference: &DescRef<K, V>) -> Self::AccessNodes<'a>;

    fn eval_edge<R>(
        &self,
        edge: (Option<Self::Node>, Self::Node),
        eval: impl FnMut(&DescRef<K, V>) -> R,
        and: impl FnMut(Vec<R>) -> R,
        or: impl FnMut(Vec<R>) -> R,
    ) -> R;
}
