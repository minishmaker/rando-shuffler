use crate::shuffles::ShufflePattern;

use super::algebra::{Ntgr, Oolean};

#[allow(clippy::too_many_arguments)]
pub trait Descriptor<V> {
    type Truthy;
    type County;
    fn eval<R, T, C>(&self, v: &[V], t: T, c: C) -> R
    where
        T: Fn(&Self::Truthy, &[V]) -> R,
        C: Fn(&Self::County, &[V]) -> R;
    fn eval_truthy<R>(
        body: &Self::Truthy,
        values: &[V],
        c: impl Fn(Oolean) -> R,
        r: impl Fn(&str, &[V]) -> R,
        a: impl Fn(&str, &[V]) -> R,
        comp: impl Fn(&Self::County, Ntgr) -> R,
        ex: impl Fn(&str, &ShufflePattern<V, V>, &dyn Fn(V) -> Self::Truthy) -> R,
        conj: impl Fn(R, &Self::Truthy) -> R,
        disj: impl Fn(R, &Self::Truthy) -> R,
        prio: impl Fn(&[(bool, V)]) -> R,
        post: impl Fn(&[(bool, V)]) -> R,
    ) -> R;

    fn eval_county<R>(
        body: &Self::County,
        values: &[V],
        c: impl Fn(Ntgr) -> R,
        r: impl Fn(&str, &[V]) -> R,
        comb: impl Fn(R, &Self::County, Ntgr) -> R,
        min: impl Fn(R, &Self::County) -> R,
        max: impl Fn(R, &Self::County) -> R,
        ct: impl Fn(&str, &ShufflePattern<V, V>, &dyn Fn(V) -> Self::Truthy) -> R,
    ) -> R;
}

/// Note: The concrete return types for these methods are unstable, pending GAT and existential types.
/// It's only stable to use these as `impl Iterator + 'a`.
pub trait Logic<V> {
    type Node: Clone;
    type Descriptor: Descriptor<V>;

    fn list_nodes<'a>(&'a self) -> Box<dyn Iterator<Item = Self::Node> + 'a>;
    fn edges<'a>(
        &'a self,
        source: &Self::Node,
    ) -> Box<dyn Iterator<Item = Edge<'a, Self::Descriptor, Self::Node>> + 'a>;
    fn access_nodes<'a>(
        &'a self,
        name: &str,
        values: &[V],
    ) -> Box<dyn Iterator<Item = Self::Node> + 'a>;
}

pub struct Edge<'a, D, N> {
    pub descriptor: &'a D,
    pub ty: EdgeTy<N>,
}

pub enum EdgeTy<N> {
    FromNode(N),
    FromTrue,
}
