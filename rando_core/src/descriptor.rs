use std::{hash::Hash, collections::{HashMap, HashSet}};

use crate::{algebra::{Sphery, Truthy, County}, shuffles::Shuffle};

pub trait Descriptor<V: Hash> {
    type Truthy;
    type County;
    fn eval<R, T, C>(v: &[V], t : T, c : C) -> R
    where
        T : Fn(Self::Truthy) -> R,
        C : Fn(Self::County) -> R;
    fn eval_truthy<R, C, Ref, A, Comp, Ex, Conj, Disj, Prio, Post>(body : Self::Truthy, c : C, r : Ref, a : A, comp : Comp, ex : Ex, prio : Prio, post : Post) -> R
    where
        C : Fn(Oolean) -> R,
        Ref : Fn(&str, &[V]) -> R,
        A : Fn(&str, &[V]) -> R,
        Comp : Fn(&Self::County, Ntgr) -> R,
        Ex : Fn(Relation<'_>, &V, &dyn Fn(&V) -> Self::Truthy) -> R,
        Conj : Fn(&Self::Truthy, &Self::Truthy) -> R,
        Disj : Fn(&Self::Truthy, &Self::Truthy) -> R,
        Prio : Fn(&[(Bool, V)]) -> R,
        Post : Fn(&[(Bool, V)]) -> R;
    fn eval_county<R, C, Ref, Comb, Min, Max, Ct>(body: Self::County, c: C, r: Ref, comb: Comb, min: Min, max: Max, ct: Ct) -> R
    where
        C: Fn(Oolean) -> R,
        Ref: Fn(&str, &[V]) -> R,
        A: Fn(&str, &[V]) -> R,
        Comb: Fn(&Self::County, u32, &Self::County, u32) -> R,
        Min: Fn(&Self::County, &Self::County) -> R,
        Max: Fn(&Self::County, &Self::County) -> R,
        Ct: Fn(Relation<'_>, &V, &dyn Fn(&V) -> Self::County) -> R;
}

trait Logic<N, V> {}

// TODO: move this to an appropriate place
trait Database<'a, D, L, LN, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<LN, V> + Hash,
    LN: Hash,
    V: Hash,
    T: Truthy + Sphery + Statey,
    C: County<T> + Sphery + Statey,
    R: Shuffle<V, V>
{
    type Err;

    fn initialize(
        shuffles: HashMap<&'a str,R>,
        logic : HashSet<L>,
        descriptors_keysy : HashMap<&'a str, (&'a str, &'a str)>,
        descriptors_truthy : HashMap<&'a str, Vec<D>>,
        descriptors_county : HashMap<&'a str, Vec<D>>
    ) -> Self;
    
    fn query_descriptor_truthy(&self, name: &str, values : &[V]) -> Result<T, Self::Err>;
    fn query_descriptor_county(&self, name: &str, values : &[V]) -> Result<C, Self::Err>;
    fn query_logic_node(&self, ln : &LN) -> Result<T, Self::Err>;
    fn query_access(&self, name: &str, values: &[V]) -> Result<T, Self::Err>;

    fn mod_shuffle(&mut self, delta : &R::Delta);
}
