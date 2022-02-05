use std::{hash::Hash, collections::{HashMap, HashSet}};

use crate::{algebra::{Sphery, Truthy, County}, shuffles::Shuffle};


pub trait Descriptor<V: Hash> {
    
}

trait Logic<N, V> {}

// TODO: move this to an appropriate place
trait Database<'a, D, L, LN, V, T, C, R>
where
    D: Descriptor<V>,
    L: Logic<LN, V> + Hash,
    LN: Hash,
    V: Hash,
    T: Truthy + Sphery,
    C: County<T> + Sphery,
    R: Shuffle<V, V>
{
    type Err;

    fn initialize(
        shuffles: HashMap<&'a str,R>,
        logic : HashSet<L>,
        descriptors_truthy : HashMap<&'a str, Vec<D>>,
        descriptors_county : HashMap<&'a str, Vec<D>>
    ) -> Self;
    
    fn query_descriptor_truthy(&self, name: &str, values : &[V]) -> Result<T, Self::Err>;
    fn query_descriptor_county(&self, name: &str, values : &[V]) -> Result<C, Self::Err>;
    fn query_logic_node(&self, ln : &LN) -> Result<T, Self::Err>;

    fn mod_shuffle(&mut self, delta : &R::Delta);
}
