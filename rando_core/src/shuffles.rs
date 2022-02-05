use std::{collections::HashSet, hash::Hash};


pub trait Shuffle<A: Hash,B: Hash> {
    type Delta;

    fn to(&self, a: &A) -> HashSet<B>;
    fn from(&self, b: &B) -> HashSet<A>;
}