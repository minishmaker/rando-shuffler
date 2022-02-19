use std::{collections::HashSet, hash::Hash};

pub trait Shuffle<A: Hash + Eq, B: Hash + Eq> {
    type Delta;

    fn to(&self, a: &A) -> HashSet<B>;
    fn from(&self, b: &B) -> HashSet<A>;
}
