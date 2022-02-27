use std::{collections::HashSet, hash::Hash};

use either::Either;

pub trait Shuffle<A: Hash + Eq, B: Hash + Eq> {
    type Delta: ShuffleDelta<A, B>;

    fn to(&self, a: &A) -> HashSet<B>;
    fn from(&self, b: &B) -> HashSet<A>;
    fn modify(&mut self, delta: &Self::Delta);
}

pub trait ShuffleDelta<A: Hash + Eq, B: Hash + Eq> {
    fn affects(&self, pattern: &ShufflePattern<A, B>) -> bool;
    fn is_destructive(&self) -> bool;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ShufflePattern<A: Hash + Eq, B: Hash + Eq> {
    A(A),
    B(B),
    Both(A, B),
}

impl<A: Hash + Eq, B: Hash + Eq> ShufflePattern<A, B> {
    pub fn apply<S: Shuffle<A, B>>(&self, r: &S) -> Either<HashSet<A>, HashSet<B>> {
        match self {
            Self::A(a) => Either::Right(r.to(a)),
            Self::B(b) => Either::Left(r.from(b)),
            Self::Both(a, b) => Either::Right(r.to(a).take(b).into_iter().collect()),
        }
    }
}
