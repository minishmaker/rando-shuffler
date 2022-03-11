use std::ops::{Add, Mul};

/// Truthy forms a lattice
pub trait Truthy {
    fn top() -> Self;
    fn ool() -> Self;
    fn bottom() -> Self;
    fn join(&self, other: &Self) -> Self;
    fn meet(&self, other: &Self) -> Self;
}

pub trait Sphery: Truthy {
    fn increment(&self) -> Self;
}

pub trait County<T: Truthy>: Truthy {
    /// add is an abelian monoid with bottom() as zero
    fn add(&self, other: &Self) -> Self;
    /// scale(add(x,y),n) = add(scale(x,n),scale(y,n))
    /// scale(scale(x,n),m) = scale(x, n*m)
    fn scale(&self, n: Ntgr) -> Self;
    /// lift(bottom()) = bottom()
    /// lift(top()) = top()
    /// lift(join(x,y)) = join(lift(x), lift(y))
    /// lift(meet(x,y)) = meet(lift(x), lift(y))
    fn lift(truthy: &T) -> Self;
    /// ge(x,0) = Top
    /// ge(lift(x), n) = x for n > 0
    /// Scaling should probably make sense
    fn ge(&self, n: Ntgr) -> T;
}

pub trait Statey: Truthy {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Oolean {
    False,
    Ool,
    True,
}

impl Oolean {
    pub fn to_truthy<T: Truthy>(self) -> T {
        match self {
            Oolean::False => T::bottom(),
            Oolean::Ool => T::ool(),
            Oolean::True => T::top(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ntgr {
    Num(u32),
    Infinity,
}

impl Ntgr {
    pub fn to_county<T: Truthy, C: County<T>>(self, t: T) -> C {
        C::lift(&t).scale(self)
    }
}

impl Add for Ntgr {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if let (Ntgr::Num(a), Ntgr::Num(b)) = (self, rhs) {
            Ntgr::Num(a + b)
        } else {
            Ntgr::Infinity
        }
    }
}

impl Mul for Ntgr {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if let (Ntgr::Num(a), Ntgr::Num(b)) = (self, rhs) {
            Ntgr::Num(a * b)
        } else {
            Ntgr::Infinity
        }
    }
}
