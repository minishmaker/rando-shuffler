use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    hash::Hash,
    iter,
    ops::ControlFlow,
};

use crate::algebra::Truthy;

#[derive(Clone, Debug)]
pub struct Statey<T, V>(StateyInner<NodePossibility<T, V>>);

impl<T: Truthy + PartialOrd + Clone, V: Hash + Eq + Clone> Statey<T, V> {
    /// Upgrade to an edge truthy.
    /// If any state in `self` is also in `constraints`, that state will be set in the resulting posterior set.
    pub fn to_edge(self, constraints: &HashSet<V>) -> EdgeStatey<T, V> {
        let possibilities = self.0 .0.into_iter().map(|n| EdgePossibility {
            t: n.t,
            prior: HashMap::new(),
            posterior: n
                .states
                .intersection(constraints)
                .cloned()
                .map(|v| (v, true))
                .collect(),
        });

        EdgeStatey(StateyInner(mapping_maximal(
            possibilities,
            StateyItem::map_partial_cmp,
        )))
    }

    pub fn bottom() -> Self {
        Self(StateyInner(vec![]))
    }

    pub fn lift(t: T) -> Self {
        if t != T::bottom() {
            Self(StateyInner(vec![NodePossibility::lift(t)]))
        } else {
            Self::bottom()
        }
    }

    pub fn map<U: Truthy + PartialOrd, F: FnMut(T) -> U>(self, mut map: F) -> Statey<U, V> {
        let possibilities = self.0 .0.into_iter().map(|p| p.map(&mut map));

        Statey(StateyInner(mapping_maximal(
            possibilities,
            StateyItem::map_partial_cmp,
        )))
    }

    /// Unlike [`and`][Self::and], this takes elements by value rather than by reference.
    pub fn or(self, other: Self) -> Self {
        Self(StateyInner::or_all([self.0, other.0]))
    }

    /// More efficient version of [`or`][Self::or] for iterators
    pub fn or_all<I: IntoIterator<Item = Self>>(items: I) -> Self {
        Self(StateyInner::or_all(items.into_iter().map(|i| i.0)))
    }

    pub fn combine<F: Fn(&T, &T) -> T>(&self, other: &Self, f: F) -> Self {
        Self(self.0.combine(&other.0, &mut |a, b| a.combine(b, &f)))
    }

    pub fn apply_edge<F: Fn(&T, &T) -> T>(&self, other: &EdgeStatey<T, V>, f: F) -> Self {
        Self(self.0.combine(&other.0, &mut |a, b| a.apply_edge(b, &f)))
    }

    pub fn and(&self, other: &Self) -> Self {
        self.combine(other, T::meet)
    }

    pub fn and_edge(&self, other: &EdgeStatey<T, V>) -> Self {
        self.apply_edge(other, T::meet)
    }
}

impl<T: Truthy + PartialOrd + Clone, V: Hash + Eq + Clone> FromIterator<Self> for Statey<T, V> {
    fn from_iter<I: IntoIterator<Item = Self>>(iter: I) -> Self {
        Self::or_all(iter)
    }
}

#[derive(Clone, Debug)]
pub struct EdgeStatey<T, V>(StateyInner<EdgePossibility<T, V>>);

impl<T: Truthy + PartialOrd + Clone, V: Hash + Eq + Clone> EdgeStatey<T, V> {
    pub fn from_states(prior: HashMap<V, bool>, posterior: HashMap<V, bool>) -> Self {
        Self(StateyInner(vec![EdgePossibility::from_states(
            prior, posterior,
        )]))
    }

    pub fn bottom() -> Self {
        Self(StateyInner(vec![]))
    }

    pub fn lift(t: T) -> Self {
        Self(StateyInner(vec![EdgePossibility::lift(t)]))
    }

    pub fn map<U: Truthy + PartialOrd, F: FnMut(T) -> U>(self, mut map: F) -> EdgeStatey<U, V> {
        let possibilities = self.0 .0.into_iter().map(|p| p.map(&mut map));

        EdgeStatey(StateyInner(mapping_maximal(
            possibilities,
            StateyItem::map_partial_cmp,
        )))
    }

    /// Unlike [`and`][Self::and], this takes elements by value rather than by reference.
    pub fn or(self, other: Self) -> Self {
        Self(StateyInner::or_all(
            iter::once(self.0).chain(iter::once(other.0)),
        ))
    }

    /// More efficient version of [`or`][Self::or] for iterators
    pub fn or_all<I: IntoIterator<Item = Self>>(items: I) -> Self {
        Self(StateyInner::or_all(items.into_iter().map(|i| i.0)))
    }

    pub fn combine<F: Fn(&T, &T) -> T>(&self, other: &Self, f: F) -> Self {
        Self(self.0.combine(&other.0, &mut |a, b| a.combine(b, &f)))
    }

    pub fn and(&self, other: &Self) -> Self {
        self.combine(other, T::meet)
    }
}

impl<T: Truthy + PartialOrd + Clone, V: Hash + Eq + Clone> FromIterator<Self> for EdgeStatey<T, V> {
    fn from_iter<I: IntoIterator<Item = Self>>(iter: I) -> Self {
        Self::or_all(iter)
    }
}

#[derive(Clone, Debug)]
struct StateyInner<T>(Vec<T>);

trait StateyItem {
    fn map_partial_cmp(self, other: &Self) -> (Self, Option<Ordering>)
    where
        Self: Sized;
    fn is_bottom(&self) -> bool;
}

impl<T: StateyItem + PartialOrd> StateyInner<T> {
    fn or_all<I: IntoIterator<Item = Self>>(items: I) -> Self {
        StateyInner(mapping_maximal(
            items.into_iter().flat_map(|i| i.0),
            T::map_partial_cmp,
        ))
    }

    fn combine<U, F: Fn(&T, &U) -> T>(&self, StateyInner(other): &StateyInner<U>, f: &F) -> Self
    where
        T: Clone,
    {
        let result = self
            .0
            .iter()
            .flat_map(move |a| other.iter().map(move |b| f(a, b)))
            .filter(T::is_bottom);

        StateyInner(mapping_maximal(result, T::map_partial_cmp))
    }
}

/// Collects the maximal elements of the iterator according to the `map_partial_cmp` function,
/// which returns a new element and the partial order between that element and the passed reference.
/// The new element will be used for further comparisons, so modifying the argument when returning `None`
/// may lead to unexpected behavior.
fn mapping_maximal<T>(
    elements: impl Iterator<Item = T>,
    mut map_partial_cmp: impl FnMut(T, &T) -> (T, Option<Ordering>),
) -> Vec<T> {
    let mut maximal = Vec::new();
    for element in elements {
        let control = maximal
            .iter()
            .enumerate()
            .try_fold(element, |element, (n, maximal)| {
                match map_partial_cmp(element, maximal) {
                    (e, Some(Ordering::Greater)) => ControlFlow::Break(Some((n, e))),
                    (e, None) => ControlFlow::Continue(e),
                    _ => ControlFlow::Break(None),
                }
            });
        match control {
            ControlFlow::Break(Some((n, mut new_maximal))) => {
                for m in ((n + 1)..maximal.len()).rev() {
                    match map_partial_cmp(new_maximal, &maximal[m]) {
                        (new, Some(Ordering::Greater)) => {
                            new_maximal = new;
                            maximal.swap_remove(m);
                        }
                        (new, None) => new_maximal = new,
                        _ => panic!("join function is not acting transitive"),
                    }
                }
                maximal[n] = new_maximal;
            }
            ControlFlow::Continue(new_maximal) => maximal.push(new_maximal),
            _ => {}
        }
    }

    maximal
}

impl<T: Eq + PartialOrd> Eq for StateyInner<T> {}

/// Represents one possible set of states and a value for this possibility.
#[derive(Clone, Debug)]
struct NodePossibility<T, V> {
    t: T,
    states: HashSet<V>,
}

impl<T, V: Hash + Eq + Clone> NodePossibility<T, V> {
    /// HACK: Because there's no GAT yet, these are implemented as inherent methods.
    /// Don't tell anybody!
    fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> NodePossibility<U, V> {
        NodePossibility {
            t: f(self.t),
            states: self.states,
        }
    }

    fn lift(t: T) -> Self {
        Self {
            t,
            states: HashSet::new(),
        }
    }

    fn combine<F: FnOnce(&T, &T) -> T>(&self, other: &Self, f: F) -> Self {
        Self {
            t: f(&self.t, &other.t),
            states: self.states.union(&other.states).cloned().collect(),
        }
    }

    fn apply_edge<F: FnOnce(&T, &T) -> T>(&self, edge: &EdgePossibility<T, V>, f: F) -> Self {
        let t = f(&self.t, &edge.t);

        let states = self
            .states
            .iter()
            .filter(|&v| {
                !edge.prior.get(v).unwrap_or(&true) && !edge.posterior.get(v).unwrap_or(&true)
            })
            .chain(edge.posterior.iter().filter_map(|(v, set)| set.then(|| v)))
            .cloned()
            .collect();

        Self { t, states }
    }
}

impl<T: Truthy, V: Hash + Eq + Clone> EdgePossibility<T, V> {
    fn from_states(prior: HashMap<V, bool>, posterior: HashMap<V, bool>) -> Self {
        Self {
            t: T::top(),
            prior,
            posterior,
        }
    }

    fn map<U, F: FnMut(T) -> U>(self, mut f: F) -> EdgePossibility<U, V> {
        EdgePossibility {
            t: f(self.t),
            prior: self.prior,
            posterior: self.posterior,
        }
    }

    fn lift(t: T) -> Self {
        Self {
            t,
            prior: HashMap::new(),
            posterior: HashMap::new(),
        }
    }

    fn combine<F: FnOnce(&T, &T) -> T>(&self, other: &Self, f: F) -> Self {
        let prior = map_union(&self.prior, &other.prior)
            .map(Result::ok)
            .map(|o| o.map(|(k, v)| (k.clone(), v.clone())))
            .collect::<Option<_>>();

        if let Some(prior) = prior {
            let posterior = map_union(&self.posterior, &self.posterior)
                .flatten()
                .map(|(k, v)| (k.clone(), v.clone()));
            Self {
                t: f(&self.t, &other.t),
                prior,
                posterior: posterior.collect(),
            }
        } else {
            Self {
                t: T::bottom(),
                prior: HashMap::new(),
                posterior: HashMap::new(),
            }
        }
    }
}

impl<T: Truthy + PartialOrd, V: Hash + Eq + Clone> StateyItem for NodePossibility<T, V> {
    fn map_partial_cmp(self, other: &Self) -> (Self, Option<Ordering>) {
        if &self > other {
            (self, Some(Ordering::Greater))
        } else if self.states == other.states {
            let this = NodePossibility {
                t: self.t.join(&other.t),
                states: self.states,
            };
            (this, Some(Ordering::Greater))
        } else {
            let cmp = self.partial_cmp(other);
            (self, cmp)
        }
    }

    fn is_bottom(&self) -> bool {
        self.t == T::bottom()
    }
}

#[derive(Clone, Debug)]
struct EdgePossibility<T, V> {
    t: T,
    prior: HashMap<V, bool>,
    posterior: HashMap<V, bool>,
}

impl<T: Truthy + PartialOrd, V: Hash + Eq + Clone> StateyItem for EdgePossibility<T, V> {
    fn map_partial_cmp(self, other: &Self) -> (Self, Option<Ordering>)
    where
        Self: Sized,
    {
        if &self > other {
            (self, Some(Ordering::Greater))
        } else if self.posterior == other.posterior && self.prior == other.prior {
            let this = EdgePossibility {
                t: self.t.join(&other.t),
                ..self
            };

            (this, Some(Ordering::Greater))
        } else {
            let cmp = self.partial_cmp(other);
            (self, cmp)
        }
    }

    fn is_bottom(&self) -> bool {
        self.t == T::bottom()
    }
}

// HACK: Perfect derive isn't available, so I can't derive these implementations
/*
BEGIN BOILERPLATE TRAIT IMPLS (perfect derive please)
*/

impl<T, V> PartialEq for Statey<T, V>
where
    T: PartialOrd,
    V: Hash + Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T, V> PartialOrd for Statey<T, V>
where
    T: PartialOrd,
    V: Hash + Eq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T, V> PartialEq for EdgeStatey<T, V>
where
    T: PartialOrd,
    V: Hash + Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T, V> PartialOrd for EdgeStatey<T, V>
where
    T: PartialOrd,
    V: Hash + Eq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T: PartialEq, V: Hash + Eq> PartialEq for NodePossibility<T, V> {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t && self.states == other.states
    }
}

impl<T: PartialOrd, V: Hash + Eq> PartialOrd for NodePossibility<T, V> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let t = self.t.partial_cmp(&other.t)?;
        let states = subset_partial_cmp(&self.states, &other.states)?;

        pointwise_ord(t, states)
    }
}

impl<T: PartialEq, V: Hash + Eq> PartialEq for EdgePossibility<T, V> {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t && self.prior == other.prior && self.posterior == other.posterior
    }
}

impl<T: PartialOrd, V: Hash + Eq> PartialOrd for EdgePossibility<T, V> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let t = self.t.partial_cmp(&other.t)?;
        let prior = submap_partial_cmp(&self.prior, &other.prior)?.reverse();
        let posterior = submap_partial_cmp(&self.posterior, &other.posterior)?;

        pointwise_ord(t, prior).and_then(|o| pointwise_ord(o, posterior))
    }
}

impl<T: PartialEq + PartialOrd> PartialEq for StateyInner<T> {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other)
            .map(Ordering::is_eq)
            .unwrap_or(false)
    }
}

impl<T: PartialEq + PartialOrd> PartialOrd for StateyInner<T> {
    fn partial_cmp(&self, StateyInner(other): &Self) -> Option<Ordering> {
        self.0
            .iter()
            .map(|a| {
                // Some if all comparing elements (at least one) are compatible
                other.iter().map(|b| a.partial_cmp(b)).fold(None, |a, n| {
                    n.and_then(|n| pointwise_ord(a.unwrap_or(n), n))
                })
            })
            // If all elements compare with at least one element in self,
            .try_fold(Ordering::Equal, |a, b| pointwise_ord(a, b?))
    }
}

fn subset_partial_cmp<V: Hash + Eq>(a: &HashSet<V>, b: &HashSet<V>) -> Option<Ordering> {
    if a.is_superset(b) {
        Some(Ordering::Greater)
    } else if a == b {
        Some(Ordering::Equal)
    } else if a.is_subset(b) {
        Some(Ordering::Less)
    } else {
        None
    }
}

fn submap_partial_cmp<K: Hash + Eq, V: Eq>(
    a: &HashMap<K, V>,
    b: &HashMap<K, V>,
) -> Option<Ordering> {
    if a.len() > b.len() {
        b.iter()
            .all(|(k, b)| a.get(k).map(|a| a == b).unwrap_or(false))
            .then(|| Ordering::Greater)
    } else if a.len() < b.len() {
        a.iter()
            .all(|(k, a)| b.get(k).map(|b| a == b).unwrap_or(false))
            .then(|| Ordering::Less)
    } else {
        (a == b).then(|| Ordering::Equal)
    }
}

fn pointwise_ord(a: Ordering, b: Ordering) -> Option<Ordering> {
    if a == b {
        Some(a)
    } else if b == Ordering::Equal {
        Some(a)
    } else if a == Ordering::Equal {
        Some(b)
    } else {
        None
    }
}

/// Returns the union of the elements of the two maps.
/// If the two maps contain the same key with the same value,
/// it ends up in the resulting iterator as `Ok((k, v))`.
/// If they have different values, the iterator will yield Err((k, a, b))
fn map_union<'a, K: Hash + Eq, V: Eq>(
    a: &'a HashMap<K, V>,
    b: &'a HashMap<K, V>,
) -> impl Iterator<Item = Result<(&'a K, &'a V), (&'a K, &'a V, &'a V)>> {
    a.iter()
        .map(|(k, a)| {
            let b = b.get(k).unwrap_or(a);
            if a == b {
                Ok((k, a))
            } else {
                Err((k, a, b))
            }
        })
        .chain(b.iter().filter(|(k, _)| a.contains_key(k)).map(Ok))
}
