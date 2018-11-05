use hashbrown::hash_map::Keys;
use hashbrown::{HashMap, HashSet};
use num::traits::float::Float;
use num::traits::real::Real;
use num::{Integer, Zero};
use serde::Deserialize;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Sub;

// todo: trait alias https://github.com/rust-lang/rfcs/pull/1733
//#[derive(Hash, Debug, Eq, Copy, Ord)]
//struct NodeType<T: Hash + Debug + Eq + Copy + Ord> {
//    id: T
//}

// Type aliases allow easier switching between hash implementations
/// Cryptographically insecure mapping, generally for <NodeType, SomethingElse>
pub type FastMap<T, U> = HashMap<T, U>;
pub type FastKeys<'a, T, U> = Keys<'a, T, U>;

/// Cryptographically insecure set, generally for <NodeType>
pub type FastSet<T> = HashSet<T>;

pub fn cmp_len<T>(a: &Vec<T>, b: &Vec<T>) -> Ordering {
    let a_len = a.len();
    let b_len = b.len();
    if a_len < b_len {
        Ordering::Less
    } else if a_len > b_len {
        Ordering::Greater
    } else {
        Ordering::Equal
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct Location<F: Float> {
    pub x: F,
    pub y: F,
    pub z: F,
}

impl<F: Float> Location<F> {
    pub fn norm(&self) -> F {
        (self.x.powi(2) + self.y.powi(2) + self.z.powi(2)).sqrt()
    }
}

impl<F: Float> Sub<Location<F>> for Location<F> {
    type Output = Location<F>;

    fn sub(self, rhs: Location<F>) -> <Self as Sub<Location<F>>>::Output {
        Location {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl<F: Float> Location<F> {
    pub fn distance_to(self, other: &Location<F>) -> F {
        (self - other.clone()).norm()
    }
}

#[derive(Debug, Deserialize)]
pub struct FlowCentrality {
    pub centrifugal: usize,
    pub centripetal: usize,
    pub sum: usize,
}

impl FlowCentrality {
    pub fn new(centrifugal: usize, centripetal: usize) -> FlowCentrality {
        FlowCentrality {
            centrifugal,
            centripetal,
            sum: centrifugal + centripetal,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct NodesDistanceTo<NodeType: Hash + Debug + Eq, D> {
    distances: FastMap<NodeType, D>,
    max: D,
}

impl<NodeType: Hash + Debug + Eq, I: Integer + Clone> NodesDistanceTo<NodeType, I> {
    pub fn from_orders(orders: FastMap<NodeType, I>) -> NodesDistanceTo<NodeType, I> {
        let max = orders.values().max().unwrap().to_owned();
        NodesDistanceTo {
            distances: orders,
            max,
        }
    }
}

impl<NodeType: Hash + Debug + Eq, F: Real + Clone> NodesDistanceTo<NodeType, F> {
    pub fn from_distances(distances: FastMap<NodeType, F>) -> NodesDistanceTo<NodeType, F> {
        let mut max: F = Zero::zero();

        for d in distances.values() {
            max = match max.partial_cmp(d).unwrap_or(Ordering::Greater) {
                Ordering::Less => *d,
                _ => max,
            };
        }

        NodesDistanceTo {
            distances,
            max: max.to_owned(),
        }
    }
}
