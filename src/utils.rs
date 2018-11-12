use fxhash::{FxHashMap, FxHashSet};
use num::traits::float::Float;
use num::traits::real::Real;
use num::Integer;
use num::Zero;
use std::cmp::Ordering;
use std::collections::hash_map::Keys;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Sub;
use Arbor;

// todo: trait alias https://github.com/rust-lang/rfcs/pull/1733
//#[derive(Hash, Debug, Eq, Copy, Ord)]
//struct NodeType<T: Hash + Debug + Eq + Copy + Ord> {
//    id: T
//}

// Type aliases allow easier switching between hash implementations
/// Cryptographically insecure mapping, generally for <NodeType, SomethingElse>
pub type FastMap<T, U> = FxHashMap<T, U>;
pub type FastKeys<'a, T, U> = Keys<'a, T, U>;

/// Cryptographically insecure set, generally for <NodeType>
pub type FastSet<T> = FxHashSet<T>;

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

#[derive(Deserialize, Debug, Copy, Clone, PartialEq)]
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
        (self - *other).norm()
    }
}

#[derive(Debug, Deserialize, PartialEq)]
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

#[derive(Debug, PartialEq, Deserialize)]
pub struct NodesDistanceTo<NodeType: Hash + Debug + Eq, D> {
    pub distances: FastMap<NodeType, D>,
    pub max: D,
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

#[derive(Debug, PartialEq, Deserialize)]
pub struct ArborRegions<NodeType: Hash + Debug + Eq + Copy + Ord> {
    pub above: FastSet<NodeType>,
    pub plateau: FastSet<NodeType>,
    pub zeros: FastSet<NodeType>,
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> Default for ArborRegions<NodeType> {
    fn default() -> Self {
        ArborRegions {
            above: FastSet::default(),
            plateau: FastSet::default(),
            zeros: FastSet::default(),
        }
    }
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Axon<NodeType: Hash + Debug + Eq + Copy + Ord> {
    pub arbor: Arbor<NodeType>,
    pub fc_max_plateau: FastSet<NodeType>,
    pub fc_zeros: FastSet<NodeType>,
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> Axon<NodeType> {
    pub fn new(arbor: Arbor<NodeType>, regions: ArborRegions<NodeType>) -> Self {
        Self {
            arbor,
            fc_max_plateau: regions.plateau,
            fc_zeros: regions.zeros,
        }
    }
}
