use fnv::{FnvHashMap, FnvHashSet};
use num::traits::float::Float;
use num::traits::real::Real;
use num::Integer;
use num::Zero;
use serde::Deserialize;
use std::cmp::Ordering;
use std::collections::hash_map::Keys;
use std::fmt::Debug;
use std::hash::Hash;
use std::mem;
use std::ops::Sub;
use Arbor;
use std::collections::hash_map::Entry;

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
    pub fn distance_to(self, other: Location<F>) -> F {
        (self - other).norm()
    }
}

pub struct RootwardPath<'a, NodeType: 'a + Hash + Clone + Eq> {
    arbor: &'a Arbor<NodeType>,
    next: Option<NodeType>,
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> RootwardPath<'a, NodeType> {
    pub fn new(
        arbor: &Arbor<NodeType>,
        start: NodeType,
    ) -> Result<RootwardPath<NodeType>, &'static str> {
        if !arbor.has_parent(start) && arbor.root.clone().map_or(false, |n| start != n) {
            Err("No path to root: Arbor does not contain starting node")
        } else {
            Ok(RootwardPath {
                arbor,
                next: Some(start),
            })
        }
    }
}

impl<'a, NodeType: Copy + Debug + Hash + Eq + Ord> Iterator for RootwardPath<'a, NodeType> {
    type Item = NodeType;

    fn next(&mut self) -> Option<NodeType> {
        // todo: should this check for cycles?
        match self.next {
            Some(node) => mem::replace(
                &mut self.next,
                self.arbor.get_parent(node).map(|n| n.clone()),
            ),
            None => None,
        }
    }
}

//pub struct Partitions<'a, NodeType: 'a + Hash + Clone + Eq> {
//    arbor: &'a Arbor<NodeType>,
//    visited: FnvHashSet<NodeType>, // todo: use branches instead, save memory
//    ends: Vec<NodeType>,
//}
//
//impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Partitions<'a, NodeType> {
//    pub fn new(arbor: &Arbor<NodeType>) -> Partitions<NodeType> {
//        let mut ends: Vec<NodeType> = arbor
//            .find_branch_and_end_nodes()
//            .ends
//            .iter()
//            .cloned()
//            .collect();
//        ends.sort_unstable(); // for deterministic results
//
//        Partitions {
//            arbor,
//            visited: FnvHashSet::default(),
//            ends,
//        }
//    }
//}
//
//impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Iterator for Partitions<'a, NodeType> {
//    type Item = Vec<NodeType>;
//
//    fn next(&mut self) -> Option<Vec<NodeType>> {
//        // todo: check whether this actually needs a true longest partition as per JS
//        self.ends.pop().map(|start| {
//            let mut path: Vec<NodeType> = Vec::new();
//            for node in self
//                .arbor
//                .path_to_root(start)
//                .expect("end must be in arbor")
//            {
//                path.push(node);
//                if self.visited.contains(&node) {
//                    break;
//                }
//                self.visited.insert(node);
//            }
//            path
//        })
//    }
//}

pub struct Partitions<'a, NodeType: 'a + Hash + Clone + Eq> {
    arbor: &'a Arbor<NodeType>,
    branches: FnvHashMap<NodeType, bool>,
    ends: Vec<NodeType>,
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Partitions<'a, NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> Partitions<NodeType> {
        let mut branch_ends = arbor.find_branch_and_end_nodes();
        let mut ends: Vec<NodeType> = branch_ends.ends
            .iter()
            .cloned()
            .collect();

        ends.sort_unstable();

        Partitions {
            arbor,
            branches: branch_ends.branches.keys().map(| k | (*k, false )).collect(),
            ends,
        }
    }
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Iterator for Partitions<'a, NodeType> {
    type Item = Vec<NodeType>;

    fn next(&mut self) -> Option<Vec<NodeType>> {
        // todo: check whether this actually needs a true longest partition as per JS
        self.ends.pop().map(|start| {
            let mut path: Vec<NodeType> = Vec::new();
            for node in self
                .arbor
                .path_to_root(start)
                .expect("end must be in arbor")
            {
                path.push(node);

                // must be a better way to do this with Entry
                if let Some(was_visited) = self.branches.remove(&node) {
                    self.branches.insert(node, true);
                    if was_visited {
                        return path
                    }
                }
            }
            path
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct NodesDistanceTo<NodeType: Hash + Debug + Eq, D> {
    distances: FnvHashMap<NodeType, D>,
    max: D,
}

impl<NodeType: Hash + Debug + Eq, I: Integer + Clone> NodesDistanceTo<NodeType, I> {
    pub fn from_orders(orders: FnvHashMap<NodeType, I>) -> NodesDistanceTo<NodeType, I> {
        let max = orders.values().max().unwrap().to_owned();
        NodesDistanceTo {
            distances: orders,
            max,
        }
    }
}

impl<NodeType: Hash + Debug + Eq, F: Real + Clone> NodesDistanceTo<NodeType, F> {
    pub fn from_distances(distances: FnvHashMap<NodeType, F>) -> NodesDistanceTo<NodeType, F> {
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

pub struct BranchAndEndNodes<NodeType> {
    pub branches: FnvHashMap<NodeType, usize>,
    pub ends: FnvHashSet<NodeType>,
    pub n_branches: usize,
}

impl<NodeType: Hash + Eq> BranchAndEndNodes<NodeType> {
    pub fn new(
        branches: FnvHashMap<NodeType, usize>,
        ends: FnvHashSet<NodeType>,
    ) -> BranchAndEndNodes<NodeType> {
        let n_branches = branches.len();
        BranchAndEndNodes {
            branches,
            ends,
            n_branches,
        }
    }
}

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
