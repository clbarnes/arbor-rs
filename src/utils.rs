use num::traits::float::Float;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::mem;
use serde::Deserialize;
use Arbor;
use std::ops::Sub;
use num::Integer;


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

#[derive(Deserialize, Debug, Clone)]
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

pub struct RootwardPath<'a, NodeType: 'a + Hash> {
    arbor: &'a Arbor<NodeType>,
    start: NodeType,
    next: Option<NodeType>,
}

impl<'a, NodeType: Hash + Eq + Copy + Ord> RootwardPath<'a, NodeType> {
    pub fn new(arbor: &Arbor<NodeType>, start: NodeType) -> Result<RootwardPath<NodeType>, &str> {
        if !arbor.has_parent(start) && arbor.root.clone().map_or(false, |n| start != n) {
            Err("No path to root: Arbor does not contain starting node")
        } else {
            Ok(RootwardPath {
                arbor,
                start,
                next: Some(start),
            })
        }
    }
}

impl<'a, NodeType: Copy + Hash + Eq + Ord> Iterator for RootwardPath<'a, NodeType> {
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

pub struct Partitions<'a, NodeType: 'a + Hash> {
    arbor: &'a Arbor<NodeType>,
    visited: HashSet<NodeType>, // todo: use branches instead, save memory
    ends: Vec<NodeType>,
}

impl<'a, NodeType: Hash + Eq + Copy + Ord> Partitions<'a, NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> Partitions<NodeType> {
        let mut ends: Vec<NodeType> = arbor
            .find_branch_and_end_nodes()
            .ends
            .iter()
            .cloned()
            .collect();
        ends.sort_unstable(); // for deterministic results

        Partitions {
            arbor,
            visited: HashSet::new(),
            ends,
        }
    }
}

impl<'a, NodeType: Hash + Eq + Copy + Ord> Iterator for Partitions<'a, NodeType> {
    type Item = Vec<NodeType>;

    fn next(&mut self) -> Option<Vec<NodeType>> {
        // todo: check whether this actually needs a true longest partition as per JS
        match self.ends.pop() {
            Some(start) => {
                let mut path: Vec<NodeType> = Vec::new();
                for node in self
                    .arbor
                    .path_to_root(start)
                    .expect("end must be in arbor")
                {
                    path.push(node);
                    if self.visited.contains(&node) {
                        break;
                    }
                    self.visited.insert(node);
                }
                Some(path)
            }
            None => None,
        }
    }
}

pub struct NodesDistanceTo<NodeType: Hash, Float> {
    distances: HashMap<NodeType, Float>,
    max: Float,
}

impl<NodeType: Hash + Eq, Float: Ord + Clone> NodesDistanceTo<NodeType, Float> {
    pub fn new(distances: HashMap<NodeType, Float>) -> NodesDistanceTo<NodeType, Float> {
        let max = distances.values().cloned().max().unwrap();
        NodesDistanceTo { distances, max }
    }
}

pub struct BranchAndEndNodes<NodeType> {
    pub branches: HashMap<NodeType, usize>,
    pub ends: HashSet<NodeType>,
    pub n_branches: usize,
}

impl<NodeType: Hash + Eq> BranchAndEndNodes<NodeType> {
    pub fn new(
        branches: HashMap<NodeType, usize>,
        ends: HashSet<NodeType>,
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
