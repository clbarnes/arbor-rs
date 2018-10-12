use num::traits::real::Real;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::mem;
use Arbor;

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

pub struct Location<CoordType> {
    pub x: CoordType,
    pub y: CoordType,
    pub z: CoordType,
}

impl<CoordType: Real> Location<CoordType> {
    pub fn distance_to(&self, other: &Location<CoordType>) -> CoordType {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2) + (self.z - other.z).powi(2))
            .sqrt()
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

pub struct NodesDistanceTo<NodeType: Hash, CoordType> {
    distances: HashMap<NodeType, CoordType>,
    max: CoordType,
}

impl<NodeType: Hash + Eq, CoordType: Ord + Clone> NodesDistanceTo<NodeType, CoordType> {
    pub fn new(distances: HashMap<NodeType, CoordType>) -> NodesDistanceTo<NodeType, CoordType> {
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
