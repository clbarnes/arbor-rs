use hashbrown::{HashMap, HashSet};
use num::traits::float::Float;
use num::traits::real::Real;
use num::Integer;
use num::Zero;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::Hash;
use std::mem;
use std::ops::Sub;
use Arbor;

// todo: trait alias https://github.com/rust-lang/rfcs/pull/1733
//#[derive(Hash, Debug, Eq, Copy, Ord)]
//struct NodeType<T: Hash + Debug + Eq + Copy + Ord> {
//    id: T
//}

// Type aliases allow easier switching between hash implementations
/// Cryptographically insecure mapping, generally for <NodeType, SomethingElse>
pub type FastMap<T, U> = HashMap<T, U>;

/// Cryptographically insecure mapping, generally for <NodeType, SomethingElse>
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
        match self.next {
            Some(node) => mem::replace(&mut self.next, self.arbor.get_parent(node).cloned()),
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
    branches: FastMap<NodeType, bool>,
    ends: Vec<NodeType>,
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Partitions<'a, NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> Partitions<NodeType> {
        let branch_ends = arbor.find_branch_and_end_nodes();
        let mut ends: Vec<NodeType> = branch_ends.ends.iter().cloned().collect();

        ends.sort_unstable();

        Partitions {
            arbor,
            branches: branch_ends.branches.keys().map(|k| (*k, false)).collect(),
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
                        return path;
                    }
                }
            }
            path
        })
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

pub struct BranchAndEndNodes<NodeType> {
    pub branches: FastMap<NodeType, usize>,
    pub ends: FastSet<NodeType>,
    pub n_branches: usize,
}

impl<NodeType: Hash + Eq> BranchAndEndNodes<NodeType> {
    pub fn new(
        branches: FastMap<NodeType, usize>,
        ends: FastSet<NodeType>,
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

// could base this on DFS instead
pub struct Toposort<'a, NodeType: 'a + Hash + Clone + Eq> {
    partitions: Partitions<'a, NodeType>,
    to_visit: Vec<NodeType>,
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Toposort<'a, NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> Toposort<NodeType> {
        let mut partitions = arbor.partition();
        let to_visit = partitions.next().map_or(Vec::new(), |v| v.clone());
        Toposort {
            partitions,
            to_visit,
        }
    }
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Iterator for Toposort<'a, NodeType> {
    type Item = NodeType;

    fn next(&mut self) -> Option<NodeType> {
        if self.to_visit.is_empty() {
            if let Some(v) = self.partitions.next().as_mut() {
                self.to_visit.append(v);
                self.to_visit.pop();
            }
        }
        self.to_visit.pop()
    }
}

pub struct DepthFirstSearch<NodeType: Hash + Clone + Eq> {
    successors: FastMap<NodeType, Vec<NodeType>>,
    to_yield: Vec<(NodeType, Option<NodeType>)>,
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> DepthFirstSearch<NodeType> {
    pub fn from_root(arbor: &Arbor<NodeType>) -> Result<DepthFirstSearch<NodeType>, &'static str> {
        Self::new(arbor, arbor.root.expect("Arbor has no root"))
    }

    pub fn new(
        arbor: &Arbor<NodeType>,
        root: NodeType,
    ) -> Result<DepthFirstSearch<NodeType>, &'static str> {
        let successors = arbor.all_successors();
        if successors.contains_key(&root) {
            Ok(DepthFirstSearch {
                successors,
                to_yield: vec![(root, None)],
            })
        } else {
            Err("Given root is not in arbor")
        }
    }
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> Iterator for DepthFirstSearch<NodeType> {
    type Item = (NodeType, Option<NodeType>);

    fn next(&mut self) -> Option<(NodeType, Option<NodeType>)> {
        self.to_yield.pop().map(|(n, p)| {
            let mut succ = self.successors[&n].clone();
            succ.sort_unstable();
            self.to_yield.extend(succ.iter().map(|c| (*c, Some(n))));
            (n, p)
        })
    }
}
