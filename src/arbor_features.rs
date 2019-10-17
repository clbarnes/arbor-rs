use std::fmt::Debug;
use std::hash::Hash;
use std::mem;

use std::collections::VecDeque;
use utils::{FastMap, FastSet};
use Arbor;

pub struct RootwardPath<'a, NodeType: 'a + Hash + Clone + Eq> {
    arbor: &'a Arbor<NodeType>,
    next: Option<NodeType>,
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> RootwardPath<'a, NodeType> {
    pub fn new(
        arbor: &Arbor<NodeType>,
        start: NodeType,
    ) -> Result<RootwardPath<NodeType>, &'static str> {
        if !arbor.has_parent(start) && arbor.root.map_or(false, |n| start != n) {
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

/// Iterate over partitions in the tree as vectors of nodes,
/// starting with a leaf and ending with a root or branch.
/// The partitions are in order of the leaf nodes (highest first).
/// The first partition ends at the root;
/// subsequent partitions end at branch nodes already included in a previous partition.
pub struct PartitionsTopological<'a, NodeType: 'a + Hash + Clone + Eq> {
    arbor: &'a Arbor<NodeType>,
    branches: FastMap<NodeType, bool>,
    ends: Vec<NodeType>,
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> PartitionsTopological<'a, NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> PartitionsTopological<NodeType> {
        let branch_ends = arbor.find_branch_and_end_nodes();
        let mut ends: Vec<NodeType> = branch_ends.ends.iter().cloned().collect();

        ends.sort_unstable();

        PartitionsTopological {
            arbor,
            branches: branch_ends.branches.keys().map(|k| (*k, false)).collect(),
            ends,
        }
    }
}

impl<'a, NodeType: Hash + Debug + Eq + Copy + Ord> Iterator
    for PartitionsTopological<'a, NodeType>
{
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

/// JS-style partitioning, where the root is in the longest (unweighted) partition
/// but the order (including location of the longest partition) is non-deterministic.
/// In this implementation, the longest partition will be yielded first,
/// and the remaining order is deterministic.
pub struct PartitionsClassic<NodeType: Hash + Clone + Eq> {
    partitions: VecDeque<Vec<NodeType>>,
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> PartitionsClassic<NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> Self {
        let branch_ends = arbor.find_branch_and_end_nodes();
        let mut ends: Vec<NodeType> = branch_ends.ends.iter().cloned().collect();
        ends.sort_unstable(); // for determinism

        let branches = branch_ends.branches;
        let mut partitions: VecDeque<Vec<NodeType>> = VecDeque::default();

        // mapping of branch node to vec of slabs beneath it
        let mut junctions: FastMap<NodeType, Vec<Vec<NodeType>>> =
            branches.keys().map(|k| (*k, Vec::default())).collect();

        // partitions yet to reach completion
        let mut open: VecDeque<Vec<NodeType>> = ends.iter().map(|n| vec![*n]).collect();

        while !open.is_empty() {
            // partition to be grown
            let mut seq = open.pop_front().unwrap();
            let mut last_node = *seq.last().unwrap();

            let mut parent: Option<NodeType> = None;
            let mut n_successors: Option<usize> = None;

            // grow seq toward root until it reaches the root or a branch point
            while n_successors.is_none() {
                parent = arbor.edges.get(&last_node).map(|n| *n);
                if let Some(p) = parent {
                    seq.push(p);
                    n_successors = branches.get(&p).map(|v| *v);
                    last_node = p;
                } else {
                    break;
                }
            }

            if parent.is_none() {
                // reached the root, add seq to the front of vec of completed partitions
                partitions.push_front(seq);
            } else {
                // reached a branch

                let mut junction = junctions.get_mut(&last_node).expect("pre-populated");
                junction.push(seq);

                if junction.len() < n_successors.unwrap() {
                    // not all branch's downstream partners have been seen
                    continue;
                }

                // longest seq should be added to open, so it can get longer and reach root
                // others should be added to partitions

                let mut junction_iter = junction.drain(..);
                let mut longest = junction_iter.next().unwrap();

                for this_seq in junction_iter {
                    if this_seq.len() > longest.len() {
                        partitions.push_back(longest);
                        longest = this_seq;
                    } else {
                        partitions.push_back(this_seq);
                    }
                }

                open.push_back(longest);
            }
        }

        Self { partitions }
    }
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> Iterator for PartitionsClassic<NodeType> {
    type Item = Vec<NodeType>;

    fn next(&mut self) -> Option<Vec<NodeType>> {
        self.partitions.pop_front()
    }
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct BranchAndEndNodes<NodeType: Hash + Eq> {
    pub branches: FastMap<NodeType, usize>,
    pub ends: FastSet<NodeType>,
    pub n_branches: usize,
}

impl<NodeType: Hash + Eq + Copy + Ord + Debug> BranchAndEndNodes<NodeType> {
    pub fn new(arbor: &Arbor<NodeType>) -> Self {
        let mut branches: FastMap<NodeType, usize> = FastMap::default();
        let mut ends: FastSet<NodeType> = FastSet::default();

        for (node, degree) in arbor.out_degrees().iter() {
            match degree {
                0 => {
                    ends.insert(node.clone());
                }
                1 => (),
                _ => {
                    branches.insert(node.clone(), degree.clone());
                }
            };
        }

        let n_branches = branches.len();
        BranchAndEndNodes {
            branches,
            ends,
            n_branches,
        }
    }
}
