use std::fmt::Debug;
use std::hash::Hash;
use std::mem;

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
