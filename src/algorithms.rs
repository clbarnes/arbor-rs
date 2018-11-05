use std::fmt::Debug;
use std::hash::Hash;

use arbor_features::Partitions;
use utils::FastMap;
use Arbor;

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
