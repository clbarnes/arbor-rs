use std::fmt::Debug;
use std::hash::Hash;

use arbor_features::PartitionsTopological;
use utils::FastMap;
use Arbor;

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
            if let Some(mut successors) = self.successors.remove(&n) {
                successors.sort_unstable();
                self.to_yield
                    .extend(successors.drain(..).map(|c| (c, Some(n))));
            }
            (n, p)
        })
    }
}
