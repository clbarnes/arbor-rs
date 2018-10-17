use utils::{
    cmp_len, BranchAndEndNodes, FlowCentrality, Location, NodesDistanceTo, Partitions, RootwardPath,
};

use num::traits::float::Float;
use num::Zero;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::mem;
use utils::NodesIterator;

pub struct Arbor<NodeType: Hash> {
    pub(crate) edges: HashMap<NodeType, NodeType>,
    pub root: Option<NodeType>,
}

impl<NodeType: Hash + Eq + Copy + Ord> Arbor<NodeType> {
    /// Creates an empty arbor
    pub fn new() -> Arbor<NodeType> {
        Arbor {
            edges: HashMap::new(),
            root: None,
        }
    }

    /// Creates a populated arbor and checks that it is valid
    pub fn from<'a>(
        edges: HashMap<NodeType, NodeType>,
        root: Option<NodeType>,
    ) -> Result<Arbor<NodeType>, &'static str> {
        let a = Arbor { edges, root };
        a.check_valid()?;
        Ok(a)
    }

    /// Checks whether arbor has any nodes
    fn is_empty(&self) -> bool {
        self.edges.is_empty() && match self.root {
            Some(n) => false,
            None => true,
        }
    }

    /// Adds a node without any edges (errors if arbor has nodes already)
    pub fn add_singleton_node(&mut self, root: NodeType) -> Result<(), &'static str> {
        if !self.is_empty() {
            return Err("Arbor already has nodes");
        } else {
            self.root = Some(root);
        }
        Ok(())
    }

    pub fn has_parent(&self, node: NodeType) -> bool {
        self.edges.contains_key(&node)
    }

    pub fn get_parent(&self, node: NodeType) -> Option<&NodeType> {
        self.edges.get(&node)
    }

    pub fn path_to_root(&self, start: NodeType) -> Result<RootwardPath<NodeType>, &'static str> {
        RootwardPath::new(&self, start)
    }

    fn check_valid_root(&self) -> Result<NodeType, &'static str> {
        match self.root {
            Some(root) => {
                if self.find_root()? == root {
                    Ok(root)
                } else {
                    Err("Arbor invalid: explicit root does not match implicit root")
                }
            }
            None => Err("Arbor invalid: no explicit root"),
        }
    }

    pub fn check_valid(&self) -> Result<&Arbor<NodeType>, &'static str> {
        let root = self.check_valid_root()?;

        let mut global_visited: HashSet<NodeType> = HashSet::new();
        global_visited.insert(root);
        let mut intersects: bool;

        for start in self.edges.keys() {
            let mut local_visited: HashSet<NodeType> = HashSet::new();
            intersects = false;

            for node in self.path_to_root(*start)? {
                if !local_visited.insert(node) {
                    return Err("Arbor has cycles");
                }

                if !global_visited.insert(node) {
                    intersects = true;
                    break;
                }
            }

            if !intersects {
                return Err("Arbor is not fully-connected (some nodes do not lead to root)");
            }
        }

        Ok(self)
    }

    fn find_root(&self) -> Result<NodeType, &'static str> {
        let mut distals: HashSet<NodeType> = HashSet::new();
        let mut proximals: HashSet<NodeType> = HashSet::new();

        for (distal, proximal) in self.edges.iter() {
            distals.insert(distal.clone());
            proximals.insert(proximal.clone());
        }

        let diff: Vec<NodeType> = proximals.difference(&distals).cloned().collect();

        match diff.len() {
            1 => Ok(diff[0]),
            0 => {
                if distals.len() == 0 {
                    self.root.ok_or("Arbor invalid: no edges or nodes")
                } else {
                    Err("Arbor invalid: no implicit root")
                }
            }
            _ => Err("Arbor invalid: more than one implicit root"),
        }
    }

    pub fn add_edges(&mut self, edges: Vec<NodeType>) -> &mut Arbor<NodeType> {
        for chunk in edges.chunks(2) {
            self.edges.insert(chunk[0], chunk[1]);
        }
        let new_root = self.find_root().unwrap();
        mem::replace(&mut self.root, Some(new_root));
        self
    }

    pub fn add_path(&mut self, path: Vec<NodeType>) -> &mut Arbor<NodeType> {
        let possible_root = path.first().expect("Given empty path to add");
        let should_replace = !self.has_node(*possible_root);
        let mut intersects = self.edges.is_empty() || !should_replace;

        for proximal_distal in path.windows(2) {
            intersects = intersects || self.edges.contains_key(&proximal_distal[1]);
            self.edges.insert(proximal_distal[1], proximal_distal[0]);
        }

        if !intersects {
            panic!("Path does not intersect with existing arbor")
        }

        if should_replace {
            mem::replace(&mut self.root, Some(possible_root.clone()));
        }
        self
    }

    pub fn reroot(&mut self, new_root: NodeType) -> &mut Arbor<NodeType> {
        let path: Vec<NodeType> = self
            .path_to_root(new_root)
            .expect("New root not in arbor")
            .collect();

        for old_dist_prox in path.windows(2).rev() {
            let old_dist = old_dist_prox[0];
            let old_prox = old_dist_prox[1];

            self.edges.remove(&old_dist);
            self.edges.insert(old_prox, old_dist);
        }

        self.root = Some(new_root);

        self
    }

    pub fn nodes_distance_to_root<F: Float>(
        &self,
        positions: HashMap<NodeType, Location<F>>,
    ) -> NodesDistanceTo<NodeType, F> {
        self.nodes_distance_to(self.root.expect("Arbor has no root"), positions)
    }

    pub fn nodes_distance_to<F: Float>(
        &self,
        target: NodeType,
        positions: HashMap<NodeType, Location<F>>,
    ) -> NodesDistanceTo<NodeType, F> {
        let msg = "positions did not contain all required nodes";
        let distance_fn = |n1: NodeType, n2: NodeType| {
            positions
                .get(&n1)
                .expect(msg)
                .clone()
                .distance_to(positions.get(&n2).expect(msg).clone())
        };

        let successors = self.all_successors();

        let mut dists: HashMap<NodeType, F> = HashMap::new();
        dists.insert(target, Zero::zero());

        let mut to_visit = successors.get(&target).unwrap_or(&vec![]).to_owned();

        while !to_visit.is_empty() {
            let distal = to_visit
                .pop()
                .expect("unreachable: popped from non-empty vec");
            let proximal = self.edges.get(&distal).expect("unreachable");
            let dist_to_prox = *dists.get(proximal).expect("unreachable");

            dists.insert(distal, dist_to_prox + distance_fn(*proximal, distal));

            for next in successors.get(&distal).unwrap_or(&vec![]).iter() {
                to_visit.push(*next);
            }
        }

        NodesDistanceTo::from_distances(dists)
    }

    pub fn all_successors(&self) -> HashMap<NodeType, Vec<NodeType>> {
        let mut out: HashMap<NodeType, Vec<NodeType>> = HashMap::new();
        for (succ, pred) in self.edges.iter() {
            let entry = out.entry(pred.clone()).or_insert(Vec::new());
            entry.push(succ.clone());
        }
        out
    }

    pub fn children(&self) -> HashSet<NodeType> {
        self.edges.keys().cloned().collect()
    }

    pub fn find_branch_and_end_nodes(&self) -> BranchAndEndNodes<NodeType> {
        let mut out_degrees: HashMap<NodeType, usize> = HashMap::new();
        for (distal, proximal) in self.edges.iter() {
            out_degrees.entry(*distal).or_insert(0);

            let prox_entry = out_degrees.entry(*proximal).or_insert(0);
            *prox_entry += 1
        }

        let mut branches: HashMap<NodeType, usize> = HashMap::new();
        let mut ends: HashSet<NodeType> = HashSet::new();

        for (node, degree) in out_degrees.iter() {
            match degree {
                0 => { ends.insert(node.clone()); },
                1 => (),
                _ => { branches.insert(node.clone(), degree.clone()); }
            };
        }

        BranchAndEndNodes::new(branches, ends)
    }

    pub fn partition(&self) -> Partitions<NodeType> {
        Partitions::new(self)
    }

    pub fn partition_sorted(&self) -> Vec<Vec<NodeType>> {
        let mut partitions: Vec<Vec<NodeType>> = self.partition().collect();
        partitions.sort_by(cmp_len);
        partitions
    }

    pub fn all_neighbours(&self) -> HashMap<NodeType, Vec<NodeType>> {
        let mut out: HashMap<NodeType, Vec<NodeType>> = HashMap::new();
        for (succ, pred) in self.edges.iter() {
            out.entry(pred.clone())
                .or_insert(Vec::new())
                .push(succ.clone());

            out.entry(succ.clone())
                .or_insert(Vec::new())
                .push(pred.clone());
        }
        out
    }

    pub fn flow_centrality(
        &self,
        targets: HashMap<NodeType, usize>,
        sources: HashMap<NodeType, usize>,
    ) -> Option<HashMap<NodeType, FlowCentrality>> {
        unimplemented!()
    }

    pub fn has_node(&self, node: NodeType) -> bool {
        self.edges.contains_key(&node) || match self.root {
            Some(n) => n == node,
            None => false,
        }
    }

    /// Iterate over nodes
    pub fn nodes(&self) -> NodesIterator<NodeType> {
        NodesIterator::new(self)
    }

    /// Return a map of node to topographical path lengths to the root, for all nodes
    pub fn nodes_order_from_root(&self) -> NodesDistanceTo<NodeType, usize> {
        self.nodes_order_from(self.root.expect("Arbor has no root"))
    }

    /// Return a map of node to topographical path lengths to given target,
    /// for all nodes distal to that target
    pub fn nodes_order_from(&self, target: NodeType) -> NodesDistanceTo<NodeType, usize> {
        // todo: shared logic with nodes_distance_to, how to factor out?
        let successors = self.all_successors();

        let mut orders: HashMap<NodeType, usize> = HashMap::new();
        orders.insert(target, 0);

        let mut to_visit = successors.get(&target).unwrap_or(&vec![]).to_owned();

        while !to_visit.is_empty() {
            let distal = to_visit
                .pop()
                .expect("unreachable: popped from non-empty vec");
            let proximal = self.edges.get(&distal).expect("unreachable");
            let dist_to_prox = *orders.get(proximal).expect("unreachable");

            orders.insert(distal, dist_to_prox + 1);

            for next in successors.get(&distal).unwrap_or(&vec![]).iter() {
                to_visit.push(*next);
            }
        }

        NodesDistanceTo::from_orders(orders)
    }

    /// Return a new arbor rooted at the given node and containing all nodes distal to it.
    pub fn sub_arbor(&self, new_root: NodeType) -> Arbor<NodeType> {
        let succs = self.all_successors();
        let mut to_visit = vec![new_root];

        let mut edges: HashMap<NodeType, NodeType> = HashMap::new();

        while !to_visit.is_empty() {
            let proximal = to_visit
                .pop()
                .expect("Impossible: failed to pop from non-empty Vec");
            for distal in succs.get(&proximal).unwrap_or(&vec![]).iter() {
                edges.insert(distal.clone(), proximal);
                to_visit.push(distal.clone());
            }
        }

        Arbor {
            edges,
            root: Some(new_root),
        }
    }

    /// Remove given node and all nodes distal to it.
    pub fn prune(&mut self, cut: NodeType) -> &Arbor<NodeType> {
        // shared logic with sub_arbor
        // todo: DownstreamEdges iterator

        let successors = self.all_successors();

        let mut to_visit = vec![cut];

        while !to_visit.is_empty() {
            let to_remove = to_visit
                .pop()
                .expect("unreachable: popped from non-empty vec");

            self.edges.remove(&to_remove);

            for next in successors.get(&to_remove).unwrap_or(&vec![]).iter() {
                to_visit.push(*next);
            }
        }
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    //    1
    //    |
    //    2
    //    |
    //    3
    //    | \
    //    4  6
    //    |   \
    //    5    7

    fn make_arbor() -> Arbor<u64> {
        let edges: Vec<u64> = vec![5, 4, 4, 3, 3, 2, 2, 1, 7, 6, 6, 3];
        let mut arbor = Arbor::new();
        arbor.add_edges(edges);
        arbor
    }

    #[test]
    fn add_edges() {
        let mut arbor = Arbor::new();
        arbor.add_edges(vec![5, 4, 4, 3, 3, 2, 2, 1, 7, 6, 6, 3]);
        assert_eq!(arbor.root.unwrap(), 1);
        assert_eq!(arbor.edges.len(), 6);
    }

    #[test]
    fn add_path() {
        let mut arbor = Arbor::new();
        arbor.add_path(vec![1, 2, 3, 4, 5]);
        assert_eq!(arbor.root.unwrap(), 1);
        arbor.add_path(vec![3, 6, 7]);
        assert_eq!(arbor.root.unwrap(), 1);
        assert_eq!(arbor.edges.len(), 6);
    }

    #[test]
    fn has_parent() {
        let arbor = make_arbor();
        assert!(arbor.has_parent(4));
    }

    #[test]
    fn get_parent() {
        let arbor = make_arbor();
        assert_eq!(*arbor.get_parent(4).unwrap(), 3);
    }

    #[test]
    fn check_valid() {
        let arbor = make_arbor();
        arbor.check_valid().expect("Not valid");
    }

    #[test]
    fn reroot() {
        let mut arbor = make_arbor();
        arbor.reroot(3);
        assert_eq!(arbor.root.expect("no root"), 3);
        arbor.check_valid().expect("Rerooted tree not valid");
    }

    #[test]
    fn find_branch_and_end_nodes() {
        let arbor = make_arbor();
        let branch_ends = arbor.find_branch_and_end_nodes();

        let mut expected_branches: HashMap<u64, usize> = vec![(3, 2)].into_iter().collect();
        assert_eq!(branch_ends.branches, expected_branches);

        let mut expected_ends: HashSet<u64> = vec![5, 7].into_iter().collect();
        assert_eq!(branch_ends.ends, expected_ends);

        assert_eq!(branch_ends.n_branches, 1)
    }

    #[test]
    fn sub_arbor() {
        let arbor = make_arbor();
        let subarbor = arbor.sub_arbor(3);

        assert_eq!(subarbor.root.expect("no root"), 3);
        let expected_edges = vec![(5, 4), (4, 3), (6, 3), (7, 6)].into_iter().collect();
        assert_eq!(subarbor.edges, expected_edges);
    }

    #[test]
    fn nodes_order_from() {
        let arbor = make_arbor();

        let orders = arbor.nodes_order_from(3);
        let expected: HashMap<u64, usize> = vec![(3, 0), (4, 1), (5, 2), (6, 1), (7, 2)]
            .into_iter()
            .collect();

        assert_eq!(orders, expected);
    }

    #[test]
    fn nodes_distance_to() {
        let arbor = make_arbor();

        let locations: HashMap<u64, Location<f64>> = vec![
            (
                1,
                Location {
                    x: 1.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
            (
                2,
                Location {
                    x: 2.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
            (
                3,
                Location {
                    x: 3.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
            (
                4,
                Location {
                    x: 4.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
            (
                5,
                Location {
                    x: 5.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
            (
                6,
                Location {
                    x: 6.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
            (
                7,
                Location {
                    x: 7.0,
                    y: 0.0,
                    z: 0.0,
                },
            ),
        ].into_iter()
            .collect();

        let orders = arbor.nodes_distance_to(3, locations);
        let expected: HashMap<u64, f64> = vec![(3, 0.0), (4, 1.0), (5, 2.0), (6, 3.0), (7, 4.0)]
            .into_iter()
            .collect();

        assert_eq!(orders, expected);
    }

    #[test]
    fn empty_test() {
        let mut arbor = make_arbor();
    }

    #[test]
    fn partitions() {
        let arbor = make_arbor();
        let partitions: Vec<Vec<u64>> = arbor.partition().collect();
        assert_eq!(partitions.len(), 2);
        assert_eq!(partitions[0], vec![7, 6, 3, 2, 1]);
        assert_eq!(partitions[1], vec![5, 4, 3]);
    }
}
