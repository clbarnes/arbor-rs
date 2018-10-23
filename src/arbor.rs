use utils::{
    cmp_len, BranchAndEndNodes, FlowCentrality, Location, NodesDistanceTo, Partitions, RootwardPath,
};

use fnv::{FnvHashMap, FnvHashSet};
use num::traits::float::Float;
use num::Zero;
use std::collections::hash_map::Keys;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::Chain;
use std::option::Iter;
use utils::DepthFirstSearch;
use utils::Toposort;

#[derive(Clone, Debug, PartialEq)]
pub struct Arbor<NodeType: Hash + Clone + Eq> {
    pub(crate) edges: FnvHashMap<NodeType, NodeType>,
    pub root: Option<NodeType>,
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> Default for Arbor<NodeType> {
    /// Create an empty arbor
    fn default() -> Self {
        Arbor {
            edges: FnvHashMap::default(),
            root: None,
        }
    }
}

impl<NodeType: Hash + Debug + Eq + Copy + Ord> Arbor<NodeType> {
    /// Creates a populated arbor and checks that it is valid
    pub fn new(
        edges: FnvHashMap<NodeType, NodeType>,
        root: Option<NodeType>,
    ) -> Result<Arbor<NodeType>, &'static str> {
        let mut a = Arbor { edges, root };
        if root.is_none() {
            a.root = a.find_root()?;
        }
        a.check_valid()?;
        Ok(a)
    }

    /// Return whether arbor has any nodes
    fn is_empty(&self) -> bool {
        self.edges.is_empty() && self.root.is_none()
    }

    /// Add a node without any edges (errors if arbor has nodes already)
    pub fn add_singleton_node(&mut self, root: NodeType) -> Result<(), &'static str> {
        if !self.is_empty() {
            return Err("Arbor already has nodes");
        } else {
            self.root = Some(root);
        }
        Ok(())
    }

    /// Return whether the given node has a proximal partner
    pub fn has_parent(&self, node: NodeType) -> bool {
        self.edges.contains_key(&node)
    }

    /// Return the proximal partner of the given node
    pub fn get_parent(&self, node: NodeType) -> Option<&NodeType> {
        self.edges.get(&node)
    }

    /// Return an iterator of nodes between the given node and the root
    pub fn path_to_root(&self, start: NodeType) -> Result<RootwardPath<NodeType>, &'static str> {
        RootwardPath::new(&self, start)
    }

    /// Error if the arbor's root is inconsistent with its edges
    fn check_valid_root(&self) -> Result<Option<NodeType>, &'static str> {
        match self.root {
            Some(root) => {
                if self.find_root()? == Some(root) {
                    Ok(Some(root))
                } else {
                    Err("Explicit and implicit roots differ")
                }
            }
            None => {
                if self.edges.is_empty() {
                    Ok(None)
                } else {
                    Err("No explicit root")
                }
            }
        }
    }

    /// Error if the arbor is inconsistent: e.g. root does not match edges, arbor has cycles,
    /// arbor has more than one connected component
    pub fn check_valid(&self) -> Result<&Arbor<NodeType>, &'static str> {
        let root = self.check_valid_root()?;
        if root.is_none() {
            return Ok(self);
        }

        let mut global_visited: FnvHashSet<NodeType> = FnvHashSet::default();
        global_visited.insert(root.expect("already checked for nones"));
        let mut intersects: bool;

        for start in self.edges.keys() {
            let mut local_visited: FnvHashSet<NodeType> = FnvHashSet::default();
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

    /// Return the root, based on the edges
    fn find_root(&self) -> Result<Option<NodeType>, &'static str> {
        if self.edges.is_empty() {
            return Ok(self.root);
        }

        let distals: FnvHashSet<NodeType> = self.edges.keys().cloned().collect();
        let proximals: FnvHashSet<NodeType> = self.edges.values().cloned().collect();

        let diff: Vec<NodeType> = proximals.difference(&distals).cloned().collect();

        match diff.len() {
            1 => Ok(Some(diff[0])),
            0 => Err("Arbor invalid: no implicit root"),
            _ => Err("Arbor invalid: more than one implicit root"),
        }
    }

    /// Add edges and set the root to match the new topology.
    pub fn add_edges(&mut self, edges: &[NodeType]) -> Result<&mut Arbor<NodeType>, &'static str> {
        let mut to_add: FnvHashMap<NodeType, NodeType> = FnvHashMap::default();
        let mut has_intersected = self.is_empty();

        for chunk in edges.chunks(2) {
            if to_add.insert(chunk[0], chunk[1]).is_some() || self.edges.contains_key(&chunk[0]) {
                return Err("Node would have two parents");
            }

            // edge case: this is the reverse of an existing edge (bad)
            let intersects = self.has_node(chunk[0]) || self.has_node(chunk[1]);

            if has_intersected {
                if intersects {
                    return Err("New edges intersect with arbor more than once");
                }
            } else {
                has_intersected = intersects;
            }
        }

        if !has_intersected {
            return Err("New edges do not intersect with existing arbor");
        }

        for (k, v) in to_add.drain() {
            self.edges.insert(k, v);
        }

        // todo: check for validity before adding
        // these panic intentionally to prevent leaving the arbor in a dirty state
        self.root = self.find_root().unwrap();
        self.check_valid().unwrap();
        Ok(self)
    }

    /// Add path, setting the root to the first node in the new path unless that node is already in the arbor.
    pub fn add_path(&mut self, path: &[NodeType]) -> Result<&mut Arbor<NodeType>, &'static str> {
        // todo: un-spaghetti this

        // catch trivial cases
        match path.len() {
            0 => return Ok(self),
            1 => {
                let only = path.first().expect("definitely has first");
                if self.has_node(*only) {
                    return Ok(self);
                } else if self.is_empty() {
                    self.root = Some(*only);
                    return Ok(self);
                } else {
                    return Err("Path does not intersect with arbor");
                }
            }
            _ => (),
        }

        // check path isn't bad
        let uniques: FnvHashSet<NodeType> = path.iter().cloned().collect();
        if uniques.len() != path.len() {
            return Err("Path intersects with itself");
        }

        let possible_root = path.first().expect("Already checked for emptiness");

        if self.is_empty() {
            self.root = Some(*possible_root);
        }

        let must_reroot = !self.has_node(*possible_root);
        let mut has_intersected = !must_reroot;

        let mut edges_to_add: Vec<(NodeType, NodeType)> = Vec::new();

        for pair in path.windows(2) {
            let proximal: NodeType;
            let distal: NodeType;

            if has_intersected {
                // add pairs in proper order
                proximal = pair[0];
                distal = pair[1];

                if self.has_node(distal) {
                    return Err("Path intersects existing arbor more than once");
                }
            } else {
                // add pairs in reverse order: the arbor will be rerooted at the end, reversing them
                distal = pair[0];
                proximal = pair[1];

                has_intersected = self.has_node(proximal);
            }
            edges_to_add.push((distal, proximal));
        }

        if !has_intersected {
            return Err("Path does not intersect with existing arbor");
        }

        for (dist, prox) in edges_to_add.iter() {
            self.edges.insert(*dist, *prox);
        }

        if must_reroot {
            self.reroot(*possible_root);
        }

        Ok(self)
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
        positions: FnvHashMap<NodeType, Location<F>>,
    ) -> NodesDistanceTo<NodeType, F> {
        self.nodes_distance_to(self.root.expect("Arbor has no root"), positions)
    }

    pub fn nodes_distance_to<F: Float>(
        &self,
        target: NodeType,
        positions: FnvHashMap<NodeType, Location<F>>,
    ) -> NodesDistanceTo<NodeType, F> {
        let msg = "positions did not contain all required nodes";
        let distance_fn = |n1: NodeType, n2: NodeType| {
            positions
                .get(&n1)
                .expect(msg)
                .clone()
                .distance_to(positions.get(&n2).expect(msg).clone())
        };

        let dists = self.nodes_edge_metric(target, distance_fn);

        NodesDistanceTo::from_distances(dists)
    }

    fn nodes_edge_metric<F: Fn(NodeType, NodeType) -> T, T: Zero + Clone>(
        &self,
        target: NodeType,
        distance_fn: F,
    ) -> FnvHashMap<NodeType, T> {
        let mut dists: FnvHashMap<NodeType, T> = FnvHashMap::default();

        for (distal, proximal) in self.dfs(target) {
            match proximal {
                Some(p) => {
                    let dist_to_prox = dists[&p].clone();
                    dists.insert(distal, dist_to_prox + distance_fn(p, distal))
                }
                None => dists.insert(distal, T::zero()),
            };
        }

        dists
    }

    pub fn all_successors(&self) -> FnvHashMap<NodeType, Vec<NodeType>> {
        // todo: cache this
        let mut out: FnvHashMap<NodeType, Vec<NodeType>> =
            self.nodes().map(|n| (*n, Vec::new())).collect();

        for (succ, pred) in self.edges.iter() {
            out.entry(*pred).and_modify(|v| v.push(*succ));
        }
        out
    }

    pub fn children(&self) -> FnvHashSet<NodeType> {
        self.edges.keys().cloned().collect()
    }

    pub fn out_degrees(&self) -> FnvHashMap<NodeType, usize> {
        // todo: maybe cache this?
        let mut degree: FnvHashMap<NodeType, usize> = self.nodes().map(|n| (*n, 0)).collect();
        for proximal in self.edges.values() {
            degree.entry(*proximal).and_modify(|c| *c += 1);
        }
        degree
    }

    pub fn find_branch_and_end_nodes(&self) -> BranchAndEndNodes<NodeType> {
        // todo: cache this
        let mut branches: FnvHashMap<NodeType, usize> = FnvHashMap::default();
        let mut ends: FnvHashSet<NodeType> = FnvHashSet::default();

        for (node, degree) in self.out_degrees().iter() {
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

    pub fn all_neighbours(&self) -> FnvHashMap<NodeType, Vec<NodeType>> {
        let mut out: FnvHashMap<NodeType, Vec<NodeType>> = FnvHashMap::default();
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
        targets: FnvHashMap<NodeType, usize>,
        sources: FnvHashMap<NodeType, usize>,
    ) -> Option<FnvHashMap<NodeType, FlowCentrality>> {
        unimplemented!()
    }

    pub fn has_node(&self, node: NodeType) -> bool {
        self.edges.contains_key(&node) || match self.root {
            Some(n) => n == node,
            None => false,
        }
    }

    /// Iterate over nodes
    pub fn nodes(&self) -> Chain<Keys<NodeType, NodeType>, Iter<NodeType>> {
        self.edges.keys().chain(self.root.iter())
    }

    /// Return a map of node to topographical path lengths to the root, for all nodes
    pub fn nodes_order_from_root(&self) -> NodesDistanceTo<NodeType, usize> {
        self.nodes_order_from(self.root.expect("Arbor has no root"))
    }

    /// Return a map of node to topographical path lengths to given target,
    /// for all nodes distal to that target
    pub fn nodes_order_from(&self, target: NodeType) -> NodesDistanceTo<NodeType, usize> {
        let orders = self.nodes_edge_metric(target, |_, _| 1);

        NodesDistanceTo::from_orders(orders)
    }

    /// Return a new arbor rooted at the given node and containing all nodes distal to it.
    pub fn sub_arbor(&self, new_root: NodeType) -> Arbor<NodeType> {
        let mut edges: FnvHashMap<NodeType, NodeType> = FnvHashMap::default();
        for (distal, proximal) in self.dfs(new_root) {
            if let Some(p) = proximal {
                edges.insert(distal, p);
            }
        }

        Arbor {
            edges,
            root: Some(new_root),
        }
    }

    /// Remove given node and all nodes distal to it.
    pub fn prune(&mut self, cut: NodeType) -> &Arbor<NodeType> {
        self.dfs(cut).map(|(d, _p)| self.edges.remove(&d)).last();

        self
    }

    pub fn toposort(&self) -> Toposort<NodeType> {
        Toposort::new(self)
    }

    pub fn dfs(&self, root: NodeType) -> DepthFirstSearch<NodeType> {
        DepthFirstSearch::new(self, root).expect("Bad root") // todo: handle errors
    }

    pub fn dfs_from_root(&self) -> DepthFirstSearch<NodeType> {
        DepthFirstSearch::from_root(self).expect("Bad root") // todo: handle errors
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
        let mut arbor = Arbor::default();
        arbor.add_edges(&edges);
        arbor
    }

    #[test]
    fn add_edges() {
        let mut arbor = Arbor::default();
        arbor.add_edges(&vec![5, 4, 4, 3, 3, 2, 2, 1, 7, 6, 6, 3]);
        assert_eq!(arbor.root.unwrap(), 1);
        assert_eq!(arbor.edges.len(), 6);
    }

    #[test]
    fn add_path() {
        let mut arbor = Arbor::default();
        arbor.add_path(&vec![1, 2, 3, 4, 5]).expect("fail");
        assert_eq!(arbor.root.unwrap(), 1);
        arbor.add_path(&vec![3, 6, 7]).expect("fail");
        assert_eq!(arbor.root.unwrap(), 1);
        assert_eq!(arbor.edges.len(), 6);
    }

    #[test]
    fn add_path_reroot() {
        let mut arbor = make_arbor();
        arbor.add_path(&vec![8, 6, 9]).expect("fail");
        arbor.check_valid().expect("New arbor is not valid");
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

        let expected_branches: FnvHashMap<u64, usize> = vec![(3, 2)].into_iter().collect();
        assert_eq!(branch_ends.branches, expected_branches);

        let expected_ends: FnvHashSet<u64> = vec![5, 7].into_iter().collect();
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
    fn prune() {
        let mut arbor = make_arbor();
        arbor.prune(3);

        assert_eq!(arbor.root.expect("no root"), 1);
        let expected_edges = vec![(2, 1)].into_iter().collect();
        assert_eq!(arbor.edges, expected_edges);
    }

    #[test]
    fn nodes_order_from() {
        let arbor = make_arbor();

        let orders = arbor.nodes_order_from(3);
        let expected: FnvHashMap<u64, usize> = vec![(3, 0), (4, 1), (5, 2), (6, 1), (7, 2)]
            .into_iter()
            .collect();

        let expected = NodesDistanceTo::from_orders(expected);
        assert_eq!(orders, expected);
    }

    #[test]
    fn nodes_distance_to() {
        let arbor = make_arbor();

        let locations: FnvHashMap<u64, Location<f64>> = vec![
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
        let expected: FnvHashMap<u64, f64> = vec![(3, 0.0), (4, 1.0), (5, 2.0), (6, 3.0), (7, 4.0)]
            .into_iter()
            .collect();

        let expected = NodesDistanceTo::from_distances(expected);
        assert_eq!(orders, expected);
    }

    #[test]
    fn partitions() {
        let arbor = make_arbor();
        let partitions: Vec<Vec<u64>> = arbor.partition().collect();
        assert_eq!(partitions.len(), 2);
        assert_eq!(partitions[0], vec![7, 6, 3, 2, 1]);
        assert_eq!(partitions[1], vec![5, 4, 3]);
    }

    #[test]
    fn toposort() {
        let arbor = make_arbor();
        let sorted: Vec<u64> = arbor.toposort().collect();
        assert_eq!(sorted, vec![1, 2, 3, 6, 7, 4, 5]);
    }
}
