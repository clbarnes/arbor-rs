extern crate num_traits;

use num_traits::real::Real;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::mem;

fn cmp_len<T>(a: &Vec<T>, b: &Vec<T>) -> Ordering {
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

struct Location<CoordType> {
    x: CoordType,
    y: CoordType,
    z: CoordType,
}

impl<CoordType: Real> Location<CoordType> {
    fn distance_to(&self, other: Location<CoordType>) -> CoordType {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2) + (self.z - other.z).powi(2))
            .sqrt()
    }
}

struct RootwardPath<'a, NodeType: 'a + Hash> {
    // todo: should this check for cycles?
    arbor: &'a Arbor<NodeType>,
    start: NodeType,
    next: Option<NodeType>,
}

impl<'a, NodeType: Hash + Eq + Copy> RootwardPath<'a, NodeType> {
    fn new(arbor: &Arbor<NodeType>, start: NodeType) -> Result<RootwardPath<NodeType>, &str> {
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

impl<'a, NodeType: Copy + Hash + Eq> Iterator for RootwardPath<'a, NodeType> {
    type Item = NodeType;

    fn next(&mut self) -> Option<NodeType> {
        match self.next {
            Some(node) => mem::replace(
                &mut self.next,
                self.arbor.get_parent(node).map(|n| n.clone()),
            ),
            None => None,
        }
    }
}

struct NodesDistanceTo<NodeType: Hash, CoordType> {
    distances: HashMap<NodeType, CoordType>,
    max: CoordType,
}

impl<NodeType: Hash + Eq, CoordType: Ord + Clone> NodesDistanceTo<NodeType, CoordType> {
    fn new(distances: HashMap<NodeType, CoordType>) -> NodesDistanceTo<NodeType, CoordType> {
        let max = distances.values().cloned().max().unwrap();
        NodesDistanceTo { distances, max }
    }
}

fn distance_fn<NodeType: Hash, CoordType: Real> (
    node1: NodeType, node2: NodeType, positions: Option<HashMap<NodeType, Location<CoordType>>>
) -> CoordType{
    let msg = "positions did not contain all required nodes";
    match positions {
        Some(pos) => pos.get(n1).expect(msg).distance_to(pos.get(n2).expect(msg)),
        None => 1
    }
}

struct Arbor<NodeType: Hash> {
    edges: HashMap<NodeType, NodeType>,
    root: Option<NodeType>,
}

impl<NodeType: Hash + Eq + Copy> Arbor<NodeType> {
    fn new() -> Arbor<NodeType> {
        Arbor {
            edges: HashMap::new(),
            root: None,
        }
    }

    fn has_parent(&self, node: NodeType) -> bool {
        self.edges.contains_key(&node)
    }

    fn get_parent(&self, node: NodeType) -> Option<&NodeType> {
        self.edges.get(&node)
    }

    fn path_to_root(&self, start: NodeType) -> Result<RootwardPath<NodeType>, &str> {
        RootwardPath::new(&self, start)
    }

    fn check_valid_root(&self) -> Result<NodeType, &str> {
        match self.root {
            Some(root) => {
                if self.find_root()? == root {
                    Ok(root)
                } else {
                    Err("Arbor invalid: explicit root does not match implicit root")
                }
            }
            None => Err("Arbor invalid: no explicit root"),
        }?

    }

    fn check_valid(&self) -> Result<&Arbor<NodeType>, &str> {
        let root = self.check_valid_root()?;

        let mut global_visited: HashSet<NodeType> = HashSet::new();
        global_visited.insert(root);
        let mut intersects: bool;

        for start in self.edges.keys() {
            let mut local_visited: HashSet<NodeType> = HashSet::new();
            intersects = false;

            for node in self.path_to_root(start.clone())? {
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

    fn find_root(&self) -> Result<NodeType, &str> {
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

    fn add_edges(&mut self, edges: Vec<NodeType>) -> &mut Arbor<NodeType> {
        for chunk in edges.chunks(2) {
            self.edges.insert(chunk[0], chunk[1]);
        }
        let new_root = self.find_root().unwrap();
        mem::replace(&mut self.root, Some(new_root));
        self
    }

    fn add_path(&mut self, path: Vec<NodeType>) -> &mut Arbor<NodeType> {
        let possible_root = path.first().expect("Given empty path to add");
        let should_replace = !self.has_node(possible_root);
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

    fn reroot(&mut self, new_root: NodeType) -> &mut Arbor<NodeType> {
        let mut path = vec![];

        for new_proximal in self.path_to_root(new_root) {
            path.push(new_proximal);
            self.edges.remove(new_proximal)
        }

        self.root = Some(new_root);
        self.add_path(path);

        self
    }

    fn nodes_distance_to_root<CoordType: Real> (
        &self, positions: Option<HashMap<NodeType, Location<CoordType>>>
    ) -> HashMap<NodeType, CoordType> {
        self.nodes_distance_to(self.root.expect("Arbor has no root"), positions)
    }

    fn nodes_distance_to<CoordType: Real>(
        &self,
        target: NodeType,
        positions: Option<HashMap<NodeType, Location<CoordType>>>,
    ) -> HashMap<NodeType, CoordType> {
        let distance_fn = match positions {
            Some(pos) => | n1: NodeType, n2 : NodeType | {

            },
            None => | n1: NodeType, n2: NodeType | 1
        };

        let successors = self.all_successors();

        let mut dists: HashMap<NodeType, CoordType> = HashMap::new();
        dists.insert(target, 0);

        let mut to_visit = successors.get(&target).unwrap_or(&vec![]).to_owned();

        while !to_visit.is_empty() {
            let distal = to_visit.pop().expect("unreachable: popped from non-empty vec");
            let proximal = self.edges.get(&distal).expect("unreachable");
            let dist_to_prox = dists.get(proximal).expect("unreachable");

            dists.insert(distal, *dist_to_prox + distance_fn(proximal, distal));

            for next in successors.get(&distal).unwrap_or(&vec![]).iter() {
                to_visit.push(*next);
            }
        }

        dists
    }

    fn all_successors(&self) -> HashMap<NodeType, Vec<NodeType>> {
        let mut out: HashMap<NodeType, Vec<NodeType>> = HashMap::new();
        for (succ, pred) in self.edges.iter() {
            let entry = out.entry(pred.clone()).or_insert(Vec::new());
            entry.push(succ.clone());
        }
        out
    }

    fn children(&self) -> HashSet<NodeType> {
        self.edges.keys().cloned().collect()
    }

    fn find_branch_and_end_nodes(&self) -> BranchAndEndNodes<NodeType> {
        let mut out_degrees: HashMap<NodeType, usize> = HashMap::new();
        for (distal, proximal) in self.edges.iter() {
            let degree_entry = out_degrees.entry(proximal.clone()).or_insert(0);
            *degree_entry += 1
        }

        let mut branches: HashMap<NodeType, usize> = HashMap::new();
        let mut ends: HashSet<NodeType> = HashSet::new();

        for (node, degree) in out_degrees.iter() {
            match degree {
                0 => {
                    ends.insert(node.clone());
                    ()
                }
                1 => (),
                _ => {
                    branches.insert(node.clone(), degree.clone());
                    ()
                }
            };
        }

        BranchAndEndNodes::new(branches, ends)
    }

    fn partition(&self) -> Vec<Vec<NodeType>> {
        // todo
        Vec::new()
    }

    fn partition_sorted(&self) -> Vec<Vec<NodeType>> {
        let mut partitions = self.partition();
        partitions.sort_by(cmp_len);
        partitions
    }

    fn all_neighbours(&self) -> HashMap<NodeType, Vec<NodeType>> {
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

    fn flow_centrality(
        self,
        targets: HashMap<NodeType, usize>,
        sources: HashMap<NodeType, usize>,
    ) -> Option<HashMap<NodeType, FlowCentrality>> {
        // todo
        None
    }

    fn has_node(self, node: NodeType) -> bool {
        self.edges.contains_key(&node) || match self.root {Some(n) => n == node, None => false}
    }

    fn nodes(self) -> HashSet<NodeType> {
        let mut out: HashSet<NodeType> = HashSet::new();
        for (key, value) in self.edges.iter() {
            out.insert(key.clone());
            out.insert(value.clone());
        }
        match self.root {
            Some(node) => {
                out.insert(node.clone());
                out
            }
            None => out,
        }
    }

    fn nodes_order_from_root(&self) -> HashMap<NodeType, usize> {
        self.nodes_distance_to_root(None)
    }

    fn nodes_order_from(&self, target: NodeType) -> HashMap<NodeType, usize> {
        self.nodes_distance_to(target, None)
    }

    fn sub_arbor(self, new_root: NodeType) -> Arbor<NodeType> {
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
}

struct BranchAndEndNodes<NodeType> {
    branches: HashMap<NodeType, usize>,
    ends: HashSet<NodeType>,
    n_branches: usize,
}

impl<NodeType: Hash + Eq> BranchAndEndNodes<NodeType> {
    fn new(
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

struct FlowCentrality {
    centrifugal: usize,
    centripetal: usize,
    sum: usize,
}

impl FlowCentrality {
    fn new(centrifugal: usize, centripetal: usize) -> FlowCentrality {
        FlowCentrality {
            centrifugal,
            centripetal,
            sum: centrifugal + centripetal,
        }
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
        let mut arbor = make_arbor();
        assert!(arbor.has_parent(4));
    }

    #[test]
    fn get_parent() {
        let mut arbor = make_arbor();
        assert_eq!(*arbor.get_parent(4).unwrap(), 3);
    }

    #[test]
    fn check_valid() {
        let mut arbor = make_arbor();
        arbor.check_valid().expect("Not valid");
    }

    #[test]
    fn reroot() {
        let mut arbor = make_arbor();
        arbor.reroot(3);
        assert_eq!(arbor.root.expect("no root"), 3);
        arbor.check_valid().expect("Rerooted tree not valid");
    }
}
