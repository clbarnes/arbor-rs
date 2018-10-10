extern crate core;

use std::collections::HashMap;
use std::collections::HashSet;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use std::mem;

fn cmp_len(a: &Vec<T>, b: &Vec<T>) -> Ordering {
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

struct InvalidArborError;

impl fmt::Display for InvalidArborError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "arbor is not topographically valid")
    }
}

struct Arbor {
    edges: HashMap<u64, u64>,
    root: Option<u64>,
}

impl Arbor {
    fn new() -> Arbor {
        Arbor{ edges: HashMap::new(), root: None }
    }

    fn find_root(self) -> Result<u64, E> {
        let mut distals: HashSet<u64> = HashSet::new();
        let mut proximals: HashSet<u64> = HashSet::new();

        for (distal, proximal) in self.edges.iter() {
            distals.insert(distal.copy());
            proximals.insert(proximal.copy());
        }

        let diff: Vec<u64> = proximals.difference(&distals).collect();

        match diff.len() {
            1 => Ok(diff[0]),
            0 => {
                if distals.len() == 0 {
                    match self.root {
                        Some(N) => Ok(N),
                        None => Err("Arbor invalid: no edges or nodes")
                    }
                } else {
                    Err("Arbor invalid: no implicit root")
                }
            },
            _ => Err("Arbor invalid: more than one implicit root")
        }
    }

    fn add_edges(&mut self, edges: Vec<u64>) -> &mut Arbor {
        for chunk in edges.chunks(2) {
            self.edges.insert(chunk[0], chunk[1]);
        }
        mem::replace(self.root, Some(self.find_root().unwrap()));
        self
    }

    fn add_path(&mut self, path: Vec<u64>) -> &mut Arbor {
        let possible_root = path.first().unwrap();
        let should_replace = !self.edges.contains_key(possible_root);
        let mut is_valid = self.edges.is_empty();

        for proximal_distal in path.windows(2) {
            is_valid = is_valid || self.edges.contains_key(proximal) || self.edges.contains_key(distal)
            self.edges.insert(proximal_distal[1], proximal_distal[0])
        }

        if !is_valid {
            panic!("Path does not intersect with existing arbor")
        }

        if should_replace {
            mem::replace(self.root, Some(possible_root))
        }
        self
    }

    fn reroot(&mut self, new_root: u64) -> &mut Arbor{
        self
    }

    fn nodes_distance_to(self, root: u64, positions: HashMap<u64, [f64]>) -> HashMap<u64, f64> {
        // todo
    }

    fn all_successors(self) -> HashMap<u64, Vec<u64>> {
        let mut out: HashMap<u64, Vec<u64>> = HashMap::new();
        for (succ, pred) in self.edges.iter() {
            let entry = out.entry(pred.copy()).or_insert(Vec::new());
            *entry.push(succ.copy())
        }
        out
    }

    fn children(self) -> HashSet<u64> {
        HashSet::from_iter(self.edges.keys())
    }

    fn find_branch_and_end_nodes(self) -> BranchAndEndNodes {
        let mut out_degrees: HashMap<u64, usize>;
        for (distal, proximal) in self.edges.iter() {
            let degree_entry = out_degrees.entry(proximal.copy()).or_insert(0);
            *degree_entry += 1
        }

        let mut branches: HashMap<u64, usize> = HashMap::new();
        let mut ends: HashSet<u64> = HashSet::new();

        for (node, degree) in out_degrees.iter() {
            match degree {
                0 => ends.insert(node.copy()),
                1 => (),
                _ => branches.insert(node.copy(), degree.copy()),
            };
        }

        BranchAndEndNodes::new(branches, ends)
    }

    fn partition(self) -> Vec<Vec<u64>> {
        // todo
    }

    fn partition_sorted(self) -> Vec<Vec<u64>> {
        let mut partitions = self.partition();
        partitions.sort_by(cmp_len);
        partitions
    }

    fn all_neighbours(self) -> HashMap<u64, Vec<u64>> {
        let mut out: HashMap<u64, Vec<u64>> = HashMap::new();
        for (succ, pred) in self.edges.iter() {
            let pred_entry = out.entry(pred.copy()).or_insert(Vec::new());
            *pred_entry.push(succ.copy());

            let succ_entry = out.entry(succ.copy()).or_insert(Vec::new());
            *succ_entry.push(pred.copy());
        }
        out
    }

    fn flow_centrality(
        self, targets: HashMap<u64, u64>, sources: HashMap<u64, i64>
    ) -> Option<HashMap<u64, FlowCentrality>> {
        // todo
    }

    fn nodes(self) -> HashSet<u64> {
        let mut out: HashSet<u64> = HashSet::new();
        for (key, value) in self.edges.iter() {
            out.insert(key.copy());
            out.insert(value.copy());
        }
        match self.root {
            Some(N) => {
                out.insert(N.copy());
                out
            },
            None => out
        }
    }

    fn nodes_order_from(self, root: Option<u64>) -> HashMap<u64, u64> {
        let root = match root {Some(N) => N, None => self.root};
    }

    fn sub_arbor(self, new_root: u64) -> Arbor {
        let succs = self.all_successors();
        to_visit = vec![new_root];

        let mut edges: HashMap<u64, u64> = HashMap::new();

        while !to_visit.is_empty() {
            let proximal = to_visit.pop();
            match succs.get(proximal) {
                Some(V) => {
                    for distal in V.iter() {
                        edges.insert(distal.copy(), proximal);
                        to_visit.push(distal)
                    }
                },
                None => ()
            }
        }

        Arbor {edges, root: Some(new_root)}
    }
}

struct BranchAndEndNodes {
    branches: HashMap<u64, usize>,
    ends: HashSet<u64>,
    n_branches: usize
}

impl BranchAndEndNodes {
    fn new(branches: HashMap<u64, usize>, ends: HashSet<u64>) -> BranchAndEndNodes {
        BranchAndEndNodes {
            branches,
            ends,
            n_branches: branches.len(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
