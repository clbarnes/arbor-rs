use num::Float;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::repeat;
use utils::FastMap;
use utils::NodesDistanceTo;
use Arbor;
use ArborParser;

pub struct SynapseClustering<NodeType: Hash + Copy + Eq + Debug + Ord, F: Float> {
    arbor: Arbor<NodeType>,
    synapses: FastMap<NodeType, usize>,
    lambda: F,
    distances_to_root: FastMap<NodeType, F>,
    synapse_distances: Option<FastMap<NodeType, Vec<F>>>,
}

impl<NodeType: Hash + Copy + Eq + Debug + Ord> SynapseClustering<NodeType, f64> {
    pub fn new(ap: ArborParser<NodeType, f64>, lambda: f64) -> Self {
        Self {
            arbor: ap.arbor.clone(),
            synapses: ap.create_synapse_map(),
            lambda,
            distances_to_root: ap.distances_to_root().distances,
            synapse_distances: None,
        }
    }

    fn _calculate_synapse_distances(&self) -> FastMap<NodeType, Vec<f64>> {
        // treenode ID to list of distances to treenodes with synapses
        // Ds
        let mut synapse_distances: FastMap<NodeType, Vec<f64>> = FastMap::default();

        // branch node to list of upstream treenode IDs
        let mut seen_downstream_nodes: FastMap<NodeType, Vec<NodeType>> = FastMap::default();

        let max_distance = self.lambda * 3.0;

        // todo: check partition ordering
        for partition in self.arbor.partition() {
            let mut downstream_nodes: Vec<NodeType> = Vec::default();
            let mut partition_it = partition.iter();

            let mut prev_treenode_id = partition_it.next().expect("partitions should not be empty");

            for treenode_id in partition_it {
                downstream_nodes.push(*prev_treenode_id);

                let n_synapses = self.synapses.get(prev_treenode_id).unwrap_or(&0);

                if !synapse_distances.contains_key(prev_treenode_id) {
                    synapse_distances.insert(*prev_treenode_id, Vec::default());
                }

                if n_synapses > &0 {
                    let d = self.distances_to_root[prev_treenode_id];

                    for child_id in downstream_nodes.iter().cloned() {
                        let ds = synapse_distances.get_mut(&child_id).expect("should exist?");
                        let distance_child_to_synapse = self.distances_to_root[&child_id] - d;

                        if distance_child_to_synapse <= max_distance {
                            for v in repeat(distance_child_to_synapse).take(*n_synapses) {
                                ds.push(v)
                            }
                        }
                    }
                }

                let distance_to_root = self
                    .distances_to_root
                    .get(treenode_id)
                    .expect("should exist");
                let distance_prev_to_current = self
                    .distances_to_root
                    .get(prev_treenode_id)
                    .expect("should exist")
                    - distance_to_root;

                if let Some(seen) = seen_downstream_nodes.remove(treenode_id) {
                    let current_ds = synapse_distances
                        .get(treenode_id)
                        .expect("should exist if seen does")
                        .clone();

                    for child_id in downstream_nodes.iter().cloned() {
                        let mut child_ds =
                            synapse_distances.get_mut(&child_id).expect("should exist");
                        let distance = self.distances_to_root.get(&child_id).expect("should exist")
                            - distance_to_root;

                        if distance <= max_distance {
                            for current_d in current_ds.iter().cloned() {
                                child_ds.push(current_d + distance);
                            }
                        }
                    }

                    let prev_ds = synapse_distances
                        .get(prev_treenode_id)
                        .expect("should exist?")
                        .clone();

                    for child_id in seen {
                        let mut child_ds =
                            synapse_distances.get_mut(&child_id).expect("should exist");
                        let distance = self.distances_to_root.get(&child_id).expect("should exist")
                            - distance_to_root;

                        if distance <= max_distance {
                            for prev_d in prev_ds.iter().cloned() {
                                child_ds.push(prev_d + distance);
                            }
                        }

                        downstream_nodes.push(child_id);
                    }
                }

                let mut translated_prev_ds: Vec<f64> = Vec::default();

                for prev_d in synapse_distances
                    .get(prev_treenode_id)
                    .expect("assigned earlier")
                {
                    let distance = prev_d + distance_prev_to_current;
                    if distance < max_distance {
                        translated_prev_ds.push(distance);
                    }
                }

                let current_ds = synapse_distances
                    .entry(*treenode_id)
                    .or_insert(Vec::default());
                current_ds.append(&mut translated_prev_ds);

                prev_treenode_id = treenode_id;
            }

            let last_treenode_id = partition.last().expect("partitions should not be empty");
            seen_downstream_nodes.insert(*last_treenode_id, downstream_nodes);
        }

        if let Some(n_synapses_at_root) = self
            .synapses
            .get(&self.arbor.root.expect("arbor should be rooted"))
        {
            if n_synapses_at_root > &0 {
                for (treenode_id, ds) in synapse_distances.iter_mut() {
                    let distance = self
                        .distances_to_root
                        .get(treenode_id)
                        .expect("all tns have a distance");
                    if *distance < max_distance {
                        ds.push(*distance);
                    }
                }
            }
        }

        synapse_distances
    }

    pub fn synapse_distances(&mut self) -> &FastMap<NodeType, Vec<f64>> {
        if self.synapse_distances.is_none() {
            self.synapse_distances = Some(self._calculate_synapse_distances());
        }

        self.synapse_distances.as_ref().expect("just checked")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use arbor_parser::Response;
    use serde_json;
    use std::collections::HashSet;
    use std::fs::File;
    use std::io::Read;
    use std::path::PathBuf;
    use ArborResponse;

    const LAMBDA: f64 = 2000.0;

    fn read_file(fname: &str) -> String {
        let mut f = File::open(to_path(fname)).expect("file not found");

        let mut contents = String::new();
        f.read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        contents.to_owned()
    }

    fn to_path(fname: &str) -> PathBuf {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("resources/test");
        d.push(fname);
        d
    }

    fn make_arbor() -> ArborParser<u64, f64> {
        let s = read_file("3034133/compact-arbor.json");
        let response: ArborResponse = serde_json::from_str(&s).expect("fail");
        ArborParser::new(Response::Arbor(response)).expect("fail")
    }

    fn make_synapse_clustering() -> SynapseClustering<u64, f64> {
        let ap = make_arbor();
        SynapseClustering::new(ap, LAMBDA)
    }

    #[test]
    fn can_get_distances() {
        let mut sc = make_synapse_clustering();
        let dists = sc.synapse_distances().clone();
        let dist_keys: HashSet<u64> = dists.keys().cloned().collect();
        let treenodes: HashSet<u64> = sc.arbor.nodes().cloned().collect();

        assert_eq!(dist_keys, treenodes);

        let has_values = dists.values().fold(false, |acc, v| acc || !v.is_empty());
        assert_eq!(has_values, true)
    }
}
