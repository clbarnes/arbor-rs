use num::Float;
use std::fmt::Debug;
use std::hash::Hash;
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

    pub fn synapse_distances<'a>(&mut self) -> &'a FastMap<NodeType, Vec<f64>> {
        if let Some(ref d) = self.synapse_distances {
            return &d;
        }

        // treenode ID to list of distances to treenodes with synapses
        let mut synapse_distances: FastMap<NodeType, Vec<f64>> = FastMap::default();

        // branch node to list of upstream treenode IDs
        let mut seen_downstream_nodes: FastMap<NodeType, Vec<NodeType>> = FastMap::default();

        let max_distance = self.lambda * 3.0;

        for partition in self.arbor.partition() {
            let mut downstream_nodes: Vec<NodeType> = Vec::default();
            let mut partition_it = partition.iter();
            let mut prev_treenode_id = partition_it.next().expect("partitions should not be empty");

            for treenode_id in partition_it {
                downstream_nodes.push(*prev_treenode_id);
                let n_synapses = self.synapses.get(prev_treenode_id).unwrap_or(&0);
                let prev_distances = synapse_distances
                    .entry(*prev_treenode_id)
                    .or_insert(Vec::default());

                if n_synapses > &0 {
                    let d = self.distances_to_root[prev_treenode_id];
                    // todo
                }
            }
        }

        // todo

        &synapse_distances
    }
}
