use num::Float;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter;
use std::iter::repeat;
use std::usize;

use std::collections::VecDeque;
use std::iter::repeat_with;
use std::iter::Chain;
use std::iter::Flatten;
use std::ops::RangeFrom;
use utils::ArborRegions;
use utils::Axon;
use utils::FastMap;
use utils::FastSet;
use utils::FlowCentrality;
use utils::Location;
use Arbor;
use ArborParser;

fn entropy(p: f64) -> Option<f64> {
    // todo: make generic
    if p <= 0.0 || p >= 1.0 {
        return None;
    }

    let p_i = 1.0 - p;

    Some(-(p * p.ln() + p_i * p_i.ln()))
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct SynapseClustering<NodeType: Hash + Copy + Eq + Debug + Ord, F: Float> {
    arbor: Arbor<NodeType>,
    synapses: FastMap<NodeType, usize>,
    lambda: F,
    #[serde(rename = "distancesToRoot")]
    distances_to_root: FastMap<NodeType, F>,
    #[serde(rename = "Ds")]
    synapse_distances: Option<FastMap<NodeType, Vec<F>>>,
}

/// The synapse density of a given node is a function of how close it is to arbor synapses
fn synapse_density<NodeType: Hash + Copy + Debug + Eq>(
    synapse_distances: &FastMap<NodeType, Vec<f64>>,
    lambda: &f64,
) -> FastMap<NodeType, f64> {
    let lambda_sq = lambda.powi(2);

    synapse_distances
        .iter()
        .map(|(treenode_id, distances)| {
            let val = distances.iter().fold(0.0, |sum, d| {
                let exponent = -(d.powi(2) / lambda_sq);
                sum + exponent.exp()
            });
            (*treenode_id, val)
        })
        .collect()
}

fn chain_values<T: Hash + Eq, U: Copy>(map: &FastMap<T, Vec<U>>) -> Vec<U> {
    map.values().cloned().flatten().collect()
}

fn chain_values_except<T: Hash + Eq, U: Copy + PartialEq>(
    map: &FastMap<T, Vec<U>>,
    except: &T,
) -> Vec<U> {
    map.iter()
        .filter(|(k, v)| k != &except)
        .map(|(k, v)| v)
        .cloned()
        .flatten()
        .collect()
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

    fn edge_length(&self, node1: &NodeType, node2: &NodeType) -> Option<f64> {
        if node1 == node2 {
            return Some(0.0);
        }

        // check adjacent
        let adj = match self.arbor.get_parent(*node1) {
            Some(n) => n == node2,
            None => false,
        } || match self.arbor.get_parent(*node2) {
            Some(n) => n == node1,
            None => false,
        };

        match adj {
            true => Some((self.distances_to_root[node1] - self.distances_to_root[node2]).abs()),
            false => None,
        }
    }

    /// Traverse partitions towards the root, populating each one with a dict of {child_id: [downstream_dists]}.
    /// Then do a DFS, updating the dict with {parent_id: [upstream_dists]},
    /// taking only distances from the parent's keys which are not this child.
    /// Then iterate over all nodes once more, collapsing the values of the dicts into a single array.
    ///
    /// Gives correct results for toy data but doesn't match JS
    fn _calculate_synapse_distances2(&self) -> FastMap<NodeType, Vec<f64>> {
        let mut ds_by_neighbour: FastMap<NodeType, FastMap<NodeType, Vec<f64>>> =
            FastMap::default();

        // populate ds_by_neighbour with {self_id: [0]*n_synapses}
        for node in self.arbor.nodes() {
            let mut to_insert = FastMap::default();
            if let Some(n_synapses) = self.synapses.get(node) {
                if n_synapses > &0 {
                    let dists = repeat(0.0).take(*n_synapses).collect();
                    to_insert.insert(*node, dists);
                }
            }
            ds_by_neighbour.insert(*node, to_insert);
        }

        // root-containing first
        let partitions: Vec<Vec<NodeType>> = self.arbor.partition().collect();

        let max_distance = self.lambda * 3.0;

        // partitions which stop at the first unseen branch (or root)
        for partition in partitions.iter().rev() {
            // end -> root
            let mut node_iter = partition.iter();
            let mut distal = node_iter.next().unwrap();

            for proximal in node_iter {
                let dist_between = self.edge_length(distal, proximal).unwrap();

                // add edge length to all downstream partner's distances, flatten
                let ds: Vec<f64> = ds_by_neighbour
                    .get(distal)
                    .unwrap()
                    .values()
                    .flatten()
                    .map(|i| i + dist_between)
                    .filter(|i| i < &max_distance)
                    .collect();

                ds_by_neighbour
                    .get_mut(proximal)
                    .unwrap()
                    .insert(*distal, ds);

                distal = proximal;
            }
        }

        // partitions which stop at the first seen branch (or root)
        for partition in partitions.iter() {
            // root -> end
            let mut node_iter = partition.iter().rev();
            let mut proximal = node_iter.next().unwrap();

            for distal in node_iter {
                let dist_between = self.edge_length(distal, proximal).unwrap();

                // add edge length to all upstream partner's distances
                // other than those coming from this node, flatten
                let mut ds: Vec<f64> = ds_by_neighbour
                    .get(proximal)
                    .unwrap()
                    .iter()
                    .filter_map(|(k, v)| match k == distal {
                        true => None,
                        false => Some(v),
                    })
                    .flatten()
                    .map(|i| i + dist_between)
                    .filter(|i| i < &max_distance)
                    .collect();

                ds_by_neighbour
                    .get_mut(distal)
                    .unwrap()
                    .insert(*proximal, ds);

                proximal = distal;
            }
        }

        ds_by_neighbour
            .into_iter()
            .map(|(k, vals)| (k, vals.values().flatten().cloned().collect()))
            .collect()
    }

    /// as per reference implementation, but this is wrong
    fn _calculate_synapse_distances(&self) -> FastMap<NodeType, Vec<f64>> {
        // treenode ID to list of distances to treenodes with synapses
        // Ds
        let mut synapse_distances: FastMap<NodeType, Vec<f64>> = FastMap::default();

        // branch node to list of upstream treenode IDs
        // entries get removed once the branch treenode has been visited as part of a partition
        // where it is not the last treenode of the partition
        let mut seen_downstream_nodes: FastMap<NodeType, Vec<NodeType>> = FastMap::default();

        // don't include synapses further away than this
        let max_distance = self.lambda * 3.0;

        for partition in self.arbor.partition_sorted().iter() {
            // treenodes downstream of the current treenode,
            // in the current partition and in all partitions merging into a seen node
            let mut downstream_nodes: Vec<NodeType> = Vec::default();

            let mut partition_it = partition.iter();
            let mut prev_treenode_id = partition_it.next().expect("partitions should not be empty");

            for treenode_id in partition_it {
                downstream_nodes.push(*prev_treenode_id);

                let n_synapses = self.synapses.get(prev_treenode_id).unwrap_or(&0);

                synapse_distances
                    .entry(*prev_treenode_id)
                    .or_insert(Vec::default());

                if n_synapses > &0 {
                    let d = self.distances_to_root[prev_treenode_id];

                    for child_id in downstream_nodes.iter() {
                        let distance_child_to_synapse = self.distances_to_root[child_id] - d;

                        if distance_child_to_synapse <= max_distance {
                            let ds = synapse_distances.get_mut(child_id).expect("should exist?");

                            for v in repeat(distance_child_to_synapse).take(*n_synapses) {
                                ds.push(v)
                            }
                        }
                    }
                }

                // if treenode_id is a branch, append all its children to downstream nodes
                // if it is a branch, we have already seen it: therefore it is in seen_downstream_nodes

                let distance_to_root = self.distances_to_root[treenode_id];
                let distance_prev_to_current =
                    self.distances_to_root[prev_treenode_id] - distance_to_root;

                // remove seen_downstream so it's not processed again
                if let Some(seen) = seen_downstream_nodes.remove(treenode_id) {
                    let current_ds = synapse_distances.get(treenode_id).unwrap().clone();

                    for child_id in downstream_nodes.iter().cloned() {
                        let distance = self.distances_to_root.get(&child_id).expect("should exist")
                            - distance_to_root;

                        if distance <= max_distance {
                            let mut child_ds =
                                synapse_distances.get_mut(&child_id).expect("should exist");

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
                        let distance = self.distances_to_root.get(&child_id).expect("should exist")
                            - distance_to_root;

                        if distance <= max_distance {
                            let mut child_ds =
                                synapse_distances.get_mut(&child_id).expect("should exist");

                            for prev_d in prev_ds.iter().cloned() {
                                child_ds.push(prev_d + distance);
                            }
                        }

                        // update list of children
                        downstream_nodes.push(child_id);
                    }
                }

                // assign synapse distances to the current node

                let mut translated_prev_ds: Vec<f64> = Vec::default();

                for prev_d in synapse_distances
                    .get(prev_treenode_id)
                    .expect("assigned earlier")
                    .iter()
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

                // reset for next iteration
                prev_treenode_id = treenode_id;
            }

            // finished traversing the partition
            let last_treenode_id = partition.last().expect("partitions should not be empty");
            seen_downstream_nodes.insert(*last_treenode_id, downstream_nodes);
        }

        // update the last node: the root
        if let Some(n_synapses_at_root) = self
            .synapses
            .get(&self.arbor.root.expect("arbor should be rooted"))
        {
            if n_synapses_at_root > &0 {
                for (treenode_id, ds) in synapse_distances.iter_mut() {
                    let distance = self.distances_to_root[treenode_id];
                    if distance < max_distance {
                        for d in repeat(distance).take(*n_synapses_at_root) {
                            ds.push(d);
                        }
                    }
                }
            }
        }

        synapse_distances
    }

    /// Compute a map from treenodes to an array of cable distances to all arbor synapses
    pub fn synapse_distances(&mut self) -> &FastMap<NodeType, Vec<f64>> {
        if self.synapse_distances.is_none() {
            self.synapse_distances = Some(self._calculate_synapse_distances2());
        }

        self.synapse_distances.as_ref().expect("just checked")
    }

    pub fn density_hill_map(&mut self) -> FastMap<NodeType, usize> {
        self._density_hill_map_classic()
    }

    /// Return a map of treenodes to cluster index, based on the distance map
    fn _density_hill_map_classic(&mut self) -> FastMap<NodeType, usize> {
        type HillId = usize;

        let mut density_hill_map: FastMap<NodeType, HillId> = FastMap::default();
        let lambda = self.lambda;
        let density: FastMap<NodeType, f64> = synapse_density(self.synapse_distances(), &lambda);

        let mut hill_ids: RangeFrom<HillId> = 0..;

        let mut all_neighbours = self.arbor.all_neighbours();
        let edges = &self.arbor.edges;
        let root = &self.arbor.root.expect("unrooted arbor");

        density_hill_map.insert(*root, hill_ids.next().unwrap());

        let mut partitions = self.arbor.partition_sorted();
        partitions.reverse();

        for mut partition in partitions {
            let first = partition.pop().expect("len >=1");

            let mut density_hill_index =
                *density_hill_map.get(&first).expect("first must be visited");
            if let Some(idx) = density_hill_map.get(partition.last().expect("len >=2")) {
                density_hill_index = *idx;
            }

            while !partition.is_empty() {
                let treenode_id = partition.pop().expect("just checked");
                density_hill_map.insert(treenode_id, density_hill_index);

                let neighbors = all_neighbours
                    .get_mut(&treenode_id)
                    .expect("all nodes have neighbours");
                if neighbors.len() <= 1 {
                    continue;
                }
                neighbors.sort_unstable();

                let self_density = density.get(&treenode_id).expect("everything has density");

                let (n_over_zero, delta_density) = neighbors.iter().fold(
                    (0 as usize, Vec::default()),
                    |(mut n, mut deltas), neighbor| {
                        let d = density.get(neighbor).expect("defined for all") - self_density;
                        if d > 0.0 {
                            n += 1;
                        }
                        deltas.push((*neighbor, d));
                        (n, deltas)
                    },
                );

                if n_over_zero <= 1 {
                    continue;
                }

                let parent = edges.get(&treenode_id).expect("can't be root");
                for (neighbor, dens_delta) in delta_density.iter() {
                    if parent == neighbor || dens_delta < &0.0 {
                        continue;
                    }
                    density_hill_map.insert(*neighbor, hill_ids.next().unwrap());
                }

                let distance_to_current = self
                    .distances_to_root
                    .get(&treenode_id)
                    .expect("all have distances");
                let mut steepest_id: Option<NodeType> = None;
                let mut max = f64::min_value();

                let get_incline = |neighbor: &NodeType, dens: &f64| -> f64 {
                    dens / (self.distances_to_root[neighbor] - distance_to_current).abs()
                };

                let mut delta_density_iter = delta_density.iter();
                let (neighbor, dens) = delta_density_iter.next().expect(">=2 elements");
                let mut steepest_neighbor = neighbor;
                let mut steepest_incline = get_incline(neighbor, dens);
                let mut negative_dens = Vec::with_capacity(delta_density.len());

                for (neighbor, dens) in delta_density_iter {
                    if dens < &0.0 {
                        negative_dens.push(*neighbor);
                    }
                    let incline = get_incline(neighbor, dens);
                    if incline > steepest_incline {
                        steepest_incline = incline;
                        steepest_neighbor = neighbor;
                    }
                }

                let steepest_hill = density_hill_map[steepest_neighbor];
                density_hill_map.insert(treenode_id, steepest_hill);

                for neighbor in negative_dens {
                    density_hill_map.insert(neighbor, steepest_hill);
                }

                density_hill_index =
                    density_hill_map[partition.last().expect("already continued if an end node")];
            }
        }

        density_hill_map
    }

    /// Return a map of cluster ID to treenode
    pub fn clusters(
        &self,
        density_hill_map: &FastMap<NodeType, usize>,
    ) -> FastMap<usize, FastSet<NodeType>> {
        let mut clusters = FastMap::default();

        for (node, cluster) in density_hill_map.iter() {
            let entry = clusters.entry(*cluster).or_insert(FastSet::default());
            entry.insert(*node);
        }

        clusters
    }

    /// Return a map of cluster ID to number of treenodes in the cluster
    fn cluster_sizes(&self, density_hill_map: &FastMap<NodeType, usize>) -> FastMap<usize, usize> {
        let mut sizes = FastMap::default();

        for (node, cluster) in density_hill_map.iter() {
            let entry = sizes.entry(*cluster).or_insert(0);
            *entry += 1;
        }

        sizes
    }

    /// Compute the sum of cluster entropies,
    /// measured as a deviation from uniformity relative to the arbor as a whole.
    pub fn segregation_index(
        clusters: &FastMap<usize, FastSet<NodeType>>,
        outputs: &FastMap<NodeType, usize>,
        inputs: &FastMap<NodeType, usize>,
    ) -> f64 {
        struct ClusterInfo {
            pub id: usize,
            pub n_inputs: usize,
            pub n_outputs: usize,
            pub n_synapses: usize,
            pub entropy: f64,
        }

        let cluster_infos =
            clusters
                .iter()
                .fold(Vec::default(), |mut accum, (cluster_id, nodes)| {
                    let mut init = ClusterInfo {
                        id: *cluster_id,
                        n_inputs: 0,
                        n_outputs: 0,
                        n_synapses: 0,
                        entropy: 0.0,
                    };

                    let mut cluster_info = nodes.iter().fold(init, |mut c_info, node| {
                        c_info.n_outputs += outputs.get(node).unwrap_or(&0);
                        c_info.n_inputs += inputs.get(node).unwrap_or(&0);
                        c_info
                    });

                    cluster_info.n_synapses = cluster_info.n_inputs + cluster_info.n_outputs;

                    if cluster_info.n_synapses == 0 {
                        return accum;
                    }

                    let p = cluster_info.n_inputs as f64 / cluster_info.n_synapses as f64;
                    cluster_info.entropy = entropy(p).unwrap_or(0.0);

                    accum.push(cluster_info);
                    accum
                });

        let mut n_synapses: usize = 0;
        let mut n_inputs: usize = 0;
        let mut S = 0.0;

        for cluster_info in cluster_infos {
            n_synapses += cluster_info.n_synapses;
            n_inputs += cluster_info.n_inputs;
            S += cluster_info.n_synapses as f64 * cluster_info.entropy;
        }

        S /= n_synapses as f64;

        if S == 0.0 {
            return 1.0;
        }

        let p = n_inputs as f64 / n_synapses as f64;

        if let Some(S_norm) = entropy(p) {
            1.0 - S / S_norm
        } else {
            1.0
        }
    }

    /// Find the treenodes in the arbor which have a centrifugal flow centrality of zero,
    /// those belonging to the max-FC plateau,
    /// and those with FC above `fraction` of the max FC
    fn find_arbor_regions(
        arbor: &Arbor<NodeType>,
        flow_centralities: &FastMap<NodeType, FlowCentrality>,
        fraction: f64,
    ) -> Option<ArborRegions<NodeType>> {
        let max = flow_centralities
            .values()
            .map(|fc| fc.centrifugal)
            .max()
            .unwrap_or(0);
        if max == 0 {
            return None;
        }

        let threshold = fraction * (max as f64);
        let mut regions = ArborRegions::default();

        for node in arbor.nodes() {
            let fc = flow_centralities
                .get(node)
                .expect("flow centralities does not contain all nodes");
            if fc.centrifugal as f64 > threshold {
                regions.above.insert(*node);
                if fc.centrifugal == max {
                    regions.plateau.insert(*node);
                }
            } else if fc.sum == 0 {
                regions.zeros.insert(*node);
            }
        }

        Some(regions)
    }

    pub fn find_axon(
        arbor_parser: &ArborParser<NodeType, f64>,
        fraction: f64,
        positions: &FastMap<NodeType, Location<f64>>,
    ) -> Option<Axon<NodeType>> {
        let flow_centralities = arbor_parser.flow_centrality()?;
        let regions = Self::find_arbor_regions(&arbor_parser.arbor, &flow_centralities, fraction)?;
        let output_treenodes =
            arbor_parser
                .outputs
                .iter()
                .fold(FastSet::default(), |mut accum, (id, n)| {
                    if n >= &1 {
                        accum.insert(*id);
                    }
                    accum
                });
        let cut = Self::find_axon_cut(
            &arbor_parser.arbor,
            &output_treenodes,
            &regions.above,
            &positions,
        )?;

        Some(Axon::new(arbor_parser.arbor.sub_arbor(cut), regions))
    }

    /// Find a node ID at which is its optimal to cut an arbor so that the downstream
    /// sub-arbor is the axon and the rest is the dendrites.
    ///
    /// The heuristic is fidgety: finds the lowest-order node (relative to root)
    /// that contains an output synapse or is a branch where more than one of the downstream branches
    /// contains output synapses and is on the lower 50% of the cable for the flow centrality plateau
    /// (the "above" array).
    ///
    /// arbor: an Arbor instance
    /// outputs: map of node ID vs non-undefined to signal there are one or more output synapses at the node. There MUST be at least one output.
    /// above: array of nodes with e.g. maximum centrifugal flow centrality.
    /// positions: the map of node ID vs object with a distanceTo function like THREE.Vector3.
    ///
    /// The returned node is present in 'above'.
    fn find_axon_cut(
        arbor: &Arbor<NodeType>,
        output_treenodes: &FastSet<NodeType>,
        above: &FastSet<NodeType>,
        positions: &FastMap<NodeType, Location<f64>>,
    ) -> Option<NodeType> {
        let mut above_vec: Vec<NodeType> = above.iter().cloned().collect();

        if above.len() < 2 {
            return above_vec.first().cloned();
        }

        let orders = arbor.nodes_order_from_root().distances;
        let successors = arbor.all_successors();

        // nearest to furthest
        above_vec.sort_by_key(|n| orders.get(n).expect("must exist"));
        let furthest_from_root = *above_vec.last().expect(">2");
        let closest_to_root = *above_vec.first().expect(">2");

        let node = closest_to_root;
        let mut open = VecDeque::default();
        open.push_back(node);
        let mut max: f64 = 0.0;

        let mut distances = FastMap::default();
        distances.insert(node, 0.0);

        while !open.is_empty() {
            let parent = open.pop_front().expect("just checked non-empty");
            let d = *distances.get(&parent).expect("everything has a distance");
            let p = positions
                .get(&parent)
                .expect("everything has a location")
                .clone();

            for child in successors.get(&parent).expect("everything has succ").iter() {
                if !above.contains(child) {
                    continue;
                }
                let dc = d + p.distance_to(positions.get(child).expect("everything has"));
                distances.insert(*child, dc);
                if dc > max {
                    max = dc;
                }
                open.push_back(*child);
            }
        }

        let threshold = max / 2.0;
        let beyond: Vec<NodeType> = above_vec
            .iter()
            .filter(|n| match distances.get(n) {
                Some(d) => d > &threshold,
                None => false,
            })
            .cloned()
            .collect();

        let branches_ends = arbor.find_branch_and_end_nodes();
        let mut lowest = None;
        let mut lowest_order = usize::MAX;

        for node in above_vec {
            match distances.get(&node) {
                Some(d) => {
                    if d <= &threshold {
                        continue;
                    }
                }
                None => continue,
            }
            let order = orders.get(&node).expect("everything has order");

            if output_treenodes.contains(&node) {
                if order < &lowest_order {
                    lowest = Some(node);
                    lowest_order = *order;
                }
            } else if branches_ends.branches.contains_key(&node) {
                if order >= &lowest_order
                    || (match arbor.get_parent(node) {
                        Some(n) => !above.contains(n),
                        None => false,
                    }) {
                    continue;
                }

                let mut count: usize = 0;
                for child in successors.get(&node).expect("has successors").iter() {
                    // todo: this does arbor.all_successors() every loop - slow
                    if above.contains(child)
                        || subarbor_has_outputs(&arbor, &child, &output_treenodes)
                    {
                        if count >= 1 {
                            lowest = Some(node);
                            lowest_order = *order;
                            break;
                        }
                        count += 1;
                    }
                }
            }
        }

        lowest.or(Some(furthest_from_root))
    }
}

fn subarbor_has_outputs<NodeType: Hash + Copy + Eq + Debug + Ord>(
    arbor: &Arbor<NodeType>,
    root: &NodeType,
    output_nodes: &FastSet<NodeType>,
) -> bool {
    for (distal, _proximal) in arbor.dfs(*root) {
        if output_nodes.contains(&distal) {
            return true;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use approx;
    use arbor_parser::Response;
    use serde_json;
    use std::collections::HashSet;
    use std::fs::File;
    use std::io::Read;
    use std::path::PathBuf;
    use ArborResponse;

    const LAMBDA: f64 = 2000.0;
    const FRACTION: f64 = 0.9;

    //            1
    //            |
    //            2
    //            |
    // 13<-(101)<-3
    //            | \
    //            4  6<-(103)<-16
    //            |   \
    // 15<-(102)<-5    7<-(104)<-17

    fn small_arbor() -> Arbor<u64> {
        let edges: Vec<u64> = vec![5, 4, 4, 3, 3, 2, 2, 1, 7, 6, 6, 3];
        let mut arbor = Arbor::default();
        arbor.add_edges(&edges).expect("fail");
        arbor
    }

    fn small_arborparser() -> ArborParser<u64, f64> {
        let mut locations: FastMap<u64, Location<f64>> = FastMap::default();
        for idx in 1..8 {
            locations.insert(
                idx as u64,
                Location {
                    x: idx as f64,
                    y: 0.0,
                    z: 0.0,
                },
            );
        }
        let inputs: FastMap<u64, usize> = vec![(6, 1), (7, 1)].into_iter().collect();
        let outputs: FastMap<u64, usize> = vec![(3, 1), (5, 1)].into_iter().collect();

        ArborParser {
            arbor: small_arbor(),
            positions: locations,
            inputs,
            outputs,
        }
    }

    fn small_synapse_clustering() -> SynapseClustering<u64, f64> {
        let ap = small_arborparser();
        SynapseClustering::new(ap, 2.01)
    }

    fn sort_vecs<T: PartialOrd + PartialEq + Clone + Debug>(
        test: &Vec<T>,
        reference: &Vec<T>,
    ) -> (Vec<T>, Vec<T>) {
        let cmp = |a: &T, b: &T| a.partial_cmp(b).unwrap();

        let mut v1 = test.clone();
        v1.sort_by(cmp);

        let mut v2 = reference.clone();
        v2.sort_by(cmp);

        assert_eq!(v1.len(), v2.len());
        (v1, v2)
    }

    pub fn assert_vec_members_approx(test: &Vec<f64>, reference: &Vec<f64>, tol: f64) {
        let (v1, v2) = sort_vecs(test, reference);

        for (test_val, ref_val) in v1.iter().zip(v2.iter()) {
            assert_abs_diff_eq!(test_val, ref_val, epsilon = tol);
        }
    }

    pub fn assert_keys<T: Hash + Eq + Debug + Clone, U>(
        test: &FastMap<T, U>,
        reference: &FastMap<T, U>,
    ) {
        let m1: FastSet<T> = test.keys().cloned().collect();
        let m2: FastSet<T> = reference.keys().cloned().collect();

        assert_eq!(m1, m2);
    }

    #[test]
    fn distances() {
        let mut sc = small_synapse_clustering();
        let mut test = sc.synapse_distances().clone();

        for v in test.values_mut() {
            v.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap())
        }

        println!("test: {:?}", test);

        let reference: FastMap<u64, Vec<f64>> = vec![
            (1, vec![2.0, 4.0, 5.0, 6.0]),
            (2, vec![1.0, 3.0, 4.0, 5.0]),
            (3, vec![0.0, 2.0, 3.0, 4.0]),
            (4, vec![1.0, 1.0, 4.0, 5.0]),
            (5, vec![0.0, 2.0, 5.0, 6.0]),
            (6, vec![0.0, 1.0, 3.0, 5.0]),
            (7, vec![0.0, 1.0, 4.0, 6.0]),
        ]
        .into_iter()
        .collect();

        println!("ref: {:?}", reference);

        assert_keys(&test, &reference);
        for (key, test_val) in test.iter() {
            assert_vec_members_approx(test_val, &reference[key], 0.1)
        }
    }

    #[test]
    #[ignore]
    fn density_hill_map() {
        let mut sc = small_synapse_clustering();
        let test = sc.density_hill_map();

        println!("test: {:?}", test);

        // skips an ID?!
        let reference: FastMap<u64, usize> =
            vec![(1, 0), (2, 4), (3, 4), (4, 3), (5, 1), (6, 4), (7, 4)]
                .into_iter()
                .collect();

        println!(" ref: {:?}", reference);

        assert_eq!(test, reference);
    }

    fn entropy(p: f64) -> Option<f64> {
        // todo: make generic
        if p <= 0.0 || p >= 1.0 {
            return None;
        }

        let p_i = 1.0 - p;

        Some(-(p * p.ln() + p_i * p_i.ln()))
    }

    #[test]
    fn test_entropy() {
        let in_out = vec![(0.1, 0.3251), (0.2, 0.5004), (0.99, 0.0560), (0.5, 0.6931)];

        for (in_val, out) in in_out.iter() {
            let test = entropy(*in_val).unwrap();
            assert_abs_diff_eq!(test, out, epsilon = 0.001)
        }
    }

    #[test]
    fn segregation_index() {
        let ap = small_arborparser();
        let mut sc = small_synapse_clustering();

        // js result
        let clusters: FastMap<usize, FastSet<u64>> = vec![
            (0, vec![1].into_iter().collect()),
            (1, vec![5].into_iter().collect()),
            (3, vec![4].into_iter().collect()),
            (4, vec![2, 3, 6, 7].into_iter().collect()),
        ]
        .into_iter()
        .collect();

        let test = SynapseClustering::segregation_index(&clusters, &ap.outputs, &ap.inputs);
        println!("test: {:?}", test);
        let reference = 0.311278;
        println!("reference: {:?}", reference);

        assert_abs_diff_eq!(test, reference, epsilon = 0.001);
    }

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

    fn make_arborparser() -> ArborParser<u64, f64> {
        let s = read_file("3034133/compact-arbor.json");
        let response: ArborResponse = serde_json::from_str(&s).expect("fail");
        ArborParser::new(Response::Arbor(response)).expect("fail")
    }

    fn make_synapse_clustering() -> SynapseClustering<u64, f64> {
        let ap = make_arborparser();
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

    #[test]
    fn can_get_density_hill_map() {
        let mut sc = make_synapse_clustering();
        let dhm = sc.density_hill_map().clone();
        let dhm_keys: HashSet<u64> = dhm.keys().cloned().collect();
        let treenodes: HashSet<u64> = sc.arbor.nodes().cloned().collect();

        assert_eq!(dhm_keys, treenodes);

        let values: FastSet<usize> = dhm.values().cloned().collect();

        assert_eq!(values.len() >= 2, true)
    }

    #[test]
    fn can_segregation_index() {
        let ap = make_arborparser();
        let outputs = ap.outputs.clone();
        let inputs = ap.inputs.clone();

        let mut sc = SynapseClustering::new(ap, LAMBDA);

        let dhm = sc.density_hill_map();
        let clusters = sc.clusters(&dhm);

        SynapseClustering::segregation_index(&clusters, &outputs, &inputs);
    }

    #[test]
    fn can_find_axon() {
        let ap = make_arborparser();
        let locations = ap.positions.clone();
        SynapseClustering::find_axon(&ap, FRACTION, &locations);
    }
}
