extern crate serde_json;

extern crate arbor;
extern crate serde;

use arbor::utils::{FastMap, FastSet, FlowCentrality, NodesDistanceTo};
use arbor::BranchAndEndNodes;

mod common;

use arbor::utils::Axon;
use arbor::Arbor;
use arbor::SynapseClustering;
use common::{
    assert_equivalent_partitions, assert_keys, assert_vec_members, mk_arbor, mk_arbor_parser,
    mk_synapse_clustering, partitions_to_edges, read_file, str_id, val_id, FRACTION,
};
use serde_json::Value;
use std::fmt::Debug;
use std::hash::Hash;

// todo: all of these will fail as they depend on nodes_distance_to

#[test]
#[ignore]
fn distance_map() {
    let mut syn_clus = mk_synapse_clustering();
    let test = syn_clus.synapse_distances().clone();

    let ref_json = read_file("results/synapse_clustering/distance_map.result.json");
    let reference: FastMap<u64, Vec<f64>> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");

    assert_keys(&test, &reference);
    for (key, test_val) in test.iter() {
        assert_vec_members(test_val, reference.get(key).expect("just checked keys"));
    }
}

fn dhm_to_clusters(dhm: FastMap<u64, usize>) -> FastMap<usize, FastSet<u64>> {
    let mut out = FastMap::default();

    for (k, v) in dhm.iter() {
        let entry = out.entry(*v).or_insert(FastSet::default());
        entry.insert(*k);
    }

    out
}

#[test]
#[ignore]
fn density_hill_map() {
    // todo: sensitive to partition ordering; clusters is a better test
    let test = mk_synapse_clustering().density_hill_map();
    let ref_json = read_file("results/synapse_clustering/density_hill_map.result.json");
    let reference: FastMap<u64, usize> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");

    assert_eq!(test, reference);
    assert_equiv_clusters(dhm_to_clusters(test), dhm_to_clusters(reference));
}

fn clusters_by_size(clus: FastMap<usize, FastSet<u64>>) -> FastMap<usize, Vec<FastSet<u64>>> {
    clus.values().fold(FastMap::default(), |mut accum, v| {
        let key = v.len();
        {
            let entry = accum.entry(key).or_insert(Vec::new());
            entry.push(v.clone());
        }
        accum
    })
}

/// Takes cluster_ID: Vec<NodeID> maps
fn assert_equiv_clusters(
    test: FastMap<usize, FastSet<u64>>,
    reference: FastMap<usize, FastSet<u64>>,
) {
    let mut test_lens: Vec<usize> = test.values().map(|v| v.len()).collect();
    test_lens.sort();
    let mut ref_lens: Vec<usize> = reference.values().map(|v| v.len()).collect();
    ref_lens.sort();
    assert_eq!(test_lens, ref_lens);

    // todo: this is hard and is going to fail anyway
}

#[test]
#[ignore]
fn clusters() {
    // todo: sensitive to partition ordering
    let mut syn_clus = mk_synapse_clustering();
    let dhm = syn_clus.density_hill_map();
    let test = syn_clus.clusters(&dhm);

    let ref_json = read_file("results/synapse_clustering/clusters.result.json");
    let ref_str: FastMap<usize, FastSet<String>> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");
    let reference: FastMap<usize, FastSet<u64>> = ref_str
        .iter()
        .map(|(k, v)| (*k, v.iter().cloned().map(str_id).collect()))
        .collect();

    assert_eq!(test, reference);
    assert_equiv_clusters(test, reference);
}

#[test]
#[ignore]
fn segregation_index() {
    let ap = mk_arbor_parser();
    let mut syn_clus = mk_synapse_clustering();
    let dhm = syn_clus.density_hill_map();
    let clusters = syn_clus.clusters(&dhm);

    let test = SynapseClustering::segregation_index(&clusters, &ap.outputs, &ap.inputs);

    let ref_json = read_file("results/synapse_clustering/segregation_index.result.json");
    let reference: f64 = serde_json::from_str(&ref_json).expect("couldn't deser ref data");

    assert_eq!(test, reference);
}

#[test]
#[ignore]
fn arbor_regions() {
    let ap = mk_arbor_parser();
    let test = SynapseClustering::find_axon(&ap, FRACTION, &ap.positions).expect("should find");

    let ref_json = read_file("results/synapse_clustering/find_axon.result.json");
    // todo: everything is a string in the JSON, which is hard to deserialize

    unimplemented!()
    //    let ref_val: Axon<Value> = serde_json::from_str(&ref_json).expect("couldn't deser ref data");
    //
    //    let reference = Axon {
    //        arbor: Arbor {
    //            edges: ref_val.arbor.edges.iter().map(|(k, v)| (val_id(k.clone()), val_id(v.clone()))).collect(),
    //            root: Some(val_id(ref_val.arbor.root.expect("has root"))),
    //        },
    //        fc_max_plateau: ref_val.fc_max_plateau.iter().cloned().map(val_id).collect(),
    //        fc_zeros: ref_val.fc_zeros.iter().cloned().map(val_id).collect(),
    //    };
    //
    //    assert_eq!(test, reference);
}