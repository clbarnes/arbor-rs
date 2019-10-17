extern crate serde_json;

extern crate arbor;
extern crate serde;
#[macro_use]
extern crate approx;

use arbor::utils::{FastMap, FastSet, FlowCentrality, NodesDistanceTo};
use arbor::BranchAndEndNodes;

mod common;

use common::{
    assert_approx_map, assert_equivalent_partitions, assert_keys, assert_vec_members, mk_arbor,
    mk_arbor_parser, partitions_to_edges, read_file, str_id, val_id, TOLERANCE_ABS_NM,
};
use serde_json::Value;
use std::fmt::Debug;
use std::hash::Hash;

#[test]
fn all_successors() {
    let test = mk_arbor().all_successors();

    let ref_json: String = read_file("results/arbor/all_successors.result.json");
    let reference_strs: FastMap<u64, Vec<String>> =
        serde_json::from_str(&ref_json).expect("couldn't deser reference data");
    let reference: FastMap<u64, Vec<u64>> = reference_strs
        .iter()
        .map(|(k, v)| (*k, v.iter().cloned().map(str_id).collect()))
        .collect();

    assert_keys(&test, &reference);
    for (key, test_val) in test.iter() {
        let reference_val = reference.get(key).expect("have same keys");
        assert_vec_members(test_val, reference_val);
    }
}

#[test]
fn children() {
    let test: Vec<u64> = mk_arbor().children().cloned().collect();
    let ref_json = read_file("results/arbor/children_array.result.json");
    let ref_strs: Vec<String> =
        serde_json::from_str(&ref_json).expect("couldn't deser reference data");
    let reference: Vec<u64> = ref_strs.iter().cloned().map(str_id).collect();

    assert_vec_members(&test, &reference);
}

#[test]
fn find_branch_and_end_nodes() {
    let test: BranchAndEndNodes<u64> = mk_arbor().find_branch_and_end_nodes();

    let ref_json = read_file("results/arbor/find_branch_and_end_nodes.result.json");
    let ref_strs: BranchAndEndNodes<String> =
        serde_json::from_str(&ref_json).expect("couldn't deser reference data");

    let reference = BranchAndEndNodes {
        branches: ref_strs
            .branches
            .iter()
            .map(|(k, f)| (str_id(k.clone()), f.clone()))
            .collect(),
        ends: ref_strs.ends.iter().cloned().map(str_id).collect(),
        n_branches: ref_strs.n_branches,
    };

    assert_eq!(test, reference);
}

#[test]
fn flow_centrality() {
    let ap = mk_arbor_parser();
    let test: FastMap<u64, FlowCentrality> = ap
        .arbor
        .flow_centrality(&ap.outputs, &ap.inputs)
        .expect("FC failed");

    let ref_json = read_file("results/arbor/flow_centrality.result.json");
    let reference: FastMap<u64, FlowCentrality> =
        serde_json::from_str(&ref_json).expect("couldn't deser reference data");

    assert_eq!(test, reference);
}

#[test]
//#[ignore]
fn nodes_distance_to() {
    let ap = mk_arbor_parser();
    let test: NodesDistanceTo<u64, f64> = ap.arbor.nodes_distance_to_root(&ap.positions);

    let ref_json = read_file("results/arbor/nodes_distance_to.result.json");
    let reference: NodesDistanceTo<u64, f64> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");

    assert_eq!(test.max, reference.max);
    assert_approx_map(&test.distances, &reference.distances, TOLERANCE_ABS_NM);
}

#[test]
fn nodes_order_from() {
    let ap = mk_arbor_parser();
    let test: NodesDistanceTo<u64, usize> = ap.arbor.nodes_order_from_root();

    let ref_json = read_file("results/arbor/nodes_order_from.result.json");
    let reference: FastMap<u64, usize> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");

    assert_eq!(test.distances, reference);
}

#[test]
fn partition() {
    let arbor = mk_arbor();
    let test: Vec<Vec<u64>> = arbor.partition().collect();

    let ref_json = read_file("results/arbor/partition.result.json");
    let ref_str: Vec<Vec<Value>> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");
    let reference: Vec<Vec<u64>> = ref_str
        .iter()
        .cloned()
        .map(|v| v.iter().cloned().map(val_id).collect())
        .collect();

    assert_equivalent_partitions(test, reference);

    // todo: check order?
}

#[test]
fn partition_sorted() {
    let arbor = mk_arbor();
    let test: Vec<Vec<u64>> = arbor.partition().collect();

    let ref_json = read_file("results/arbor/partition_sorted.result.json");
    let ref_str: Vec<Vec<Value>> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref data");
    let reference: Vec<Vec<u64>> = ref_str
        .iter()
        .cloned()
        .map(|v| v.iter().cloned().map(val_id).collect())
        .collect();

    assert_equivalent_partitions(test, reference);

    // todo: check order?
}
