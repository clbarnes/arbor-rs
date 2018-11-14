//extern crate approx;
//
//extern crate arbor;
//extern crate serde_json;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::string::ToString;

use arbor::utils::{FastMap, FastSet};
use arbor::{Arbor, ArborParser, SynapseClustering};
use serde_json::Value;
use std::fmt::Debug;
use std::hash::Hash;

const TEST_SKELETON: u64 = 3034133;
pub const LAMBDA: f64 = 2000.0;
pub const FRACTION: f64 = 0.9;
pub const TOLERANCE_ABS_NM: f64 = 1.0;

fn skeleton_root() -> PathBuf {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("resources/test/arbor-harness/data");
    d.push(TEST_SKELETON.to_string());
    d
}

fn to_path(relpath: &str) -> PathBuf {
    let mut p = skeleton_root();
    p.push(relpath);
    p
}

pub fn read_file(relpath: &str) -> String {
    let mut f = File::open(to_path(relpath)).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");
    contents.to_owned()
}

//pub fn deser_str_key<T: Clone>(relpath: &str) -> FastMap<u64, T> {
//    let contents = read_file(relpath);
//    let map: FastMap<String, T> = serde_json::from_str(&contents).expect("could not deserialise reference data");
//    map.iter().map(|(key, value)| (key.parse::<u64>(), value.clone())).collect()
//}

pub fn mk_arbor_parser() -> ArborParser<u64, f64> {
    let json = read_file("results/arbor_parser/from_compact-arbor.result.json");
    serde_json::from_str(&json).expect("couldn't deser")
}

pub fn mk_arbor() -> Arbor<u64> {
    mk_arbor_parser().arbor.clone()
}

pub fn mk_synapse_clustering() -> SynapseClustering<u64, f64> {
    SynapseClustering::new(mk_arbor_parser(), LAMBDA)
}

fn sort_vecs<T: PartialOrd + PartialEq + Clone + Debug>(
    test: &Vec<T>,
    reference: &Vec<T>,
) -> (Vec<T>, Vec<T>) {
    let mut v1 = test.clone();
    v1.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let mut v2 = reference.clone();
    v2.sort_by(|a, b| a.partial_cmp(b).unwrap());

    assert_eq!(v1.len(), v2.len());
    (v1, v2)
}

pub fn assert_vec_members<T: PartialOrd + PartialEq + Clone + Debug>(
    test: &Vec<T>,
    reference: &Vec<T>,
) {
    let (v1, v2) = sort_vecs(test, reference);
    assert_eq!(v1, v2);
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

pub fn str_id(s: String) -> u64 {
    s.parse().expect("couldn't parse ID")
}

pub fn val_id(v: Value) -> u64 {
    match v {
        Value::String(s) => str_id(s),
        Value::Number(n) => n.as_u64().expect("should be u64"),
        _ => panic!("Not a string or a number"),
    }
}

pub fn partitions_to_edges(partitions: Vec<Vec<u64>>) -> FastMap<u64, u64> {
    let mut edges: FastMap<u64, u64> = FastMap::default();
    for partition in partitions.iter() {
        for pair in partition.windows(2) {
            let was_present = edges.insert(pair[0].clone(), pair[1].clone());
            assert_eq!(was_present, None);
        }
    }
    edges
}

pub fn assert_equivalent_partitions(test: Vec<Vec<u64>>, reference: Vec<Vec<u64>>) {
    assert_eq!(partitions_to_edges(test), partitions_to_edges(reference));
}

pub fn assert_approx_map<T: Hash + Eq + Debug + Clone>(
    test: &FastMap<T, f64>,
    reference: &FastMap<T, f64>,
    tol: f64,
) {
    assert_keys(test, reference);
    for (key, test_val) in test.iter() {
        assert_abs_diff_eq!(test_val, &reference[key], epsilon = tol);
    }
}
