extern crate serde_json;

extern crate arbor;
extern crate serde;
#[macro_use]
extern crate approx;
extern crate num;

mod common;

use arbor::utils::FastMap;
use arbor::{ArborParser, ArborResponse, Response, SkeletonResponse};
use common::{mk_arbor_parser, read_file};

#[test]
fn deserialize_compact_arbor() {
    let response_str = read_file("compact-arbor.json");
    let response_obj: ArborResponse =
        serde_json::from_str(&response_str).expect("It didn't work :(");
    let parsed_ap = ArborParser::new(Response::Arbor(response_obj)).expect("response was invalid");

    let json = read_file("results/arbor_parser/from_compact-arbor.result.json");
    let expected_ap: ArborParser<u64, f64> = serde_json::from_str(&json).expect("couldn't deser");

    assert_eq!(parsed_ap, expected_ap);
}

#[test]
#[ignore]
fn deserialize_compact_skeleton() {
    // todo: fails because JS fails to recover same arbor too
    let response_str = read_file("compact-skeleton.json");
    let response_obj: SkeletonResponse =
        serde_json::from_str(&response_str).expect("It didn't work :(");
    let parsed_ap =
        ArborParser::new(Response::Skeleton(response_obj)).expect("response was invalid");

    let json = read_file("results/arbor_parser/from_compact-skeleton.result.json");
    let expected_ap: ArborParser<u64, f64> =
        serde_json::from_str(&json).expect("couldn't deser ref");

    assert_eq!(parsed_ap, expected_ap);
}

#[test]
fn create_synapse_map() {
    let test = mk_arbor_parser().create_synapse_map();

    let ref_json = read_file("results/arbor_parser/create_synapse_map.result.json");
    let reference: FastMap<u64, usize> =
        serde_json::from_str(&ref_json).expect("couldn't deser ref");

    assert_eq!(test, reference);
}
