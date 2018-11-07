extern crate arbor;
#[macro_use]
extern crate bencher;
extern crate serde_json;

use std::vec::Vec;

use bencher::Bencher;

use arbor::*;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

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

fn make_synclus() -> SynapseClustering<u64, f64> {
    let s = read_file("3034133/compact-arbor.json");
    let response: ArborResponse = serde_json::from_str(&s).expect("fail");
    SynapseClustering::new(response.to_arborparser().expect("fail"), LAMBDA)
}

// benchmarks requiring cloning

fn bench_clone(b: &mut Bencher) {
    let synclus = make_synclus();
    b.iter(|| synclus.clone())
}

fn bench_synapse_distances(b: &mut Bencher) {
    let synclus = make_synclus();
    b.iter(|| {
        let mut sc = synclus.clone();
        sc.synapse_distances();
    })
}

fn bench_synapse_distances_twice(b: &mut Bencher) {
    let synclus = make_synclus();

    b.iter(|| {
        let mut sc = synclus.clone();
        sc.synapse_distances();
        sc.synapse_distances();
    })
}

benchmark_group!(
    synapse_clustering,
    bench_clone,
    bench_synapse_distances,
    bench_synapse_distances_twice
);
benchmark_main!(synapse_clustering);
