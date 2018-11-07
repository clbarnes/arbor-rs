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

fn bench_deser_arbor(b: &mut Bencher) {
    let s = read_file("3034133/compact-arbor.json");
    b.iter(|| {
        let response: ArborResponse = serde_json::from_str(&s).expect("fail");
        let _ap = response.to_arborparser().expect("fail");
    })
}

fn bench_deser_skeleton(b: &mut Bencher) {
    let s = read_file("3034133/compact-skeleton.json");
    b.iter(|| {
        let response: SkeletonResponse = serde_json::from_str(&s).expect("fail");
        let _ap = response.to_arborparser().expect("fail");
    })
}

benchmark_group!(parse, bench_deser_arbor, bench_deser_skeleton);
benchmark_main!(parse);
