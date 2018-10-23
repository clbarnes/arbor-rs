extern crate arbor;
#[macro_use]
extern crate bencher;
extern crate serde_json;

use std::vec::Vec;

use bencher::Bencher;

use arbor::*;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;

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

fn make_arbor() -> Arbor<u64> {
    let mut arbor = Arbor::default();

    let mut max_id: u64 = 1000;
    let branch_len = 1000;
    arbor.add_path((0..max_id).collect());

    for branch in (100..1000).step_by(100) {
        let next_max_id = max_id + branch_len;
        let mut path = vec![branch];
        path.extend(max_id..next_max_id);
        arbor.add_path(path);
        max_id = next_max_id;
    }

    arbor
}

fn read_arbor() -> Arbor<u64> {
    let s = read_file("3034133/compact-arbor.json");
    let response: ArborResponse = serde_json::from_str(&s).expect("fail");
    let ap = response.to_arborparser().expect("fail");
    ap.arbor.clone()
}

// benchmarks requiring cloning

fn bench_clone(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| arbor.clone())
}

fn bench_reroot(b: &mut Bencher) {
    let arbor = read_arbor();

    b.iter(|| {
        arbor.clone().reroot(3034449);
    });
}

// others

fn bench_branch_ends(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| arbor.find_branch_and_end_nodes())
}

fn bench_partitions(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| {
        let _v: Vec<Vec<_>> = arbor.partition().collect(); // do I need to assign this?
    })
}

fn bench_successors(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| arbor.all_successors())
}

fn bench_degrees(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| arbor.out_degrees())
}

fn bench_toposort(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| {
        let _v: Vec<_> = arbor.toposort().collect();
    })
}

fn bench_dfs(b: &mut Bencher) {
    let arbor = read_arbor();
    b.iter(|| {
        let _v: Vec<_> = arbor.dfs_from_root().collect();
    })
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

benchmark_group!(clone, bench_clone, bench_reroot);
benchmark_group!(
    noclone,
    bench_branch_ends,
    bench_partitions,
    bench_successors,
    bench_degrees,
    bench_toposort,
    bench_dfs,
);
benchmark_group!(parse, bench_deser_arbor, bench_deser_skeleton);
benchmark_main!(clone, noclone, parse);
