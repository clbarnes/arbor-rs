extern crate arbor;
#[macro_use]
extern crate bencher;

use std::vec::Vec;

use bencher::Bencher;

use arbor::*;

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

// benchmarks requiring cloning

fn bench_clone(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| arbor.clone())
}

fn bench_reroot(b: &mut Bencher) {
    let arbor = make_arbor();

    b.iter(|| {
        arbor.clone().reroot(500);
    });
}

// others

fn bench_branch_ends(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| arbor.find_branch_and_end_nodes())
}

fn bench_partitions(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| {
        let _v: Vec<Vec<_>> = arbor.partition().collect(); // do I need to assign this?
    })
}

fn bench_successors(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| arbor.all_successors())
}

fn bench_degrees(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| arbor.out_degrees())
}

fn bench_toposort(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| {
        let _v: Vec<_> = arbor.toposort().collect();
    })
}

fn bench_dfs(b: &mut Bencher) {
    let arbor = make_arbor();
    b.iter(|| {
        let _v: Vec<_> = arbor.dfs_from_root().collect();
    })
}

// cloning
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
benchmark_main!(clone, noclone);
