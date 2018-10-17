extern crate arbor;
#[macro_use]
extern crate bencher;

use bencher::Bencher;

use arbor::*;


fn make_arbor() -> Arbor<u64> {
    let mut arbor = Arbor::new();

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

fn bench_reroot(b: &mut Bencher) {
    let arbor = make_arbor();

    b.iter(|| {
        arbor.clone().reroot(500);
    });
}

benchmark_group!(arbor, bench_reroot);
benchmark_main!(arbor);