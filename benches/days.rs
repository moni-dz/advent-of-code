use advent_of_code::days::{d1, d2, d3, d4, d5, d6, d7, d8};
use advent_of_code::input;
use advent_of_code::runner::Solution;
use criterion::{Criterion, black_box, criterion_group, criterion_main};

macro_rules! bench {
    ($func_name:ident, $group_name:tt, $day_num:tt, $solution_type:ty) => {
        fn $func_name(c: &mut Criterion) {
            let mut group = c.benchmark_group($group_name);

            group.bench_function("parse", |b| {
                let mut solution = <$solution_type>::default();
                b.iter(|| {
                    solution.parse(black_box(input!($day_num)));
                });
            });

            group.bench_function("p1", |b| {
                let mut solution = <$solution_type>::default();
                solution.parse(input!($day_num));
                b.iter(|| {
                    black_box(solution.p1());
                });
            });

            group.bench_function("p2", |b| {
                let mut solution = <$solution_type>::default();
                solution.parse(input!($day_num));
                b.iter(|| {
                    black_box(solution.p2());
                });
            });

            group.bench_function("total", |b| {
                b.iter(|| {
                    let mut solution = <$solution_type>::default();
                    solution.parse(black_box(input!($day_num)));
                    black_box(solution.p1());
                    black_box(solution.p2());
                });
            });

            group.finish();
        }
    };
}

bench!(bench_d1, "d1", 1, d1::SecretEntrance);
bench!(bench_d2, "d2", 2, d2::GiftShop);
bench!(bench_d3, "d3", 3, d3::Lobby);
bench!(bench_d4, "d4", 4, d4::PrintingDepartment);
bench!(bench_d5, "d5", 5, d5::Cafeteria);
bench!(bench_d6, "d6", 6, d6::TrashCompactor);
bench!(bench_d7, "d7", 7, d7::Laboratories);
bench!(bench_d8, "d8", 8, d8::Playground);

criterion_group!(
    benches, bench_d1, bench_d2, bench_d3, bench_d4, bench_d5, bench_d6, bench_d7, bench_d8
);
criterion_main!(benches);
