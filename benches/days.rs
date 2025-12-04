use advent_of_code::days::{d1, d2, d3, d4};
use advent_of_code::input;
use advent_of_code::runner::Solution;
use criterion::{Criterion, black_box, criterion_group, criterion_main};

fn bench_d1(c: &mut Criterion) {
    let mut group = c.benchmark_group("d1");

    group.bench_function("parse", |b| {
        let mut solution = d1::SecretEntrance::default();
        b.iter(|| {
            solution.parse(black_box(input!(1)));
        });
    });

    group.bench_function("p1", |b| {
        let mut solution = d1::SecretEntrance::default();
        solution.parse(input!(1));
        b.iter(|| {
            black_box(solution.p1());
        });
    });

    group.bench_function("p2", |b| {
        let mut solution = d1::SecretEntrance::default();
        solution.parse(input!(1));
        b.iter(|| {
            black_box(solution.p2());
        });
    });

    group.bench_function("total", |b| {
        b.iter(|| {
            let mut solution = d1::SecretEntrance::default();
            solution.parse(black_box(input!(1)));
            black_box(solution.p1());
            black_box(solution.p2());
        });
    });

    group.finish();
}

fn bench_d2(c: &mut Criterion) {
    let mut group = c.benchmark_group("d2");

    group.bench_function("parse", |b| {
        let mut solution = d2::GiftShop::default();
        b.iter(|| {
            solution.parse(black_box(input!(2)));
        });
    });

    group.bench_function("p1", |b| {
        let mut solution = d2::GiftShop::default();
        solution.parse(input!(2));
        b.iter(|| {
            black_box(solution.p1());
        });
    });

    group.bench_function("p2", |b| {
        let mut solution = d2::GiftShop::default();
        solution.parse(input!(2));
        b.iter(|| {
            black_box(solution.p2());
        });
    });

    group.bench_function("total", |b| {
        b.iter(|| {
            let mut solution = d2::GiftShop::default();
            solution.parse(black_box(input!(2)));
            black_box(solution.p1());
            black_box(solution.p2());
        });
    });

    group.finish();
}

fn bench_d3(c: &mut Criterion) {
    let mut group = c.benchmark_group("d3");

    group.bench_function("parse", |b| {
        let mut solution = d3::Lobby::default();
        b.iter(|| {
            solution.parse(black_box(input!(3)));
        });
    });

    group.bench_function("p1", |b| {
        let mut solution = d3::Lobby::default();
        solution.parse(input!(3));
        b.iter(|| {
            black_box(solution.p1());
        });
    });

    group.bench_function("p2", |b| {
        let mut solution = d3::Lobby::default();
        solution.parse(input!(3));
        b.iter(|| {
            black_box(solution.p2());
        });
    });

    group.bench_function("total", |b| {
        b.iter(|| {
            let mut solution = d3::Lobby::default();
            solution.parse(black_box(input!(3)));
            black_box(solution.p1());
            black_box(solution.p2());
        });
    });

    group.finish();
}

fn bench_d4(c: &mut Criterion) {
    let mut group = c.benchmark_group("d4");

    group.bench_function("parse", |b| {
        let mut solution = d4::PrintingDepartment::default();
        b.iter(|| {
            solution.parse(black_box(input!(4)));
        });
    });

    group.bench_function("p1", |b| {
        let mut solution = d4::PrintingDepartment::default();
        solution.parse(input!(4));
        b.iter(|| {
            black_box(solution.p1());
        });
    });

    group.bench_function("p2", |b| {
        let mut solution = d4::PrintingDepartment::default();
        solution.parse(input!(4));
        b.iter(|| {
            black_box(solution.p2());
        });
    });

    group.bench_function("total", |b| {
        b.iter(|| {
            let mut solution = d4::PrintingDepartment::default();
            solution.parse(black_box(input!(4)));
            black_box(solution.p1());
            black_box(solution.p2());
        });
    });

    group.finish();
}

criterion_group!(benches, bench_d1, bench_d2, bench_d3, bench_d4);
criterion_main!(benches);
