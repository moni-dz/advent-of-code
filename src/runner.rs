use std::time::Instant;

pub trait Solution<const DAY: u32>: Send {
    fn parse(&mut self, input: &str);
    fn p1(&self) -> String;
    fn p2(&self) -> String;
    fn prebench(&mut self) {}
}

pub struct RunResult {
    pub day: u32,
    pub parse_us: u128,
    pub p1_result: String,
    pub p1_us: u128,
    pub p2_result: String,
    pub p2_us: u128,
}

#[macro_export]
macro_rules! input {
    (1) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day1.txt"))
    };
    (2) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day2.txt"))
    };
    (3) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day3.txt"))
    };
    (4) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day4.txt"))
    };
    (5) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day5.txt"))
    };
    (6) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day6.txt"))
    };
    (7) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day7.txt"))
    };
    (8) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day8.txt"))
    };
    (9) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day9.txt"))
    };
    (10) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day10.txt"))
    };
    (11) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day11.txt"))
    };
    (12) => {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/inputs/day12.txt"))
    };
}

const WARMUP: u32 = 100;
const BENCHES: u32 = 1000;

pub fn run_with_input<const DAY: u32, S: Solution<DAY> + Default>(solution: &mut S, input: &str) -> RunResult {
    for _ in 0..WARMUP {
        *solution = S::default();
        solution.parse(input);
        std::hint::black_box(solution.p1());
        std::hint::black_box(solution.p2());
    }

    let mut parse_min = u128::MAX;
    for _ in 0..BENCHES {
        *solution = S::default();
        let start = Instant::now();
        solution.parse(input);
        parse_min = parse_min.min(start.elapsed().as_nanos());
    }

    *solution = S::default();
    solution.parse(input);

    let mut p1_min = u128::MAX;
    let mut p1_result = String::new();
    for _ in 0..BENCHES {
        solution.prebench();
        let start = Instant::now();
        p1_result = solution.p1();
        p1_min = p1_min.min(start.elapsed().as_nanos());
        std::hint::black_box(&p1_result);
    }

    let mut p2_min = u128::MAX;
    let mut p2_result = String::new();
    for _ in 0..BENCHES {
        solution.prebench();
        let start = Instant::now();
        p2_result = solution.p2();
        p2_min = p2_min.min(start.elapsed().as_nanos());
        std::hint::black_box(&p2_result);
    }

    RunResult {
        day: DAY,
        parse_us: (parse_min + 500) / 1000,
        p1_result,
        p1_us: (p1_min + 500) / 1000,
        p2_result,
        p2_us: (p2_min + 500) / 1000,
    }
}
