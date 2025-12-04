use std::time::Instant;

pub trait Solution<const DAY: u32>: Send {
    fn parse(&mut self, input: &str);
    fn p1(&self) -> String;
    fn p2(&self) -> String;
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

pub fn run_with_input<const DAY: u32, S: Solution<DAY>>(
    solution: &mut S,
    input: &str,
) -> RunResult {
    let parse_start = Instant::now();
    solution.parse(input);
    let parse_us = parse_start.elapsed().as_micros();

    let p1_start = Instant::now();
    let p1_result = solution.p1();
    let p1_us = p1_start.elapsed().as_micros();

    let p2_start = Instant::now();
    let p2_result = solution.p2();
    let p2_us = p2_start.elapsed().as_micros();

    RunResult {
        day: DAY,
        parse_us,
        p1_result,
        p1_us,
        p2_result,
        p2_us,
    }
}
