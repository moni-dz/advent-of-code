#![feature(portable_simd)]

use advent_of_code::days::{
    d1::SecretEntrance, d2::GiftShop, d3::Lobby, d4::PrintingDepartment, d5::Cafeteria,
    d6::TrashCompactor, d7::Laboratories, d8::Playground,
};

use advent_of_code::input;
use advent_of_code::runner::Solution;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn run_day<const DAY: u32, S: Solution<DAY> + Default>(input: &str) {
    let mut s = S::default();
    s.parse(input);
    println!("{}\n{}", s.p1(), s.p2());
}

fn main() {
    let day: u32 = std::env::args()
        .nth(1)
        .expect("profile <day>")
        .parse()
        .expect("day is not a number");

    match day {
        1 => run_day::<1, SecretEntrance>(input!(1)),
        2 => run_day::<2, GiftShop>(input!(2)),
        3 => run_day::<3, Lobby>(input!(3)),
        4 => run_day::<4, PrintingDepartment>(input!(4)),
        5 => run_day::<5, Cafeteria>(input!(5)),
        6 => run_day::<6, TrashCompactor>(input!(6)),
        7 => run_day::<7, Laboratories>(input!(7)),
        8 => run_day::<8, Playground>(input!(8)),
        _ => eprintln!("day {} unimplemented", day),
    }
}
