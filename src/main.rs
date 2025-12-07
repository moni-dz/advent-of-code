#![feature(portable_simd)]

use prettytable::{Table, format, row};

use advent_of_code::days::{d1::SecretEntrance, d2::GiftShop, d3::Lobby, d4::PrintingDepartment, d5::Cafeteria, d6::TrashCompactor, d7::Laboratories};

use advent_of_code::input;
use advent_of_code::runner::run_with_input;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

macro_rules! run {
    ($solution_type:ty, $day_num:tt) => {{
        let mut solution = <$solution_type>::default();
        run_with_input(&mut solution, input!($day_num))
    }};
}

fn main() {
    let mut results = vec![
        run!(SecretEntrance, 1),
        run!(GiftShop, 2),
        run!(Lobby, 3),
        run!(PrintingDepartment, 4),
        run!(Cafeteria, 5),
        run!(TrashCompactor, 6),
        run!(Laboratories, 7),
    ];

    results.sort_by_key(|r| r.day);

    let mut table = Table::new();
    table.set_format(*format::consts::FORMAT_BOX_CHARS);

    table.set_titles(row![
        bc->"day",
        bc->"parse (µs)",
        bc->"p1 result",
        bc->"time (µs)",
        bc->"p2 result",
        bc->"time (µs)",
        bc->"total (µs)"
    ]);

    for r in &results {
        table.add_row(row![
            c->r.day,
            r->r.parse_us,
            c->r.p1_result,
            r->r.p1_us,
            c->r.p2_result,
            r->r.p2_us,
            r->(r.parse_us + r.p1_us + r.p2_us)
        ]);
    }

    let total_us: u128 = results.iter().map(|r| r.parse_us + r.p1_us + r.p2_us).sum();
    table.add_row(row![
        bc->"Total",
        r->results.iter().map(|r| r.parse_us).sum::<u128>(),
        c->"xxxxxxxxxxxxxxxxx",
        r->results.iter().map(|r| r.p1_us).sum::<u128>(),
        c->"xxxxxxxxxxxxxxxxx",
        r->results.iter().map(|r| r.p2_us).sum::<u128>(),
        rb->total_us
    ]);

    table.printstd();
}
