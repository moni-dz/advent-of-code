#![feature(portable_simd)]

use prettytable::{Table, format, row};
use serde_json::json;
use std::env;

use advent_of_code::days::{
    d1::SecretEntrance, d2::GiftShop, d3::Lobby, d4::PrintingDepartment, d5::Cafeteria,
    d6::TrashCompactor, d7::Laboratories, d8::Playground, d9::MovieTheater, d10::Factory,
    d11::Reactor, d12::ChristmasTreeFarm,
};

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
    let args: Vec<String> = env::args().collect();
    let table_output = args.contains(&"--table".to_string());

    let mut results = vec![
        run!(SecretEntrance, 1),
        run!(GiftShop, 2),
        run!(Lobby, 3),
        run!(PrintingDepartment, 4),
        run!(Cafeteria, 5),
        run!(TrashCompactor, 6),
        run!(Laboratories, 7),
        run!(Playground, 8),
        run!(MovieTheater, 9),
        run!(Factory, 10),
        run!(Reactor, 11),
        run!(ChristmasTreeFarm, 12),
    ];

    results.sort_by_key(|r| r.day);

    if !table_output {
        let json_results: Vec<_> = results
            .iter()
            .map(|r| {
                json!({
                    "day": r.day,
                    "parse_us": r.parse_us,
                    "p1_result": r.p1_result,
                    "p1_us": r.p1_us,
                    "p2_result": r.p2_result,
                    "p2_us": r.p2_us,
                    "total_us": r.parse_us + r.p1_us + r.p2_us
                })
            })
            .collect();

        let total_us: u128 = results.iter().map(|r| r.parse_us + r.p1_us + r.p2_us).sum();
        let output = json!({
            "results": json_results,
            "totals": {
                "parse_us": results.iter().map(|r| r.parse_us).sum::<u128>(),
                "p1_us": results.iter().map(|r| r.p1_us).sum::<u128>(),
                "p2_us": results.iter().map(|r| r.p2_us).sum::<u128>(),
                "total_us": total_us
            }
        });

        println!("{}", output.to_string());
    } else {
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
}
