use std::cell::RefCell;

use crate::runner::Solution;

#[derive(Default)]
pub struct Laboratories {
    grid: Vec<u8>,
    dimensions: (usize, usize),
    start: usize,
    beams: RefCell<Option<Vec<u64>>>,
}

impl Laboratories {
    fn simulate(&self) -> u64 {
        let (width, height) = self.dimensions;
        let mut beams = vec![0u64; width];
        let mut splits = 0;

        beams[self.start] = 1;

        for row in 1..height {
            let step = row;
            let (min_col, max_col) = (
                self.start.saturating_sub(step),
                (self.start + step + 1).min(width),
            );

            for col in min_col..max_col {
                if beams[col] > 0 && self.grid[row * self.dimensions.0 + col] == b'^' {
                    splits += 1;
                    beams[col - 1] += beams[col];
                    beams[col + 1] += beams[col];
                    beams[col] = 0;
                }
            }
        }

        *self.beams.borrow_mut() = Some(beams.clone());
        splits
    }
}

impl Solution<7> for Laboratories {
    fn parse(&mut self, input: &str) {
        let lines: Vec<&str> = input
            .lines()
            .enumerate()
            .filter(|(i, _)| *i == 0 || *i % 2 == 0)
            .map(|(_, line)| line)
            .collect();

        let width = lines.first().map(|line| line.len()).unwrap_or(0);
        let height = lines.len();

        self.dimensions = (width, height);
        self.grid = lines.iter().flat_map(|line| line.bytes()).collect();
        self.start = width / 2;
    }

    fn p1(&self) -> String {
        self.simulate().to_string()
    }

    fn p2(&self) -> String {
        if let Some(beams) = self.beams.borrow().as_ref() {
            beams.iter().sum::<u64>().to_string()
        } else {
            self.simulate();

            self.beams
                .borrow()
                .as_ref()
                .unwrap()
                .iter()
                .sum::<u64>()
                .to_string()
        }
    }
}
