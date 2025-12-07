use crate::runner::Solution;
use std::collections::VecDeque;
use memchr::memchr;

pub struct Laboratories {
    grid: Vec<u8>,
    dimensions: (usize, usize),
    start: (usize, usize),
    memo: Vec<usize>,
}

impl Default for Laboratories {
    fn default() -> Self {
        Self {
            grid: Vec::new(),
            dimensions: (0, 0),
            start: (0, 0),
            memo: Vec::new(),
        }
    }
}

impl Laboratories {
    #[inline]
    fn idx(&self, row: usize, col: usize) -> usize {
        row * self.dimensions.0 + col
    }

    #[inline]
    fn get(&self, row: usize, col: usize) -> u8 {
        self.grid[self.idx(row, col)]
    }
}

impl Solution<7> for Laboratories {
    fn parse(&mut self, input: &str) {
        let lines: Vec<&str> = input.lines().collect();
        let width = lines.first().map(|line| line.len()).unwrap_or(0);
        let height = lines.len();
    
        self.dimensions = (width, height);
        self.grid = lines.iter().flat_map(|line| line.bytes()).collect();
        self.memo.resize(width * height, 0);

        if let Some(idx) = memchr(b'S', &self.grid) {
            self.start = (idx / width, idx % width);
        }
    }

    fn p1(&self) -> String {
        let (width, height) = self.dimensions;
        let mut splits = 0;
        let mut queue = VecDeque::new();
        let mut visited_beams = vec![false; width * height];
        let mut visited_splitters = vec![false; width * height];

        queue.push_back(self.start);

        while let Some((mut row, col)) = queue.pop_front() {
            let beam_idx = self.idx(row, col);

            if visited_beams[beam_idx] {
                continue;
            }

            visited_beams[beam_idx] = true;

            loop {
                row += 1;

                if row >= height {
                    break;
                }

                match self.get(row, col) {
                    b'S' | b'.' => continue,
                    b'^' => {
                        let split_idx = self.idx(row, col);

                        if !visited_splitters[split_idx] {
                            visited_splitters[split_idx] = true;
                            splits += 1;
                        }

                        if col > 0 {
                            queue.push_back((row, col - 1));
                        }

                        if col + 1 < width {
                            queue.push_back((row, col + 1));
                        }

                        break;
                    }
                    _ => break,
                }
            }
        }

        splits.to_string()
    }

    fn p2(&self) -> String {
        let (width, height) = self.dimensions;
        let mut memo = self.memo.clone();

        for row in (0..height).rev() {
            for col in 0..width {
                memo[self.idx(row, col)] = if row == height - 1 {
                    1
                } else {
                    let next_row = row + 1;
                    if self.get(next_row, col) == b'^' {
                        let left = if col > 0 { memo[self.idx(next_row, col - 1)] } else { 0 };
                        let right = if col + 1 < width { memo[self.idx(next_row, col + 1)] } else { 0 };

                        left + right
                    } else {
                        memo[self.idx(next_row, col)]
                    }
                };
            }
        }

        memo[self.idx(self.start.0, self.start.1)].to_string()
    }

    fn prebench(&mut self) {
        let (height, width) = self.dimensions;
        self.memo.clear();
        self.memo.resize(width * height, 0);
    }
}