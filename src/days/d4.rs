use crate::runner::Solution;
use num_complex::Complex;
use std::collections::VecDeque;

type Pos = Complex<i32>;

struct BitSet {
    bits: Vec<u64>,
}

impl Clone for BitSet {
    fn clone(&self) -> Self {
        Self {
            bits: self.bits.clone(),
        }
    }
}

impl Default for BitSet {
    fn default() -> Self {
        Self { bits: Vec::new() }
    }
}

impl BitSet {
    fn new(size: usize) -> Self {
        Self {
            bits: vec![0; (size + 63) / 64],
        }
    }

    #[inline]
    fn set(&mut self, idx: usize) {
        self.bits[idx / 64] |= 1 << (idx % 64);
    }

    #[inline]
    fn get(&self, idx: usize) -> bool {
        (self.bits[idx / 64] >> (idx % 64)) & 1 != 0
    }
}

const DIRECTIONS: [Pos; 8] = [
    Complex::new(-1, -1),
    Complex::new(-1, 0),
    Complex::new(-1, 1),
    Complex::new(0, -1),
    Complex::new(0, 1),
    Complex::new(1, -1),
    Complex::new(1, 0),
    Complex::new(1, 1),
];

struct Grid {
    rows: i32,
    cols: i32,
}

impl Grid {
    fn new(rows: usize, cols: usize) -> Self {
        Self {
            rows: rows as i32,
            cols: cols as i32,
        }
    }

    #[inline]
    fn contains(&self, pos: Pos) -> bool {
        pos.re >= 0 && pos.re < self.rows && pos.im >= 0 && pos.im < self.cols
    }

    #[inline]
    fn idx(&self, pos: Pos) -> usize {
        (pos.re * self.cols + pos.im) as usize
    }

    #[inline]
    fn pos(&self, idx: usize) -> Pos {
        Complex::new(idx as i32 / self.cols, idx as i32 % self.cols)
    }

    #[inline]
    fn size(&self) -> usize {
        (self.rows * self.cols) as usize
    }

    #[inline]
    fn neighbors(&self, pos: Pos) -> impl Iterator<Item = Pos> + '_ {
        DIRECTIONS
            .iter()
            .map(move |&d| pos + d)
            .filter(|&n| self.contains(n))
    }
}

#[derive(Default)]
pub struct PrintingDepartment {
    grid: Option<Grid>,
    is_roll: BitSet,
    neighbors: Vec<u8>,
}

impl Solution<4> for PrintingDepartment {
    fn parse(&mut self, input: &str) {
        let lines: Vec<&[u8]> = input.lines().map(|l| l.as_bytes()).collect();
        let rows = lines.len();
        let cols = lines[0].len();

        let grid = Grid::new(rows, cols);
        let size = grid.size();

        self.is_roll = BitSet::new(size);
        for (r, line) in lines.iter().enumerate() {
            for (c, &b) in line.iter().enumerate() {
                if b == b'@' {
                    self.is_roll.set(grid.idx(Complex::new(r as i32, c as i32)));
                }
            }
        }

        self.neighbors = vec![0; size];
        for idx in 0..size {
            if self.is_roll.get(idx) {
                self.neighbors[idx] = grid
                    .neighbors(grid.pos(idx))
                    .filter(|&n| self.is_roll.get(grid.idx(n)))
                    .count() as u8;
            }
        }

        self.grid = Some(grid);
    }

    fn p1(&self) -> String {
        (0..self.is_roll.bits.len() * 64)
            .filter(|&idx| self.is_roll.get(idx) && self.neighbors[idx] < 4)
            .count()
            .to_string()
    }

    fn p2(&self) -> String {
        let grid = self.grid.as_ref().unwrap();
        let size = grid.size();

        let mut is_roll = self.is_roll.clone();
        let mut neighbors = self.neighbors.clone();
        let mut visited = BitSet::new(size);

        let mut q: VecDeque<Pos> = VecDeque::with_capacity(size);

        for idx in 0..size {
            if is_roll.get(idx) && neighbors[idx] < 4 {
                q.push_back(grid.pos(idx));
                visited.set(idx);
            }
        }

        let mut removed = 0;

        while let Some(pos) = q.pop_front() {
            let idx = grid.idx(pos);

            if !is_roll.get(idx) {
                continue;
            }

            is_roll.bits[idx / 64] &= !(1 << (idx % 64));
            removed += 1;

            for neighbor in grid.neighbors(pos) {
                let n_idx = grid.idx(neighbor);

                if is_roll.get(n_idx) {
                    neighbors[n_idx] -= 1;

                    if neighbors[n_idx] < 4 && !visited.get(n_idx) {
                        visited.set(n_idx);
                        q.push_back(neighbor);
                    }
                }
            }
        }

        removed.to_string()
    }
}
