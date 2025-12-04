use crate::runner::Solution;
use num_complex::Complex;

type Pos = Complex<i32>;

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
    is_roll: Vec<bool>,
    neighbors: Vec<u8>,
}

impl Solution<4> for PrintingDepartment {
    fn parse(&mut self, input: &str) {
        let lines: Vec<&[u8]> = input.lines().map(|l| l.as_bytes()).collect();
        let rows = lines.len();
        let cols = if rows > 0 { lines[0].len() } else { 0 };

        let grid = Grid::new(rows, cols);
        let size = grid.size();

        self.is_roll = vec![false; size];
        for (r, line) in lines.iter().enumerate() {
            for (c, &b) in line.iter().enumerate() {
                self.is_roll[grid.idx(Complex::new(r as i32, c as i32))] = b == b'@';
            }
        }

        self.neighbors = vec![0; size];
        for idx in 0..size {
            if self.is_roll[idx] {
                self.neighbors[idx] = grid
                    .neighbors(grid.pos(idx))
                    .filter(|&n| self.is_roll[grid.idx(n)])
                    .count() as u8;
            }
        }

        self.grid = Some(grid);
    }

    fn p1(&self) -> String {
        (0..self.is_roll.len())
            .filter(|&idx| self.is_roll[idx] && self.neighbors[idx] < 4)
            .count()
            .to_string()
    }

    fn p2(&self) -> String {
        let grid = self.grid.as_ref().unwrap();
        let size = grid.size();

        let mut is_roll = self.is_roll.clone();
        let mut neighbors = self.neighbors.clone();

        let mut in_q = vec![false; size];
        let mut q: Vec<Pos> = Vec::with_capacity(size);

        for idx in 0..size {
            if is_roll[idx] && neighbors[idx] < 4 {
                q.push(grid.pos(idx));
                in_q[idx] = true;
            }
        }

        let mut removed = 0;
        let mut head = 0;

        while head < q.len() {
            let pos = q[head];
            let idx = grid.idx(pos);
            head += 1;
            in_q[idx] = false;

            if !is_roll[idx] || neighbors[idx] >= 4 {
                continue;
            }

            is_roll[idx] = false;
            removed += 1;

            for neighbor in grid.neighbors(pos) {
                let n_idx = grid.idx(neighbor);
                if is_roll[n_idx] {
                    neighbors[n_idx] -= 1;
                    if neighbors[n_idx] < 4 && !in_q[n_idx] {
                        q.push(neighbor);
                        in_q[n_idx] = true;
                    }
                }
            }
        }

        removed.to_string()
    }
}
