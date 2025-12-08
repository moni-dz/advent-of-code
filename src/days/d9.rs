use crate::runner::Solution;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct MovieTheater {
    tiles: Vec<(i32, i32)>,
    boundary: HashSet<(i32, i32)>,
    xs: Vec<i32>,
    ys: Vec<i32>,
    point_cache: RefCell<HashMap<(i32, i32), bool>>,
}

impl MovieTheater {
    fn inside_rect(&self, px: i32, py: i32) -> bool {
        let mut inside = false;

        let mut x1 = self.tiles[self.tiles.len() - 1].0;
        let mut y1 = self.tiles[self.tiles.len() - 1].1;

        for (x2, y2) in &self.tiles {
            if y1 == *y2 {
                x1 = *x2;
                y1 = *y2;
                continue;
            }

            let y_in_range = (y1 <= py && py < *y2) || (*y2 <= py && py < y1);
            let x_intersects = px < (x2 - x1) * (py - y1) / (y2 - y1) + x1;

            if y_in_range && x_intersects {
                inside = !inside;
            }

            x1 = *x2;
            y1 = *y2;
        }

        inside
    }

    fn is_point_valid(&self, x: i32, y: i32) -> bool {
        let p = (x, y);
        let mut cache = self.point_cache.borrow_mut();

        if let Some(&result) = cache.get(&p) {
            return result;
        }

        let result = self.boundary.contains(&p) || self.inside_rect(x, y);
        cache.insert(p, result);
        result
    }

    fn is_rectilinear(&self, x1: i32, y1: i32, x2: i32, y2: i32) -> bool {
        let min_x = x1.min(x2);
        let max_x = x1.max(x2);
        let min_y = y1.min(y2);
        let max_y = y1.max(y2);

        let width = max_x - min_x;
        let height = max_y - min_y;

        if width <= height {
            for &x in self.xs.iter() {
                if x < min_x || x > max_x {
                    continue;
                }
                if !self.is_point_valid(x, min_y) || !self.is_point_valid(x, max_y) {
                    return false;
                }
            }

            for &y in self.ys.iter() {
                if y < min_y || y > max_y {
                    continue;
                }
                if !self.is_point_valid(min_x, y) || !self.is_point_valid(max_x, y) {
                    return false;
                }
            }
        } else {
            for &y in self.ys.iter() {
                if y < min_y || y > max_y {
                    continue;
                }
                if !self.is_point_valid(min_x, y) || !self.is_point_valid(max_x, y) {
                    return false;
                }
            }

            for &x in self.xs.iter() {
                if x < min_x || x > max_x {
                    continue;
                }
                if !self.is_point_valid(x, min_y) || !self.is_point_valid(x, max_y) {
                    return false;
                }
            }
        }

        true
    }

    fn build_boundary(&mut self) {
        self.boundary.clear();
        self.xs.clear();
        self.ys.clear();

        for i in 0..self.tiles.len() {
            let (x1, y1) = self.tiles[i];
            let (x2, y2) = self.tiles[(i + 1) % self.tiles.len()];

            self.xs.push(x1);
            self.ys.push(y1);

            if x1 == x2 {
                let (min_y, max_y) = (y1.min(y2), y1.max(y2));
                for y in min_y..=max_y {
                    self.boundary.insert((x1, y));
                }
            } else {
                let (min_x, max_x) = (x1.min(x2), x1.max(x2));
                for x in min_x..=max_x {
                    self.boundary.insert((x, y1));
                }
            }
        }

        self.xs.sort_unstable();
        self.xs.dedup();
        self.ys.sort_unstable();
        self.ys.dedup();
    }
}

impl Solution<9> for MovieTheater {
    fn parse(&mut self, input: &str) {
        self.tiles = input
            .lines()
            .filter_map(|line| {
                let mut parts = line.split(',');
                let x = parts.next()?.trim().parse::<i32>().ok()?;
                let y = parts.next()?.trim().parse::<i32>().ok()?;
                Some((x, y))
            })
            .collect();

        self.build_boundary();
    }

    fn p1(&self) -> String {
        let mut max_area = 0i64;

        for i in 0..self.tiles.len() {
            for j in (i + 1)..self.tiles.len() {
                let (x1, y1) = self.tiles[i];
                let (x2, y2) = self.tiles[j];

                let width = ((x1 - x2).abs() + 1) as i64;
                let height = ((y1 - y2).abs() + 1) as i64;
                let area = width * height;

                max_area = max_area.max(area);
            }
        }

        max_area.to_string()
    }

    fn p2(&self) -> String {
        let mut candidates: Vec<(i64, i32, i32, i32, i32)> = (0..self.tiles.len())
            .flat_map(|i| {
                (i + 1..self.tiles.len()).map(move |j| {
                    let (x1, y1) = self.tiles[i];
                    let (x2, y2) = self.tiles[j];
                    let area = (((x1 - x2).abs() + 1) as i64) * (((y1 - y2).abs() + 1) as i64);
                    (area, x1, y1, x2, y2)
                })
            })
            .collect();

        candidates.sort_unstable_by(|a, b| b.0.cmp(&a.0));

        for (area, x1, y1, x2, y2) in candidates {
            if self.is_rectilinear(x1, y1, x2, y2) {
                return area.to_string();
            }
        }

        "0".to_string()
    }

    fn prebench(&mut self) {
        self.point_cache.borrow_mut().clear();
    }
}
