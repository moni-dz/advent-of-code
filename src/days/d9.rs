use crate::runner::Solution;
use glam::IVec2;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct MovieTheater {
    tiles: Vec<IVec2>,
}

impl MovieTheater {
    fn inside_rect(&self, p: IVec2) -> bool {
        let mut inside = false;
        let mut prev = self.tiles[self.tiles.len() - 1];

        for curr in &self.tiles {
            if prev.y == curr.y {
                prev = *curr;
                continue;
            }

            let y_in_range = (prev.y <= p.y && p.y < curr.y) || (curr.y <= p.y && p.y < prev.y);
            let x_intersects =
                p.x < (curr.x - prev.x) * (p.y - prev.y) / (curr.y - prev.y) + prev.x;

            if y_in_range && x_intersects {
                inside = !inside;
            }

            prev = *curr;
        }

        inside
    }

    fn is_point_valid(
        &self,
        p: IVec2,
        boundary: &HashSet<IVec2>,
        point_cache: &mut HashMap<IVec2, bool>,
    ) -> bool {
        if let Some(&result) = point_cache.get(&p) {
            return result;
        }

        let result = boundary.contains(&p) || self.inside_rect(p);
        point_cache.insert(p, result);
        result
    }

    fn is_rectilinear(
        &self,
        p1: IVec2,
        p2: IVec2,
        xs: &[i32],
        ys: &[i32],
        boundary: &HashSet<IVec2>,
        point_cache: &mut HashMap<IVec2, bool>,
    ) -> bool {
        let min = p1.min(p2);
        let max = p1.max(p2);

        let width = max.x - min.x;
        let height = max.y - min.y;

        if width <= height {
            for &x in xs {
                if (min.x..=max.x).contains(&x) {
                    if !self.is_point_valid(IVec2::new(x, min.y), boundary, point_cache)
                        || !self.is_point_valid(IVec2::new(x, max.y), boundary, point_cache)
                    {
                        return false;
                    }
                }
            }

            for &y in ys {
                if (min.y..=max.y).contains(&y) {
                    if !self.is_point_valid(IVec2::new(min.x, y), boundary, point_cache)
                        || !self.is_point_valid(IVec2::new(max.x, y), boundary, point_cache)
                    {
                        return false;
                    }
                }
            }
        } else {
            for &y in ys {
                if (min.y..=max.y).contains(&y) {
                    if !self.is_point_valid(IVec2::new(min.x, y), boundary, point_cache)
                        || !self.is_point_valid(IVec2::new(max.x, y), boundary, point_cache)
                    {
                        return false;
                    }
                }
            }

            for &x in xs {
                if (min.x..=max.x).contains(&x) {
                    if !self.is_point_valid(IVec2::new(x, min.y), boundary, point_cache)
                        || !self.is_point_valid(IVec2::new(x, max.y), boundary, point_cache)
                    {
                        return false;
                    }
                }
            }
        }

        true
    }

    fn build_boundary(&self, boundary: &mut HashSet<IVec2>) {
        boundary.clear();

        for i in 0..self.tiles.len() {
            let p1 = self.tiles[i];
            let p2 = self.tiles[(i + 1) % self.tiles.len()];

            if p1.x == p2.x {
                for y in p1.y.min(p2.y)..=p1.y.max(p2.y) {
                    boundary.insert(IVec2::new(p1.x, y));
                }
            } else {
                for x in p1.x.min(p2.x)..=p1.x.max(p2.x) {
                    boundary.insert(IVec2::new(x, p1.y));
                }
            }
        }
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
                Some(IVec2::new(x, y))
            })
            .collect();
    }

    fn p1(&self) -> String {
        (0..self.tiles.len())
            .flat_map(|i| {
                (i + 1..self.tiles.len()).map(move |j| {
                    let p1 = self.tiles[i];
                    let p2 = self.tiles[j];
                    let delta = (p2 - p1).abs();
                    let area = (((delta.x) + 1) as i64) * (((delta.y) + 1) as i64);
                    area
                })
            })
            .max()
            .unwrap_or(0)
            .to_string()
    }

    fn p2(&self) -> String {
        let mut xs: Vec<_> = self.tiles.iter().map(|p| p.x).collect();
        let mut ys: Vec<_> = self.tiles.iter().map(|p| p.y).collect();

        xs.sort_unstable();
        xs.dedup();
        ys.sort_unstable();
        ys.dedup();

        let mut boundary = HashSet::new();
        self.build_boundary(&mut boundary);

        let mut point_cache = HashMap::new();

        let mut candidates: Vec<(i64, IVec2, IVec2)> = (0..self.tiles.len())
            .flat_map(|i| {
                (i + 1..self.tiles.len()).map(move |j| {
                    let p1 = self.tiles[i];
                    let p2 = self.tiles[j];
                    let delta = (p2 - p1).abs();
                    let area = (((delta.x) + 1) as i64) * (((delta.y) + 1) as i64);
                    (area, p1, p2)
                })
            })
            .collect();

        candidates.sort_unstable_by_key(|&(area, _, _)| std::cmp::Reverse(area));

        for (area, p1, p2) in candidates {
            if self.is_rectilinear(p1, p2, &xs, &ys, &boundary, &mut point_cache) {
                return area.to_string();
            }
        }

        "0".to_string()
    }
}
