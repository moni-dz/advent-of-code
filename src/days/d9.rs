use crate::runner::Solution;
use glam::IVec2;
use std::collections::HashMap;

#[derive(Default)]
pub struct MovieTheater {
    tiles: Vec<IVec2>,
    vertical_edges: HashMap<i32, Vec<(i32, i32)>>,
    horizontal_edges: HashMap<i32, Vec<(i32, i32)>>,
}

impl MovieTheater {
    fn is_bounded(&self, p: IVec2) -> bool {
        if let Some(ranges) = self.vertical_edges.get(&p.x) {
            for &(min_y, max_y) in ranges {
                if p.y >= min_y && p.y <= max_y {
                    return true;
                }
            }
        }

        if let Some(ranges) = self.horizontal_edges.get(&p.y) {
            for &(min_x, max_x) in ranges {
                if p.x >= min_x && p.x <= max_x {
                    return true;
                }
            }
        }

        false
    }

    fn build_edge_maps(&mut self) {
        self.vertical_edges.clear();
        self.horizontal_edges.clear();

        for i in 0..self.tiles.len() {
            let p1 = self.tiles[i];
            let p2 = self.tiles[(i + 1) % self.tiles.len()];

            if p1.x == p2.x {
                let min_y = p1.y.min(p2.y);
                let max_y = p1.y.max(p2.y);

                self.vertical_edges
                    .entry(p1.x)
                    .or_insert_with(Vec::new)
                    .push((min_y, max_y));
            } else {
                let min_x = p1.x.min(p2.x);
                let max_x = p1.x.max(p2.x);

                self.horizontal_edges
                    .entry(p1.y)
                    .or_insert_with(Vec::new)
                    .push((min_x, max_x));
            }
        }
    }

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

    fn is_point_valid(&self, p: IVec2, point_cache: &mut HashMap<IVec2, bool>) -> bool {
        if let Some(&result) = point_cache.get(&p) {
            return result;
        }

        let result = self.is_bounded(p) || self.inside_rect(p);
        point_cache.insert(p, result);
        result
    }

    fn is_rectilinear(
        &self,
        p1: IVec2,
        p2: IVec2,
        xs: &[i32],
        ys: &[i32],
        point_cache: &mut HashMap<IVec2, bool>,
    ) -> bool {
        let min = p1.min(p2);
        let max = p1.max(p2);

        if !self.is_point_valid(IVec2::new(min.x, min.y), point_cache)
            || !self.is_point_valid(IVec2::new(max.x, min.y), point_cache)
            || !self.is_point_valid(IVec2::new(min.x, max.y), point_cache)
            || !self.is_point_valid(IVec2::new(max.x, max.y), point_cache)
        {
            return false;
        }

        let width = max.x - min.x;
        let height = max.y - min.y;

        if width <= height {
            for &x in xs {
                if (min.x..=max.x).contains(&x) {
                    if !self.is_point_valid(IVec2::new(x, min.y), point_cache)
                        || !self.is_point_valid(IVec2::new(x, max.y), point_cache)
                    {
                        return false;
                    }
                }
            }

            for &y in ys {
                if (min.y..=max.y).contains(&y) {
                    if !self.is_point_valid(IVec2::new(min.x, y), point_cache)
                        || !self.is_point_valid(IVec2::new(max.x, y), point_cache)
                    {
                        return false;
                    }
                }
            }
        } else {
            for &y in ys {
                if (min.y..=max.y).contains(&y) {
                    if !self.is_point_valid(IVec2::new(min.x, y), point_cache)
                        || !self.is_point_valid(IVec2::new(max.x, y), point_cache)
                    {
                        return false;
                    }
                }
            }

            for &x in xs {
                if (min.x..=max.x).contains(&x) {
                    if !self.is_point_valid(IVec2::new(x, min.y), point_cache)
                        || !self.is_point_valid(IVec2::new(x, max.y), point_cache)
                    {
                        return false;
                    }
                }
            }
        }

        true
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
        self.build_edge_maps();
    }

    fn p1(&self) -> String {
        (0..self.tiles.len())
            .flat_map(|i| {
                (i + 1..self.tiles.len()).map(move |j| {
                    let delta = (self.tiles[j] - self.tiles[i]).abs();
                    (delta.x + 1) as i64 * (delta.y + 1) as i64
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

        let mut candidates: Vec<(i64, IVec2, IVec2)> = (0..self.tiles.len())
            .flat_map(|i| {
                (i + 1..self.tiles.len()).map(move |j| {
                    let delta = (self.tiles[j] - self.tiles[i]).abs();
                    let area = (((delta.x) + 1) as i64) * (((delta.y) + 1) as i64);
                    (area, self.tiles[i], self.tiles[j])
                })
            })
            .filter(|&(_, p1, p2)| {
                let delta = (p2 - p1).abs();
                delta.x > 0 && delta.y > 0
            })
            .collect();

        candidates.sort_unstable_by_key(|&(area, _, _)| std::cmp::Reverse(area));

        let mut point_cache = HashMap::new();

        for (area, p1, p2) in candidates {
            if self.is_rectilinear(p1, p2, &xs, &ys, &mut point_cache) {
                return area.to_string();
            }
        }

        "0".to_string()
    }
}
