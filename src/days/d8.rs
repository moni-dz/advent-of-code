use std::hash::BuildHasherDefault;
use std::{collections::HashMap, num::NonZero};

use crate::runner::Solution;
use kiddo::{ImmutableKdTree, SquaredEuclidean};
use rayon::prelude::*;
use twox_hash::XxHash64;
use union_find::{QuickUnionUf, UnionBySize, UnionFind};

type Junction = (i32, i32, i32);
type Edge = (i64, usize, usize);

#[derive(Default)]
pub struct Playground {
    boxes: Vec<Junction>,
    tree: Option<ImmutableKdTree<f64, 3>>,
    candidates: usize,
}

impl Playground {
    fn find_distances(&self) -> Vec<Edge> {
        let tree = self.tree.as_ref().unwrap();

        let mut distances: Vec<Edge> = self
            .boxes
            .par_iter()
            .enumerate()
            .flat_map(|(i, &point)| {
                let query = [point.0 as f64, point.1 as f64, point.2 as f64];
                let nearest = tree
                    .nearest_n::<SquaredEuclidean>(&query, NonZero::new(self.candidates).unwrap());

                nearest
                    .iter()
                    .filter_map(move |nn| {
                        let j = nn.item as usize;
                        (j > i).then_some((nn.distance as i64, i, j))
                    })
                    .collect::<Vec<Edge>>()
            })
            .collect();

        distances.sort_unstable_by_key(|d| d.0);
        distances
    }

    fn connect_circuits<const P: usize>(&self) -> String {
        let n = self.boxes.len();
        let distances: Vec<(i64, usize, usize)> = self.find_distances();

        let mut uf = QuickUnionUf::<UnionBySize>::new(n);
        let mut last = (0, 0);
        let mut components = n;
        let mut count = 0;

        for (_, i, j) in distances.iter() {
            count += 1;

            if uf.find(*i) != uf.find(*j) {
                uf.union(*i, *j);
                last = (*i, *j);
                components -= 1;
            }

            if (P == 1 && count >= 1000) || (P == 2 && components == 1) {
                break;
            }
        }

        if P == 1 {
            let mut sizes = HashMap::<_, _, BuildHasherDefault<XxHash64>>::default();
            for i in 0..n {
                let root = uf.find(i);
                *sizes.entry(root).or_insert(0) += 1;
            }

            let mut circuits: Vec<usize> = sizes.values().copied().collect();
            circuits.sort_unstable_by(|a, b| b.cmp(a));

            circuits.iter().take(3).product::<usize>().to_string()
        } else {
            ((self.boxes[last.0].0 as i64) * (self.boxes[last.1].0 as i64)).to_string()
        }
    }
}

impl Solution<8> for Playground {
    fn parse(&mut self, input: &str) {
        let mut entries = Vec::new();

        self.boxes = input
            .lines()
            .filter_map(|line| {
                let mut parts = line.split(',');
                let x: i32 = parts.next()?.trim().parse().ok()?;
                let y: i32 = parts.next()?.trim().parse().ok()?;
                let z: i32 = parts.next()?.trim().parse().ok()?;

                entries.push([x as f64, y as f64, z as f64]);
                Some((x, y, z))
            })
            .collect();

        self.tree = Some(ImmutableKdTree::new_from_slice(&entries));
        self.candidates = 5.max(self.boxes.len() / 40);
    }

    fn p1(&self) -> String {
        self.connect_circuits::<1>()
    }

    fn p2(&self) -> String {
        self.connect_circuits::<2>()
    }
}
