use crate::runner::Solution;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct Reactor {
    graph: HashMap<String, Vec<String>>,
}

impl Reactor {
    fn count_paths(&self, current: &str, target: &str, visited: &mut HashSet<String>) -> usize {
        if current == target {
            return 1;
        }

        let mut total = 0;

        if let Some(neighbors) = self.graph.get(current) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    visited.insert(neighbor.clone());
                    total += self.count_paths(neighbor, target, visited);
                    visited.remove(neighbor);
                }
            }
        }

        total
    }

    fn count_paths_with_visits(
        &self,
        current: &str,
        target: &str,
        visited_dac: bool,
        visited_fft: bool,
        memo: &mut HashMap<(String, bool, bool), usize>,
    ) -> usize {
        if current == target {
            return if visited_dac && visited_fft { 1 } else { 0 };
        }

        let key = (current.to_string(), visited_dac, visited_fft);

        if let Some(&result) = memo.get(&key) {
            return result;
        }

        let mut total = 0;

        if let Some(neighbors) = self.graph.get(current) {
            for neighbor in neighbors {
                total += self.count_paths_with_visits(
                    neighbor,
                    target,
                    visited_dac || neighbor == "dac",
                    visited_fft || neighbor == "fft",
                    memo,
                );
            }
        }

        memo.insert(key, total);
        total
    }
}

impl Solution<11> for Reactor {
    fn parse(&mut self, input: &str) {
        for line in input.lines() {
            if let Some((device, outputs)) = line.split_once(':') {
                let device = device.trim().to_string();
                let outputs: Vec<String> =
                    outputs.split_whitespace().map(|s| s.to_string()).collect();

                self.graph.insert(device, outputs);
            }
        }
    }

    fn p1(&self) -> String {
        self.count_paths("you", "out", &mut HashSet::new())
            .to_string()
    }

    fn p2(&self) -> String {
        self.count_paths_with_visits("svr", "out", false, false, &mut HashMap::new())
            .to_string()
    }
}
