use crate::runner::Solution;

#[derive(Default)]
pub struct Cafeteria {
    ids: Vec<u64>,
    merged: Vec<(u64, u64)>,
}

impl Solution<5> for Cafeteria {
    fn parse(&mut self, input: &str) {
        #[cfg(not(target_os = "windows"))]
        let (ranges_str, ids_str) = input.split_once("\n\n").unwrap();

        #[cfg(target_os = "windows")]
        let (ranges_str, ids_str) = input.split_once("\r\n\r\n").unwrap();

        self.ids = ids_str
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| line.parse().unwrap())
            .collect();

        self.ids.sort_unstable();

        let mut ranges: Vec<(u64, u64)> = ranges_str
            .lines()
            .map(|line| {
                let (b, e) = line.split_once('-').unwrap();
                (b.parse().unwrap(), e.parse().unwrap())
            })
            .collect();

        ranges.sort_unstable();
        self.merged.reserve(ranges.len());

        for (start, end) in ranges {
            match self.merged.last_mut() {
                Some((_, max_upper)) if start <= *max_upper + 1 => {
                    *max_upper = (*max_upper).max(end);
                }
                _ => self.merged.push((start, end)),
            }
        }
    }

    fn p1(&self) -> String {
        self.ids
            .iter()
            .filter(
                |id| match self.merged.binary_search_by_key(*id, |(start, _)| *start) {
                    Ok(_) => true,
                    Err(pos) => pos > 0 && *id <= &self.merged[pos - 1].1,
                },
            )
            .count()
            .to_string()
    }

    fn p2(&self) -> String {
        self.merged
            .iter()
            .map(|(start, end)| end - start + 1)
            .sum::<u64>()
            .to_string()
    }
}
