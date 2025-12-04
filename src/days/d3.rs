use crate::runner::Solution;

#[derive(Default)]
pub struct Lobby {
    data: Vec<u8>,
    width: usize,
}

fn max_joltage<const N: usize>(bytes: &[u8]) -> u64 {
    if N == 2 {
        bytes[..bytes.len() - 1]
            .iter()
            .rev()
            .fold((0u64, bytes[bytes.len() - 1]), |(max_val, max_seen), x| {
                let x = *x;
                let candidate = (x - b'0') as u64 * 10 + (max_seen - b'0') as u64;
                (max_val.max(candidate), max_seen.max(x))
            })
            .0
    } else {
        let (result, _, _) = (0..N).fold(
            (0u64, 0usize, bytes.len() - N),
            |(result, start, end), _| {
                let max_char = *bytes[start..=end].iter().max().unwrap();
                let max_idx = start
                    + bytes[start..=end]
                        .iter()
                        .position(|&b| b == max_char)
                        .unwrap();
                (result * 10 + (max_char - b'0') as u64, max_idx + 1, end + 1)
            },
        );
        result
    }
}

impl Solution<3> for Lobby {
    fn parse(&mut self, input: &str) {
        let mut lines = input.lines().filter(|s| !s.is_empty()).peekable();
        self.width = lines.peek().map_or(0, |s| s.len());
        self.data = lines.flat_map(|s| s.as_bytes()).copied().collect();
    }

    fn p1(&self) -> String {
        self.data
            .chunks(self.width)
            .map(|line| max_joltage::<2>(line))
            .sum::<u64>()
            .to_string()
    }

    fn p2(&self) -> String {
        self.data
            .chunks(self.width)
            .map(|line| max_joltage::<12>(line))
            .sum::<u64>()
            .to_string()
    }
}
