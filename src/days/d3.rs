use crate::runner::Solution;
use std::simd::{Simd, cmp::SimdPartialEq, num::SimdUint, u8x32, u8x4};

#[cfg(any(target_feature = "avx512bw", target_feature = "avx512vl"))]
use std::simd::u8x64;

macro_rules! chunk_max {
    ($bytes:expr, $j:expr, $end:expr, $size:expr, $simd_type:ty, $max_char:expr, $max_pos:expr) => {
        while $j + $size <= $end {
            let chunk: $simd_type = Simd::from_slice(unsafe { $bytes.get_unchecked($j..$j + $size) });
            let chunk_max = chunk.reduce_max();

            if chunk_max > $max_char {
                $max_char = chunk_max;
                let target: $simd_type = Simd::splat(chunk_max);
                let mask = chunk.simd_eq(target).to_bitmask();
                $max_pos = $j + mask.trailing_zeros() as usize;
            }

            $j += $size;
        }
    };
}

/// this is kind of cheating because I know the input size is 100 bytes per line
/// ideally with AVX-512 or SVE we could do 64 + 32 + 4 byte chunks
/// but for AVX2 and NEON, we can do 32 + 32 + 32 + 4 byte chunks
macro_rules! find_max_digit {
    ($bytes:expr, $range_len:expr, $start:expr, $end:expr) => {{
        let mut max_char = 0u8;
        let mut max_pos = $start;
        let mut j = $start;

        #[cfg(any(target_feature = "avx512bw", target_feature = "avx512vl"))]
        {
            if $range_len >= 64 {
                chunk_max!($bytes, j, $end, 64, u8x64, max_char, max_pos);
            }
        }

        if $range_len >= 32 && j + 32 <= $end {
            chunk_max!($bytes, j, $end, 32, u8x32, max_char, max_pos);
        }

        if $range_len >= 4 && j + 4 <= $end {
            chunk_max!($bytes, j, $end, 4, u8x4, max_char, max_pos);
        }

        for k in j..=$end {
            let b = unsafe { *$bytes.get_unchecked(k) };
            if b > max_char {
                max_char = b;
                max_pos = k;
            }
        }

        (max_char, max_pos)
    }};
}

#[derive(Default)]
pub struct Lobby {
    data: Vec<u8>,
    width: usize,
}

fn max_joltage<const N: usize>(bytes: &[u8]) -> u64 {
    let len = bytes.len();

    if N == 2 {
        let (first, first_pos) = find_max_digit!(bytes, len, 0, len - 2);

        let second = {
            let (max_digit, _) = find_max_digit!(bytes, len - first_pos - 1, first_pos + 1, len - 1);
            max_digit
        };

        (first - b'0') as u64 * 10 + (second - b'0') as u64
    } else {
        let mut result = 0u64;
        let mut start = 0usize;
        let mut end = len - N;

        for _ in 0..N {
            let range_len = end - start + 1;

            let (max_char, max_idx) = find_max_digit!(bytes, range_len, start, end);

            result = result * 10 + (max_char - b'0') as u64;
            start = max_idx + 1;
            end += 1;
        }

        result
    }
}

impl Solution<3> for Lobby {
    fn parse(&mut self, input: &str) {
        let bytes = input.as_bytes();

        self.width = bytes
            .iter()
            .position(|&b| b == b'\n' || b == b'\r')
            .unwrap_or(bytes.len());

        self.data = bytes
            .iter()
            .filter(|&&b| b != b'\n' && b != b'\r')
            .copied()
            .collect();
    }

    fn p1(&self) -> String {
        self.data
            .chunks_exact(self.width)
            .map(max_joltage::<2>)
            .sum::<u64>()
            .to_string()
    }

    fn p2(&self) -> String {
        self.data
            .chunks_exact(self.width)
            .map(max_joltage::<12>)
            .sum::<u64>()
            .to_string()
    }
}
