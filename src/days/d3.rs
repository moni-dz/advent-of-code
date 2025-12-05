use crate::runner::Solution;
use std::simd::{Simd, cmp::SimdPartialEq, num::SimdUint, u8x32};

#[derive(Default)]
pub struct Lobby {
    data: Vec<u8>,
    width: usize,
}

fn max_joltage<const N: usize>(bytes: &[u8]) -> u64 {
    let len = bytes.len();

    if N == 2 {
        let (first, first_pos) = if len > 33 {
            let mut max_digit = 0u8;
            let mut max_pos = 0usize;
            let mut i = 0;

            while i + 32 < len {
                // SAFETY: we already did the bounds checking for this
                let chunk: u8x32 = Simd::from_slice(unsafe { bytes.get_unchecked(i..i + 32) });
                let chunk_max = chunk.reduce_max();

                if chunk_max > max_digit {
                    max_digit = chunk_max;
                    let target: u8x32 = Simd::splat(chunk_max);
                    let mask = chunk.simd_eq(target).to_bitmask();
                    max_pos = i + mask.trailing_zeros() as usize;
                }

                i += 32;
            }

            for j in i..len - 1 {
                if bytes[j] > max_digit {
                    max_digit = bytes[j];
                    max_pos = j;
                }
            }

            (max_digit, max_pos)
        } else {
            // SAFETY: len > 1 for N == 2
            let (pos, val) = unsafe {
                bytes[..len - 1]
                    .iter()
                    .enumerate()
                    .max_by_key(|(_, v)| *v)
                    .unwrap_unchecked()
            };

            (*val, pos)
        };

        let second = if len - first_pos - 1 >= 32 {
            let mut max_digit = 0u8;
            let mut i = first_pos + 1;

            while i + 32 <= len {
                let chunk: u8x32 = Simd::from_slice(unsafe { bytes.get_unchecked(i..i + 32) });
                max_digit = max_digit.max(chunk.reduce_max());
                i += 32;
            }

            for j in i..len {
                max_digit = max_digit.max(bytes[j]);
            }

            max_digit
        } else {
            // SAFETY: first_pos + 1 < len
            *unsafe {
                bytes
                    .get_unchecked(first_pos + 1..)
                    .iter()
                    .max()
                    .unwrap_unchecked()
            }
        };

        (first - b'0') as u64 * 10 + (second - b'0') as u64
    } else {
        let mut result = 0u64;
        let mut start = 0usize;
        let mut end = len - N;

        for _ in 0..N {
            let range_len = end - start + 1;

            let (max_char, max_idx) = if range_len >= 32 {
                let mut max_char = 0u8;
                let mut max_pos = start;
                let mut j = start;

                while j + 32 < end {
                    // SAFETY: we already did the bounds checking for this
                    let chunk: u8x32 = Simd::from_slice(unsafe { bytes.get_unchecked(j..j + 32) });
                    let chunk_max = chunk.reduce_max();

                    if chunk_max > max_char {
                        max_char = chunk_max;
                        let target: u8x32 = Simd::splat(chunk_max);
                        let mask = chunk.simd_eq(target).to_bitmask();
                        max_pos = j + mask.trailing_zeros() as usize;
                    }

                    j += 32;
                }

                for k in j..=end {
                    // SAFETY: k is in range [start, end] and end < len
                    let b = unsafe { *bytes.get_unchecked(k) };

                    if b > max_char {
                        max_char = b;
                        max_pos = k;
                    }
                }

                (max_char, max_pos)
            } else {
                let mut max_char = 0u8;
                let mut max_pos = start;

                for k in start..=end {
                    // SAFETY: k is in range [start, end] and end < len
                    let b = unsafe { *bytes.get_unchecked(k) };

                    if b > max_char {
                        max_char = b;
                        max_pos = k;
                    }
                }

                (max_char, max_pos)
            };

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
