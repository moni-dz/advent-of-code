use crate::runner::Solution;
use std::simd::{Simd, num::SimdUint, u64x4};

#[cfg(any(target_feature = "avx512f", target_feature = "avx512dq"))]
use std::simd::u64x8;

macro_rules! v_reduce {
    ($values:expr, $init:expr, $op:tt, $reduce_fn:ident) => {{
        let mut i = 0;

        #[cfg(any(target_feature = "avx512f", target_feature = "avx512dq"))]
        {
            let mut result = u64x8::splat($init);

            while i + 8 <= $values.len() {
                result = result $op Simd::from_slice(&$values[i..i + 8]);
                i += 8;
            }

            let mut acc = result.$reduce_fn();
            let mut result = u64x4::splat($init);

            while i + 4 <= $values.len() {
                result = result $op Simd::from_slice(&$values[i..i + 4]);
                i += 4;
            }

            acc = acc $op result.$reduce_fn();

            return acc $op $values[i..].iter().fold($init, |a, &b| a $op b);
        }

        #[cfg(not(any(target_feature = "avx512f", target_feature = "avx512dq")))]
        {
            let mut result = u64x4::splat($init);

            while i + 4 <= $values.len() {
                result = result $op Simd::from_slice(&$values[i..i + 4]);
                i += 4;
            }

            result.$reduce_fn() $op $values[i..].iter().fold($init, |a, &b| a $op b)
        }
    }};
}

#[derive(Default)]
pub struct TrashCompactor {
    input: String,
}

impl Solution<6> for TrashCompactor {
    fn parse(&mut self, input: &str) {
        self.input = input.to_string();
    }

    fn p1(&self) -> String {
        let mut columns: Vec<Vec<u64>> = Vec::new();
        let mut result = 0;

        for line in self.input.lines() {
            for (i, token) in line.split_ascii_whitespace().enumerate() {
                if columns.len() <= i {
                    columns.push(Vec::new());
                }

                match token {
                    "+" => {
                        result += v_reduce!(columns[i], 0, +, reduce_sum);
                        columns[i].clear();
                    }
                    "*" => {
                        result += v_reduce!(columns[i], 1, *, reduce_product);
                        columns[i].clear();
                    }
                    _ => {
                        if let Ok(val) = token.parse::<u64>() {
                            columns[i].push(val);
                        }
                    }
                }
            }
        }

        result.to_string()
    }

    fn p2(&self) -> String {
        let mut column_numbers: Vec<u64> = Vec::new();
        let mut result = 0;

        for line in self.input.lines() {
            let bytes = line.as_bytes();
            let mut pos = 0;

            while pos < bytes.len() {
                let next_op = memchr::memchr2(b'+', b'*', &bytes[pos..]).map(|i| pos + i).unwrap_or(bytes.len());

                for j in pos..next_op {
                    if column_numbers.len() <= j {
                        column_numbers.resize(j + 1, 0);
                    }

                    let byte = bytes[j];
                    if byte >= b'0' && byte <= b'9' {
                        column_numbers[j] = column_numbers[j] * 10 + (byte - b'0') as u64;
                    }
                }

                if next_op < bytes.len() {
                    let op = bytes[next_op];

                    let values = column_numbers[next_op..].iter().take_while(|&&v| v != 0).copied().collect::<Vec<_>>();

                    match op {
                        b'+' => result += v_reduce!(values, 0, +, reduce_sum),
                        b'*' => result += v_reduce!(values, 1, *, reduce_product),
                        _ => unreachable!(),
                    }

                    pos = next_op + 1;
                } else {
                    break;
                }
            }
        }

        result.to_string()
    }
}
