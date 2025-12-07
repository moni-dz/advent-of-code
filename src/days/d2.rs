use crate::runner::Solution;

type Interval = (u64, u64);

#[derive(Default)]
pub struct GiftShop {
    ranges: Vec<Interval>,
}

#[inline]
fn count_digits(num: u64) -> u32 {
    num.ilog10() + 1
}

#[inline]
fn get_prefix(num: u64, p_len: u32) -> u64 {
    let digits = count_digits(num);
    if p_len >= digits { num } else { num / 10u64.pow(digits - p_len) }
}

fn repeat_pattern(pattern: u64, pattern_len: u32, target_len: u32) -> u64 {
    let multiplier_step = 10u64.pow(pattern_len);
    let mut result = 0u64;
    let mut multiplier = 1u64;

    for _ in (0..target_len).step_by(pattern_len as usize) {
        result += pattern * multiplier;
        multiplier *= multiplier_step;
    }

    result
}

fn invalids_for_range<const PART: u8>((range_b, range_e): Interval) -> u64 {
    let len_b = count_digits(range_b);
    let len_e = count_digits(range_e);

    let mut invalid_ids: Vec<u64> = Vec::new();

    for len in len_b..=len_e {
        if PART == 1 && len % 2 != 0 {
            continue;
        }

        let sub_b = if len == len_b { range_b } else { 10u64.pow(len - 1) };
        let sub_e = if len == len_e { range_e } else { 10u64.pow(len) - 1 };

        if PART == 1 {
            let half_len = len / 2;
            let half_b = get_prefix(sub_b, half_len);
            let half_e = get_prefix(sub_e, half_len);
            let multiplier = 10u64.pow(half_len);

            for half in half_b..=half_e {
                let repeated = half * multiplier + half;
                if repeated >= sub_b && repeated <= sub_e {
                    invalid_ids.push(repeated);
                }
            }
        } else {
            for pattern_len in 1..len {
                if len % pattern_len != 0 {
                    continue;
                }

                let pattern_b = get_prefix(sub_b, pattern_len);
                let pattern_e = get_prefix(sub_e, pattern_len);

                for pattern in pattern_b..=pattern_e {
                    let repeated = repeat_pattern(pattern, pattern_len, len);
                    if repeated >= sub_b && repeated <= sub_e {
                        invalid_ids.push(repeated);
                    }
                }
            }
        }
    }

    invalid_ids.sort_unstable();
    invalid_ids.dedup();
    invalid_ids.into_iter().sum()
}

fn get_interval(s: &str) -> Option<Interval> {
    let (a, b) = s.split_once('-')?;
    Some((a.parse().ok()?, b.parse().ok()?))
}

impl Solution<2> for GiftShop {
    fn parse(&mut self, input: &str) {
        self.ranges = input.split(',').filter_map(|s| get_interval(s.trim())).collect();
    }

    fn p1(&self) -> String {
        self.ranges.iter().map(|&r| invalids_for_range::<1>(r)).sum::<u64>().to_string()
    }

    fn p2(&self) -> String {
        self.ranges.iter().map(|&r| invalids_for_range::<2>(r)).sum::<u64>().to_string()
    }
}
