use std::collections::HashSet;

use crate::runner::Solution;
use good_lp::{
    Expression, Solution as LpSolution, SolverModel, constraint, microlp, variable, variables,
};
use nalgebra::DMatrix;

#[derive(Default)]
pub struct Factory {
    machines: Vec<Machine>,
}

struct Machine {
    target: Vec<bool>,
    buttons: Vec<Vec<usize>>,
    joltage: Vec<usize>,
}

impl Solution<10> for Factory {
    fn parse(&mut self, input: &str) {
        for line in input.lines().filter(|l| !l.is_empty()) {
            let target = line[1..line.find(']').unwrap()]
                .chars()
                .map(|c| c == '#')
                .collect();

            let buttons = line
                .split('(')
                .skip(1)
                .map(|part| {
                    part.split(')')
                        .next()
                        .unwrap()
                        .split(',')
                        .map(|s| s.trim().parse().unwrap())
                        .collect()
                })
                .collect();

            let joltage = {
                let start = line.find('{').unwrap();
                let end = line.find('}').unwrap();

                line[start + 1..end]
                    .split(',')
                    .map(|s| s.trim().parse().unwrap())
                    .collect()
            };

            self.machines.push(Machine {
                target,
                buttons,
                joltage,
            });
        }
    }

    fn p1(&self) -> String {
        let mut total = 0;
        for machine in &self.machines {
            let n_lights = machine.target.len();
            let n_buttons = machine.buttons.len();

            let mut matrix = DMatrix::<u32>::zeros(n_lights, n_buttons + 1);

            for (b_idx, button) in machine.buttons.iter().enumerate() {
                for &l_idx in button {
                    if l_idx < n_lights {
                        matrix[(l_idx, b_idx)] = 1;
                    }
                }
            }

            for (l_idx, &target) in machine.target.iter().enumerate() {
                matrix[(l_idx, n_buttons)] = if target { 1 } else { 0 };
            }

            let mut pivot_col = vec![None; n_lights];
            let mut current_row = 0;

            for col in 0..n_buttons {
                let mut pivot_row = None;

                for row in current_row..n_lights {
                    if matrix[(row, col)] > 0 {
                        pivot_row = Some(row);
                        break;
                    }
                }

                let pivot_row = match pivot_row {
                    Some(r) => r,
                    None => continue,
                };

                for c in 0..=n_buttons {
                    let temp = matrix[(current_row, c)];
                    matrix[(current_row, c)] = matrix[(pivot_row, c)];
                    matrix[(pivot_row, c)] = temp;
                }

                pivot_col[current_row] = Some(col);

                for row in 0..n_lights {
                    if row != current_row && matrix[(row, col)] > 0 {
                        for c in 0..=n_buttons {
                            matrix[(row, c)] ^= matrix[(current_row, c)];
                        }
                    }
                }

                current_row += 1;
            }

            let mut pivot_cols_set = HashSet::new();

            for opt_col in &pivot_col {
                if let Some(col) = opt_col {
                    pivot_cols_set.insert(*col);
                }
            }

            let free_vars: Vec<usize> = (0..n_buttons)
                .filter(|i| !pivot_cols_set.contains(i))
                .collect();

            let mut min_presses = usize::MAX;

            for mask in 0..(1 << free_vars.len()) {
                let mut presses = vec![false; n_buttons];

                for (i, &free_var) in free_vars.iter().enumerate() {
                    presses[free_var] = (mask & (1 << i)) != 0;
                }

                for row in (0..n_lights).rev() {
                    if let Some(col) = pivot_col[row] {
                        let mut val = matrix[(row, n_buttons)] > 0;

                        for c in 0..n_buttons {
                            if c != col && matrix[(row, c)] > 0 && presses[c] {
                                val = !val;
                            }
                        }

                        presses[col] = val;
                    }
                }

                let mut valid = true;

                for row in current_row..n_lights {
                    let mut sum = false;

                    for c in 0..n_buttons {
                        if matrix[(row, c)] > 0 && presses[c] {
                            sum = !sum;
                        }
                    }

                    if sum != (matrix[(row, n_buttons)] > 0) {
                        valid = false;
                        break;
                    }
                }

                if valid {
                    let count = presses.iter().filter(|&&p| p).count();
                    min_presses = min_presses.min(count);
                }
            }

            total += min_presses;
        }

        total.to_string()
    }

    fn p2(&self) -> String {
        let mut total = 0usize;

        for machine in &self.machines {
            let n_buttons = machine.buttons.len();
            let targets = &machine.joltage;
            let mut vars = variables!();

            let button_vars: Vec<_> = (0..n_buttons)
                .map(|_| vars.add(variable().integer().min(0)))
                .collect();

            let mut model = vars
                .minimise(
                    button_vars
                        .iter()
                        .cloned()
                        .fold(Expression::from(0), |acc, v| acc + v),
                )
                .using(microlp);

            for (c_idx, &target) in targets.iter().enumerate() {
                let mut sum = Expression::from(0);

                for (button_idx, button) in machine.buttons.iter().enumerate() {
                    if button.contains(&c_idx) {
                        sum = sum + button_vars[button_idx];
                    }
                }

                model = model.with(constraint!(sum == target as f64));
            }

            match model.solve() {
                Ok(solution) => {
                    let mut result = 0usize;

                    for var in &button_vars {
                        result += solution.value(*var).round() as usize;
                    }

                    total += result;
                }
                Err(_) => {}
            }
        }

        total.to_string()
    }
}
