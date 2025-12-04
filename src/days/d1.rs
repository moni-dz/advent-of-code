use crate::runner::Solution;

#[derive(Clone, Copy)]
struct Command {
    distance: i32,
    direction: i32, // -1 left, 1 right
}

impl Command {
    fn parse(s: &[u8]) -> Self {
        let direction = if s[0] == b'R' { 1 } else { -1 };
        let distance = s[1..]
            .iter()
            .fold(0i32, |acc, &b| acc * 10 + (b - b'0') as i32);
        Self {
            distance,
            direction,
        }
    }

    fn turn(&self) -> i32 {
        self.direction * self.distance
    }
}

#[derive(Default)]
pub struct SecretEntrance {
    commands: Vec<Command>,
}

impl Solution<1> for SecretEntrance {
    fn parse(&mut self, input: &str) {
        self.commands = input
            .as_bytes()
            .split(|&b| b == b'\n' || b == b'\r')
            .filter(|line| !line.is_empty())
            .map(Command::parse)
            .collect();
    }

    fn p1(&self) -> String {
        self.commands
            .iter()
            .fold((50i32, 0u32), |(pos, zeroes), cmd| {
                let new_pos = (pos + cmd.turn()).rem_euclid(100);
                (new_pos, zeroes + (new_pos == 0) as u32)
            })
            .1
            .to_string()
    }

    fn p2(&self) -> String {
        self.commands
            .iter()
            .fold((50i32, 0i32), |(pos, clicks), cmd| {
                let translated_pos = if cmd.direction == 1 {
                    pos
                } else {
                    (100 - pos) % 100
                };

                let new_clicks = if translated_pos == 0 {
                    clicks + cmd.distance / 100
                } else {
                    let first_zero = 100 - translated_pos;
                    if cmd.distance >= first_zero {
                        clicks + 1 + (cmd.distance - first_zero) / 100
                    } else {
                        clicks
                    }
                };

                let new_pos = (pos + cmd.turn()).rem_euclid(100);
                (new_pos, new_clicks)
            })
            .1
            .to_string()
    }
}
