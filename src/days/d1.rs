use crate::runner::Solution;
use arrayvec::ArrayVec;

#[derive(Clone, Copy)]
struct Command {
    distance: i32,
    direction: i32, // -1 left, 1 right
}

impl Command {
    fn turn(&self) -> i32 {
        self.direction * self.distance
    }
}

pub struct SecretEntrance {
    commands: ArrayVec<Command, 4096>,
}

impl Default for SecretEntrance {
    fn default() -> Self {
        Self { commands: ArrayVec::new() }
    }
}

impl Solution<1> for SecretEntrance {
    fn parse(&mut self, input: &str) {
        let bytes = input.as_bytes();
        self.commands.clear();

        let mut i = 0;
        while i < bytes.len() {
            let direction = if bytes[i] == b'R' { 1 } else { -1 };
            i += 1;

            let mut distance = 0i32;
            while i < bytes.len() && bytes[i] >= b'0' && bytes[i] <= b'9' {
                distance = distance * 10 + (bytes[i] - b'0') as i32;
                i += 1;
            }

            self.commands.push(Command { distance, direction });

            while i < bytes.len() && (bytes[i] == b'\n' || bytes[i] == b'\r') {
                i += 1;
            }
        }
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
                let translated_pos = if cmd.direction == 1 { pos } else { (100 - pos) % 100 };

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
