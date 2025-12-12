use crate::runner::Solution;

struct Shape {
    cells: Vec<(usize, usize)>,
}

impl Shape {
    fn new(lines: &[&str]) -> Self {
        let mut cells: Vec<_> = lines
            .iter()
            .enumerate()
            .flat_map(|(r, line)| {
                line.chars()
                    .enumerate()
                    .filter(|(_, ch)| *ch == '#')
                    .map(move |(c, _)| (r, c))
            })
            .collect();

        if cells.is_empty() {
            return Shape { cells };
        }

        let min_r = cells.iter().map(|&(r, _)| r).min().unwrap();
        let min_c = cells.iter().map(|&(_, c)| c).min().unwrap();

        cells.iter_mut().for_each(|(r, c)| {
            *r -= min_r;
            *c -= min_c;
        });

        cells.sort_unstable();

        Shape { cells }
    }
}

struct Region {
    width: usize,
    height: usize,
    requirements: Vec<usize>,
}

impl Region {
    fn new(width: usize, height: usize, requirements: Vec<usize>) -> Self {
        Region {
            width,
            height,
            requirements,
        }
    }

    fn area(&self) -> usize {
        self.width * self.height
    }

    fn cells_needed(&self, shapes: &[Shape]) -> usize {
        self.requirements
            .iter()
            .enumerate()
            .filter(|&(ref idx, &qty)| qty > 0 && *idx < shapes.len())
            .map(|(idx, &qty)| shapes[idx].cells.len() * qty)
            .sum()
    }
}

#[derive(Default)]
pub struct ChristmasTreeFarm {
    shapes: Vec<Shape>,
    regions: Vec<Region>,
}

impl Solution<12> for ChristmasTreeFarm {
    fn parse(&mut self, input: &str) {
        let mut lines = input.lines().peekable();

        while let Some(&line) = lines.peek() {
            if line.is_empty() {
                lines.next();
                break;
            }

            if line.ends_with(':') {
                lines.next();

                let mut grid = Vec::new();

                while let Some(&next_line) = lines.peek() {
                    if next_line.is_empty() || next_line.ends_with(':') {
                        break;
                    }

                    grid.push(lines.next().unwrap());
                }

                if !grid.is_empty() {
                    self.shapes.push(Shape::new(&grid));
                }

                if lines.peek().map_or(false, |l| l.is_empty()) {
                    lines.next();
                }
            } else {
                break;
            }
        }

        for line in lines {
            if !line.is_empty() {
                if let Some((dims, counts)) = line.split_once(": ") {
                    let mut d = dims.split('x');

                    if let (Ok(width), Ok(height)) = (
                        d.next().unwrap_or("0").parse::<usize>(),
                        d.next().unwrap_or("0").parse::<usize>(),
                    ) {
                        let requirements: Vec<usize> = counts
                            .split_whitespace()
                            .map(|s| s.parse().unwrap_or(0))
                            .collect();

                        self.regions.push(Region::new(width, height, requirements));
                    }
                }
            }
        }
    }

    fn p1(&self) -> String {
        self.regions
            .iter()
            .filter(|region| region.cells_needed(&self.shapes) <= region.area())
            .count()
            .to_string()
    }

    fn p2(&self) -> String {
        "n/a".to_string()
    }
}
