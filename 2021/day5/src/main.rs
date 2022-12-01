use std::cmp::{max, min};
use std::collections::HashMap;
use std::str::FromStr;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl From<(isize, isize)> for Point {
    fn from((x, y): (isize, isize)) -> Self {
        Self { x, y }
    }
}

impl FromStr for Point {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<isize> = s
            .splitn(2, ',')
            .map(|x| x.parse())
            .collect::<Result<_, _>>()?;
        Ok((coords[0], coords[1]).into())
    }
}

struct LineSegment(Point, Point);

impl LineSegment {
    fn is_vertical(&self) -> bool {
        self.0.x == self.1.x
    }

    fn is_horizontal(&self) -> bool {
        self.0.y == self.1.y
    }

    fn is_diagonal(&self) -> bool {
        (self.0.x - self.1.x).abs() == (self.0.y - self.1.y).abs()
    }

    fn points(&self) -> Vec<Point> {
        let mut v = vec![];

        if self.is_diagonal() {
            let x1 = self.0.x;
            let x2 = self.1.x;

            let y1 = self.0.y;
            let y2 = self.1.y;

            for i in 0..=(x2 - x1) {
                let x = x1 + i;
                let y = y1 + if y2 - y1 >= 0 { i } else { -i };

                v.push((x, y).into())
            }

            return v;
        }

        if self.is_horizontal() || self.is_vertical() {
            let x1 = self.0.x;
            let x2 = self.1.x;

            let y1 = min(self.0.y, self.1.y);
            let y2 = max(self.0.y, self.1.y);

            for x in x1..=x2 {
                for y in y1..=y2 {
                    v.push((x, y).into())
                }
            }

            return v;
        }

        v
    }
}

impl FromStr for LineSegment {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut points: Vec<Point> = s
            .splitn(2, " -> ")
            .map(|x| x.parse())
            .collect::<Result<_, _>>()?;
        points.sort();
        let p2 = points.pop().unwrap();
        let p1 = points.pop().unwrap();

        Ok(Self(p1, p2))
    }
}

fn main() {
    let segments: Vec<LineSegment> = include_str!("input")
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();

    let mapping: HashMap<Point, usize> = segments
        .iter()
        .filter(|segment| !segment.is_diagonal())
        .fold(HashMap::new(), |mut acc, segment| {
            for point in segment.points() {
                let e = acc.entry(point).or_insert(0);
                *e += 1;
            }

            acc
        });

    let overlapping: usize = mapping.values().filter(|x| **x >= 2).count();
    println!("Part 1: {}", overlapping);

    let mapping: HashMap<Point, usize> =
        segments.iter().fold(HashMap::new(), |mut acc, segment| {
            for point in segment.points() {
                let e = acc.entry(point).or_insert(0);
                *e += 1;
            }

            acc
        });

    let overlapping: usize = mapping.values().filter(|x| **x >= 2).count();
    println!("Part 2: {}", overlapping);
}
