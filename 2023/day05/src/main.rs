use std::{
    cmp::{max, min},
    ops::Range,
};

fn main() {
    let mut it = include_str!("example").split("\n\n");

    let seeds: Vec<u64> = it
        .next()
        .map(|s| {
            let (_, seeds) = s.split_once(":").unwrap();
            seeds
                .split_whitespace()
                .map(|n| n.trim().parse().unwrap())
                .collect()
        })
        .unwrap();
    let maps: Vec<_> = it.map(parse_map).collect();

    let mut locations = vec![];
    for seed in &seeds {
        let mut current = *seed;
        for range_mappings in &maps {
            if let Some(c) = range_mappings.iter().find_map(|range| range.get(&current)) {
                current = c;
            }
        }
        locations.push(current);
    }

    let min_location = locations.iter().min().unwrap();

    println!("Part 1: {min_location}");

    let mut locations = vec![];
    let seed_ranges = seeds
        .iter()
        .step_by(2)
        .zip(seeds.iter().skip(1).step_by(2))
        .map(|(&start, &offset)| start..(start + offset));

    for range in seed_ranges {
        let mut current = vec![range];
        for range_mappings in &maps {
            let mut mapped = vec![];

            while let Some(current_range) = current.pop() {
                if let Some((projection, unmapped)) = range_mappings
                    .iter()
                    .find_map(|range| range.map_overlapping(&current_range))
                {
                    mapped.push(projection);
                    current.extend(unmapped);
                } else {
                    mapped.push(current_range);
                }
            }

            current = mapped;
        }

        locations.extend(current);
    }

    let min_location = locations.iter().map(|r| r.start).min().unwrap();
    println!("Part 2: {min_location}");
}

struct MapRange {
    from: Range<u64>,
    to: Range<u64>,
}

impl MapRange {
    fn get(&self, v: &u64) -> Option<u64> {
        if self.from.contains(&v) {
            let from = self.from.clone();
            let to: Range<u64> = self.to.clone();

            return from.zip(to).find_map(|(f, i)| (&f == v).then_some(i));
        }

        None
    }

    fn map_overlapping(&self, r: &Range<u64>) -> Option<(Range<u64>, Vec<Range<u64>>)> {
        let intersection = max(self.from.start, r.start)..min(self.from.end, r.end);

        if intersection.is_empty() {
            return None;
        }

        let projection = (self.to.start + (intersection.start - self.from.start))
            ..(self.to.end
                - (self
                    .from
                    .end
                    .checked_sub(intersection.end)
                    .unwrap_or_default()));

        let mut slack = vec![];
        let left = r.start..self.from.start;
        if !left.is_empty() {
            slack.push(left);
        }

        let right = self.from.end..r.end;
        if !right.is_empty() {
            slack.push(right);
        }

        Some((projection, slack))
    }
}

fn parse_map(s: &str) -> Vec<MapRange> {
    let mut ranges = vec![];
    for l in s.lines().skip(1) {
        let mut nums = l
            .split_whitespace()
            .map(|n| n.trim().parse::<u64>().unwrap());

        let right = nums.next().unwrap();
        let left = nums.next().unwrap();
        let offset = nums.next().unwrap();

        ranges.push(MapRange {
            from: left..left + offset,
            to: right..right + offset,
        })
    }

    ranges
}

pub enum Intersection<T> {
    Inside(Range<T>),
    Slack {
        left: Range<T>,
        intersection: Range<T>,
    },
    Overlapping {
        left: Range<T>,
        intersection: Range<T>,
        right: Range<T>,
    },
}

#[cfg(test)]
mod tests {
    use crate::MapRange;

    #[test]
    fn test_intersect() {
        let x = MapRange {
            from: 53..61,
            to: 49..57,
        };

        match x.map_overlapping(&(57..80)) {
            Some((i, v)) => {
                assert_eq!(53..57, i);
                dbg!(&v);
                assert!(v.is_empty());
            }
            None => todo!(),
        }
    }
}
