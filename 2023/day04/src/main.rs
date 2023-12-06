use std::collections::{HashMap, HashSet};

fn main() {
    let cards: Vec<(usize, usize)> = include_str!("example")
        .lines()
        .enumerate()
        .map(|(i, line)| {
            let (_, cards) = line.split_once(':').unwrap();
            let (winning, other) = cards.split_once('|').unwrap();
            let winning: HashSet<&str> = winning.split_whitespace().collect();
            let other: HashSet<&str> = other.split_whitespace().collect();

            let cnt = winning.intersection(&other).count();

            (i + 1, cnt)
        })
        .collect();

    let total: u64 = cards
        .iter()
        .map(|(_, cnt)| cnt.checked_sub(1).map(|c| 1 << c).unwrap_or(0))
        .sum();

    println!("Part 1: {total}");

    let mut counter = HashMap::new();

    for (i, matches) in cards.into_iter() {
        *counter.entry(i).or_default() += 1;

        for j in 1..=matches {
            *counter.entry(i + j).or_default() += counter[&i];
        }
    }

    let total: usize = counter.values().sum();

    println!("Part 2: {total}");
}
