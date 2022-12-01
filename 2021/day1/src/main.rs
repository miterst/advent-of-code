mod part1;
mod part2;

fn main() {
    let measurements: Vec<_> = include_str!("input")
        .lines()
        .filter_map(|m| m.parse().ok())
        .collect();

    println!(
        "Part1: {}",
        part1::cnt_increasing_measurements(&measurements)
    );
    println!(
        "Part2: {}",
        part2::cnt_increasing_measurements(&measurements)
    );
}
