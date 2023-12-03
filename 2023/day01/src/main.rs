use std::char;

fn main() {
    part1();
    part2();
}

fn part1() {
    let calibration: u32 = include_str!("./example1")
        .lines()
        .map(|line| {
            let mut it = line.chars().filter_map(|c| char::to_digit(c, 10));
            let leftmost = it.next().unwrap();
            let rightmost = it.next_back().unwrap_or(leftmost);

            10 * leftmost + rightmost
        })
        .sum();

    println!("[Part1] Calibration value: {calibration}");
}

fn part2() {
    let replace_words = |line: &str| {
        line.replace("one", "o1e")
            .replace("two", "t2o")
            .replace("three", "th3e")
            .replace("four", "4")
            .replace("five", "5e")
            .replace("six", "6")
            .replace("seven", "7n")
            .replace("eight", "e8t")
            .replace("nine", "n9e")
    };

    let calibration: u32 = include_str!("./example2")
        .lines()
        .map(replace_words)
        .map(|line| {
            let mut it = line.chars().filter_map(|c| char::to_digit(c, 10));
            let leftmost = it.next().unwrap();
            let rightmost = it.next_back().unwrap_or(leftmost);

            10 * leftmost + rightmost
        })
        .sum();

    println!("[Part2] Calibration value: {calibration}");
}
