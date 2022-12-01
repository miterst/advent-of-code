fn main() {
    let initial_state: Vec<usize> = include_str!("input")
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    println!("Part 1: {}", count_lanternfish(&initial_state, 80));
    println!("Part 2: {}", count_lanternfish(&initial_state, 256));
}

fn count_lanternfish(initial_state: &[usize], days: usize) -> u64 {
    let mut timers = vec![0; 9];

    for i in initial_state {
        timers[*i] += 1;
    }

    for _day in 1..=days {
        timers.rotate_left(1);
        timers[6] += timers[8];
    }

    timers.iter().sum()
}
