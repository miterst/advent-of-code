fn main() {
    let race_data: Vec<Vec<i64>> = include_str!("example")
        .lines()
        .map(|l| {
            let (_, vals) = l.split_once(":").unwrap();
            vals.trim()
                .split_whitespace()
                .map(|n| n.parse().unwrap())
                .collect()
        })
        .collect();

    let total = num_ways_total(&race_data[0], &race_data[1]);

    println!("Part 1: {total}");

    let race_data: Vec<i64> = include_str!("example")
        .lines()
        .map(|l| {
            let (_, vals) = l.split_once(":").unwrap();
            let s: String = vals.trim().split_whitespace().collect();
            s.parse().unwrap()
        })
        .collect();

    let total = num_ways_total(&[race_data[0]], &[race_data[1]]);

    println!("Part 2: {total}");
}

fn num_ways_total(time: &[i64], distance: &[i64]) -> i64 {
    let mut total = 1;
    for (time, distance) in time.iter().zip(distance.iter()) {
        let initial = distance / time;

        let mut ways = 0;

        for hold_for in initial..=(time - initial) {
            if distance - (time - hold_for) * hold_for < 0 {
                ways += 1;
            }
        }

        total *= ways;
    }

    total
}
