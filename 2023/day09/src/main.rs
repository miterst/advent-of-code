fn main() {
    let histories: Vec<Vec<i128>> = include_str!("example")
        .lines()
        .map(|line| line.split(' ').map(|i| i.parse().unwrap()).collect())
        .collect();

    part1(histories.clone());
    part2(histories);
}

fn part1(histories: Vec<Vec<i128>>) {
    let mut extrapolated = vec![];

    for mut history in histories {
        loop {
            let end = history.len();
            extrapolated.push(history[end - 1]);
            history = history.windows(2).map(|w| w[1] - w[0]).collect();

            if history.iter().all(|x| x == &0) {
                break;
            }
        }
    }

    let total: i128 = extrapolated.iter().sum();
    println!("Part 1: {total}");
}

fn part2(histories: Vec<Vec<i128>>) {
    let mut total = 0;
    let mut extrapolated = vec![];

    for mut history in histories {
        loop {
            extrapolated.push(history[0]);
            history = history.windows(2).map(|w| w[1] - w[0]).collect();

            if history.iter().all(|x| x == &0) {
                break;
            }
        }

        total += extrapolated.drain(..).rev().reduce(|a, b| b - a).unwrap()
    }

    println!("Part 2: {total}");
}
