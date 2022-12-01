use std::collections::HashSet;

fn main() {
    let notes: Vec<_> = include_str!("input")
        .lines()
        .map(|line| {
            let line: Vec<&str> = line.splitn(2, " | ").collect();
            let mut ten_digit_unique_signal: Vec<&str> = line[0].split(' ').collect();
            ten_digit_unique_signal.sort_by_key(|x| x.len());

            let four_digit_signal: Vec<&str> = line[1].split(' ').collect();

            (ten_digit_unique_signal, four_digit_signal)
        })
        .collect();

    println!("Part 1: {}", part1(&notes));
    println!("Part 2: {:?}", part2(&notes));
}

fn part1(notes: &[(Vec<&str>, Vec<&str>)]) -> usize {
    notes
        .iter()
        .flat_map(|(_, four_digit_signal)| four_digit_signal.iter().map(|s| s.len()))
        .filter(|l| matches!(l, 2 | 3 | 4 | 7))
        .count()
}

fn part2(notes: &[(Vec<&str>, Vec<&str>)]) -> usize {
    let mut output = vec![];

    for (unique_signals, output_signals) in notes {
        let mut digits = vec![HashSet::<char>::new(); 10];

        for signal in unique_signals {
            match signal.len() {
                2 => digits[1].extend(signal.chars()),
                3 => digits[7].extend(signal.chars()),
                4 => digits[4].extend(signal.chars()),
                7 => digits[8].extend(signal.chars()),
                5 => {
                    let signal: HashSet<char> = signal.chars().collect();
                    // #3 contains #1 fully
                    if signal.intersection(&digits[1]).count() == 2 {
                        digits[3].extend(signal);
                    // #5 contains 3 signals of #4
                    } else if signal.intersection(&digits[4]).count() == 3 {
                        digits[5].extend(signal);
                    } else {
                        digits[2].extend(signal);
                    }
                }
                6 => {
                    let signal: HashSet<char> = signal.chars().collect();
                    // only #9 and #8 contain #4 fully
                    if signal.union(&digits[4]).count() == 6 {
                        digits[9].extend(signal);
                    }
                    // #0 and #9 contain #1 fully, but #9 will finish above
                    else if signal.union(&digits[1]).count() == 6 {
                        digits[0].extend(signal);
                    } else {
                        digits[6].extend(signal);
                    }
                }
                _ => unreachable!(),
            }
        }

        let digit: String = output_signals
            .iter()
            .map(|output_digit| {
                let output_digit = output_digit.chars().collect();
                let (d, _) = digits
                    .iter()
                    .enumerate()
                    .find(|(_, d)| *d == &output_digit)
                    .unwrap();
                d.to_string()
            })
            .collect();

        output.push(digit.parse::<usize>().unwrap());
    }

    output.iter().sum()
}
