fn main() {
    let input: Vec<&str> = include_str!("input").lines().collect();

    part1(&input);
    part2(&input);
}

fn part1(input: &[&str]) {
    let mut gamma = String::new();
    let mut epsilon = String::new();

    for i in 0..input[0].len() {
        let (zeros, ones) = cnt_zeros_and_ones(input, i);

        if ones > zeros {
            gamma.push('1');
            epsilon.push('0');
        } else {
            gamma.push('0');
            epsilon.push('1');
        }
    }

    let gamma = i128::from_str_radix(&gamma, 2).unwrap();
    let epsilon = i128::from_str_radix(&epsilon, 2).unwrap();

    println!("Part 1: {}", gamma * epsilon);
}

fn part2(input: &[&str]) {
    let oxygen_generator_rating = get_rating(input.to_vec(), true);

    let co2_scrubber_rating = get_rating(input.to_vec(), false);

    let oxygen_generator_rating = i128::from_str_radix(&oxygen_generator_rating, 2).unwrap();
    let co2_scrubber_rating = i128::from_str_radix(&co2_scrubber_rating, 2).unwrap();

    println!("Part 2: {}", oxygen_generator_rating * co2_scrubber_rating);
}

fn get_rating(mut input: Vec<&str>, take_most_common: bool) -> String {
    for i in 0..input[0].len() {
        if input.len() == 1 {
            break;
        }

        let (zeros, ones) = cnt_zeros_and_ones(&input, i);

        if ones >= zeros {
            if take_most_common {
                input.retain(|x| x.chars().nth(i).unwrap() == '1');
            } else {
                input.retain(|x| x.chars().nth(i).unwrap() == '0');
            }
        } else if take_most_common {
            input.retain(|x| x.chars().nth(i).unwrap() == '0');
        } else {
            input.retain(|x| x.chars().nth(i).unwrap() == '1');
        }
    }

    input[0].to_string()
}

pub fn cnt_zeros_and_ones(report: &[&str], ix: usize) -> (usize, usize) {
    let ones = report
        .iter()
        .filter(|c| c.chars().nth(ix).unwrap() == '1')
        .count();

    let zeros = report.len() - ones;

    (zeros, ones)
}
