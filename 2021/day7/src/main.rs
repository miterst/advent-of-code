fn main() {
    let horizontal_positions: Vec<isize> = include_str!("input")
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();
    println!(
        "Part 1: {}",
        calculate_min_cost(&horizontal_positions, std::convert::identity)
    );
    println!(
        "Part 2: {}",
        calculate_min_cost(&horizontal_positions, |steps| steps
            + (((steps - 1) * steps) >> 1))
    );
}

fn calculate_min_cost(horizontal_positions: &[isize], fuel_cost_fn: fn(isize) -> isize) -> isize {
    let max_position = *horizontal_positions.iter().max().unwrap() as usize;
    let mut costs: Vec<isize> = vec![0; max_position + 1];

    for position in 1..=max_position {
        for j in horizontal_positions.iter() {
            let steps = ((position as isize) - j).abs();
            costs[position] += fuel_cost_fn(steps);
        }
    }

    costs.into_iter().filter(|x| x.is_positive()).min().unwrap()
}
