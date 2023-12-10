use std::collections::HashMap;

fn main() {
    let (instructions, nodes) = include_str!("example").split_once("\n\n").unwrap();

    let instructions = instructions;

    let network: HashMap<_, _> = nodes
        .lines()
        .map(|line| {
            let (node, neighbours) = line.split_once(" = ").unwrap();
            let neighbours: Vec<&str> = neighbours
                .trim_matches(|c| c == '(' || c == ')')
                .split(", ")
                .collect();

            (node, neighbours)
        })
        .collect();

    part1(instructions, &network);
    part2(instructions, &network);
}

fn part1(instructions: &str, network: &HashMap<&str, Vec<&str>>) {
    let mut steps = 0;
    let mut node = "AAA";
    for instruction in instructions.chars().cycle() {
        let i = if instruction == 'L' { 0 } else { 1 };
        node = network[node][i];
        steps += 1;
        if node == "ZZZ" {
            break;
        }
    }

    println!("Part 1: {steps}");
}

fn part2(instructions: &str, network: &HashMap<&str, Vec<&str>>) {
    let mut nodes = network
        .keys()
        .filter_map(|k| k.ends_with('A').then_some((k, 0)))
        .collect::<Vec<_>>();

    let mut steps_to_end_in_z = vec![];

    for instruction in instructions.chars().cycle() {
        let i = if instruction == 'L' { 0 } else { 1 };

        let (end_in_z, other): (Vec<_>, Vec<_>) =
            nodes.iter().partition(|(node, _)| node.ends_with('Z'));

        steps_to_end_in_z.extend(end_in_z.into_iter().map(|(_, steps)| steps));

        nodes = other
            .into_iter()
            .map(|(node, steps)| (&network[node][i], steps + 1))
            .collect();

        if nodes.is_empty() {
            break;
        }
    }

    let lcm = steps_to_end_in_z
        .into_iter()
        .reduce(|a, b| a * b / gdc(a, b))
        .unwrap();

    println!("Part 2: {lcm:?}");
}

fn gdc(mut m: u64, mut n: u64) -> u64 {
    while n != 0 {
        (m, n) = (n, m % n);
    }

    m
}
