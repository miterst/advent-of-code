use std::collections::HashSet;

fn main() {
    let part_numbers = part_numbers_points(include_str!("example"));
    let symbols = symbol_points(include_str!("example"));

    let points: HashSet<_> = symbols.iter().flat_map(|(_, s)| s.clone()).collect();

    let part_numbers_total: u64 = part_numbers
        .iter()
        .filter_map(|(number, coords)| {
            points
                .intersection(&coords)
                .next()
                .is_some()
                .then_some(number.parse::<u64>().unwrap())
        })
        .sum();

    println!("Part 1: {part_numbers_total:?}");

    let mut gear_ratios = vec![];
    for (_, s) in symbols {
        let mut neighbours = vec![];
        for (number, points) in &part_numbers {
            if s.intersection(&points).next().is_some() {
                neighbours.push(number.to_string());
            }
        }
        if neighbours.len() == 2 {
            gear_ratios.push(neighbours);
        }
    }

    let gear_ratios: u64 = gear_ratios
        .into_iter()
        .map(|r| {
            r.into_iter()
                .map(|n| n.parse::<u64>().unwrap())
                .product::<u64>()
        })
        .sum();

    println!("Part 2: {gear_ratios:?}");
}

fn part_numbers_points(input: &str) -> Vec<(String, HashSet<(usize, usize)>)> {
    let mut part_numbers_points: Vec<(String, HashSet<(usize, usize)>)> = vec![];

    for (row, line) in input.lines().enumerate() {
        let line_it = &mut line.char_indices().peekable();

        while line_it.peek().is_some() {
            _ = line_it.skip_while(|(_, chr)| !chr.is_numeric());

            let mut number_it = line_it.take_while(|(_, chr)| chr.is_numeric()).peekable();
            if number_it.peek().is_some() {
                let (number, coordinates): (String, HashSet<(usize, usize)>) =
                    number_it.map(|(col, chr)| (chr, (row, col))).unzip();

                part_numbers_points.push((number, coordinates))
            }
        }
    }

    part_numbers_points
}

fn symbol_points(input: &str) -> Vec<(char, HashSet<(usize, usize)>)> {
    let mut points: Vec<(char, HashSet<(usize, usize)>)> = vec![];

    for (row, line) in input.lines().enumerate() {
        let symbols = line
            .char_indices()
            .filter(|(_, chr)| !chr.is_numeric() && *chr != '.');
        for (col, symbol) in symbols {
            let mut pts: HashSet<(usize, usize)> = HashSet::new();

            for i in -1..=1 {
                for j in -1..=1 {
                    if i == j && i == 0 {
                        continue;
                    }

                    let row = row.checked_add_signed(i as isize);
                    let col = col.checked_add_signed(j as isize);
                    if let Some(ix) = row.zip(col) {
                        pts.insert(ix);
                    }
                }
            }

            points.push((symbol, pts));
        }
    }

    points
}
