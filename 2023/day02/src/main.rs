use std::collections::HashMap;

fn main() {
    let cubes = HashMap::from([("red", 12), ("green", 13), ("blue", 14)]);

    let games: Vec<(u32, _)> = include_str!("example")
        .lines()
        .map(|line| {
            let (game, reveals) = line.split_once(':').unwrap();
            let game = game.split_once(' ').unwrap().1.parse().unwrap();

            let mut cnt_by_color: HashMap<&str, Vec<u64>> = HashMap::new();

            for reveal in reveals.trim().split(';').flat_map(|l| l.split(',')) {
                let (count, color) = reveal.trim().split_once(' ').unwrap();

                let count = count.parse().unwrap();

                cnt_by_color.entry(color).or_default().push(count);
            }

            (game, cnt_by_color)
        })
        .collect();

    let total: u32 = games
        .iter()
        .filter_map(|(id, game)| {
            for (color, cnts) in game {
                if cnts.iter().max()? > &cubes[color] {
                    return None;
                }
            }

            Some(id)
        })
        .sum();

    println!("[Part 1]: {total}");

    let total: u64 = games
        .iter()
        .map(|(_, game)| {
            game.values()
                .filter_map(|v| v.iter().max())
                .product::<u64>()
        })
        .sum();

    println!("[Part 2]: {total}");
}
