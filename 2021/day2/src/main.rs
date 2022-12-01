fn main() {
    let commands: Vec<(&str, i32)> = include_str!("input")
        .lines()
        .map(|line| {
            let line: Vec<&str> = line.splitn(2, ' ').collect();
            let cmd = line[0];
            let value = line[1].parse().unwrap();
            (cmd, value)
        })
        .collect();

    let (hpos, depth) = commands
        .iter()
        .fold((0, 0), |(mut hpos, mut depth), (cmd, value)| {
            match *cmd {
                "down" => depth += value,
                "up" => depth -= value,
                _forward => hpos += value,
            }
            (hpos, depth)
        });

    println!("Part1: {}", hpos * depth);

    let (hpos, depth, _) =
        commands
            .iter()
            .fold((0, 0, 0), |(mut hpos, mut depth, mut aim), (cmd, value)| {
                match *cmd {
                    "down" => aim += value,
                    "up" => aim -= value,
                    _forward => {
                        hpos += value;
                        depth += aim * value;
                    }
                }
                (hpos, depth, aim)
            });

    println!("Part2: {}", hpos * depth);
}
