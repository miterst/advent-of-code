enum Fold {
    Up(usize),
    Left(usize),
}

fn main() {
    let input: Vec<&str> = include_str!("input").split("\n\n").collect();

    let points: Vec<(usize, usize)> = input[0]
        .lines()
        .map(|line| {
            let point: Vec<usize> = line.split(',').map(|x| x.parse().unwrap()).collect();
            (point[0], point[1])
        })
        .collect();

    let instructions: Vec<Fold> = input[1]
        .lines()
        .map(|line| {
            let instruction: Vec<&str> = line.split(' ').last().unwrap().split('=').collect();
            match instruction.as_slice() {
                ["y", value] => Fold::Up(value.parse().unwrap()),
                ["x", value] => Fold::Left(value.parse().unwrap()),
                _ => unreachable!(),
            }
        })
        .collect();

    let x_max = points.iter().max_by_key(|(x, _)| x).unwrap().0;
    let y_max = points.iter().max_by_key(|(_, y)| y).unwrap().1;

    let mut paper = vec![vec![0; x_max + 1]; y_max + 1];

    for (x, y) in points {
        paper[y][x] = 1;
    }

    let paper = apply_instructions(&instructions[..1], paper);
    println!(
        "Part 1: {}",
        paper
            .iter()
            .map(|dots| dots.iter().sum::<i32>())
            .sum::<i32>()
    );

    let paper = apply_instructions(&instructions[1..], paper);
    println!("Part 2:");
    print_paper(&paper);
}

fn print_paper(paper: &[Vec<i32>]) {
    for line in paper {
        println!(
            "{}",
            line.iter()
                .map(|x| if *x == 0 { ' ' } else { '#' })
                .collect::<String>()
        );
    }
}

fn apply_instructions(instructions: &[Fold], mut paper: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    let mut rows = paper.len();
    let mut cols = paper[0].len();

    for instruction in instructions {
        match instruction {
            Fold::Up(value) => {
                for (y_up, y_down) in (0..*value).rev().zip(value + 1..rows) {
                    for x in 0..cols {
                        paper[y_up][x] |= paper[y_down][x];
                    }
                }
                rows = *value;
            }
            Fold::Left(value) => {
                for y in 0..rows {
                    for (x_left, x_right) in (0..*value).rev().zip(value + 1..cols) {
                        paper[y][x_left] |= paper[y][x_right];
                    }
                }

                cols = *value;
            }
        }
    }

    paper.truncate(rows);
    paper.iter_mut().for_each(|p| p.truncate(cols));

    paper
}
