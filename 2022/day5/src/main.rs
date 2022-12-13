use itertools::Itertools;

struct Move {
    amount: usize,
    from: usize,
    to: usize,
}

fn parse_moves(moves: &str) -> Vec<Move> {
    moves
        .lines()
        .map(|line| {
            let (amount, from, to) = line
                .split_whitespace()
                .filter_map(|s| usize::from_str_radix(s, 10).ok())
                .collect_tuple()
                .unwrap();
            
            Move {
                amount,
                from: from - 1,
                to: to - 1,
            }
        })
        .collect()
}

fn parse_stacks(stacks_str: &str) -> Vec<Vec<char>> {
    let mut stacks_rows = stacks_str.lines();

    let stacks_cnt = stacks_rows
        .next_back()
        .unwrap()
        .split_whitespace()
        .count();

    let mut stacks = vec![vec![]; stacks_cnt];

    for line in stacks_rows.rev() {
        let krate_chunk = line.chars().chunks(4);
        for (index, mut krate) in krate_chunk.into_iter().enumerate() {
            if let Some(krate) = krate.find(|c| c.is_alphabetic()) {
                stacks[index].push(krate);
            }
        }
    }

    stacks
}

fn part2(mut stacks: Vec<Vec<char>>, moves: &[Move]) {
    for Move {
        amount: count,
        from,
        to,
    } in moves.iter()
    {
        let mv_from = stacks[*from].len() - count;
        let mut krates: Vec<_> = stacks[*from].drain(mv_from..).collect();
        stacks[*to].append(&mut krates)
    }

    let top_krates: String = stacks.iter().map(|stack| stack.last().unwrap()).collect();
    println!("Part 2: {top_krates}");
}

fn part1(mut stacks: Vec<Vec<char>>, moves: &[Move]) {
    for Move {
        amount: count,
        from,
        to,
    } in moves.iter()
    {
        for _ in 0..*count {
            let krate = stacks[*from].pop().unwrap();
            stacks[*to].push(krate);
        }
    }

    let top_krates: String = stacks.iter().map(|stack| stack.last().unwrap()).collect();
    println!("Part 1: {top_krates}");
}

fn main() {
    let (stack_description, moves) = include_str!("input").split_once("\n\n").unwrap();

    let stacks = parse_stacks(stack_description);
    let moves = parse_moves(moves);

    part1(stacks.clone(), &moves);
    part2(stacks, &moves);
}
