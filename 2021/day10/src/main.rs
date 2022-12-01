fn main() {
    let chunks: Vec<&str> = include_str!("input").lines().collect();

    part1(&chunks);
    part2(&chunks);
}

fn part1(chunks: &[&str]) {
    let mut illegal_chars = vec![];

    for chunk in chunks {
        let mut stack = vec![];
        for c in chunk.chars() {
            match c {
                '[' | '{' | '<' | '(' => stack.push(c),
                closing => match stack.pop() {
                    Some(opening)
                        if matches!(
                            (opening, closing),
                            ('[', ']') | ('{', '}') | ('<', '>') | ('(', ')')
                        ) =>
                    {
                        continue
                    }
                    _ => {
                        illegal_chars.push(closing);
                        break;
                    }
                },
            }
        }
    }

    println!(
        "Part 1: {}",
        illegal_chars
            .iter()
            .map(|c| match c {
                ')' => 3,
                ']' => 57,
                '}' => 1197,
                '>' => 25137,
                _ => unreachable!(),
            })
            .sum::<u32>()
    );
}

fn part2(chunks: &[&str]) {
    let mut scores = vec![];

    for chunk in chunks {
        let mut stack = vec![];
        for c in chunk.chars() {
            match c {
                '[' | '{' | '<' | '(' => stack.push(c),
                closing => match stack.pop() {
                    Some(opening)
                        if matches!(
                            (opening, closing),
                            ('[', ']') | ('{', '}') | ('<', '>') | ('(', ')')
                        ) =>
                    {
                        continue
                    }
                    _ => {
                        // clean the stack for corrupted lines
                        stack.clear();
                        break;
                    }
                },
            }
        }

        if !stack.is_empty() {
            let score = stack.iter().rev().fold(0u64, |mut acc, c| {
                match c {
                    '(' => acc = acc * 5 + 1,
                    '[' => acc = acc * 5 + 2,
                    '{' => acc = acc * 5 + 3,
                    '<' => acc = acc * 5 + 4,
                    _ => unreachable!(),
                }
                acc
            });

            scores.push(score);
        }
    }

    scores.sort();

    println!("Part 2: {}", scores[scores.len() / 2]);
}
