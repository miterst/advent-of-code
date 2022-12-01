use std::cmp::Reverse;
use std::collections::BinaryHeap;

type Node = (usize, usize);

fn main() {
    let mut risk_map: Vec<Vec<u32>> = include_str!("input")
        .lines()
        .map(|line| line.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();

    println!(
        "Part 1: {:?}",
        min_cost_path(
            &risk_map,
            (0, 0),
            (risk_map.len() - 1, risk_map[0].len() - 1),
        )
    );

    extend_risk_map(&mut risk_map);

    println!(
        "Part 2: {:?}",
        min_cost_path(
            &risk_map,
            (0, 0),
            (risk_map.len() - 1, risk_map[0].len() - 1),
        )
    );
}

fn min_cost_path(risk_map: &[Vec<u32>], start: Node, goal: Node) -> Option<u32> {
    let rows = risk_map.len();
    let cols = risk_map[0].len();

    // max heap
    let mut heap = BinaryHeap::new();
    heap.push(Reverse((0, start)));

    let mut min_distance = vec![vec![u32::MAX; cols]; rows];
    // starting node doesn't count
    min_distance[0][0] = 0;

    while let Some(Reverse((cost, position @ (i, j)))) = heap.pop() {
        if position == goal {
            return Some(cost);
        }

        // we already have a better path
        if cost > min_distance[i][j] {
            continue;
        }

        // get all the valid neighbours => up, down, left and right
        for (ii, jj) in [-1, 1, 0, 0].iter().zip([0, 0, -1, 1]) {
            match (
                usize::try_from((i as i32) + ii).ok(),
                usize::try_from((j as i32) + jj).ok(),
            ) {
                (Some(i), Some(j)) if i < rows && j < cols => {
                    let cost = cost + risk_map[i][j];
                    if cost < min_distance[i][j] {
                        heap.push(Reverse((cost, (i, j))));
                        min_distance[i][j] = cost;
                    }
                }
                _ => continue,
            }
        }
    }

    None
}

fn extend_risk_map(risk_map: &mut Vec<Vec<u32>>) {
    let rows = risk_map.len();
    let cols = risk_map[0].len();

    // add columns
    for row in risk_map.iter_mut() {
        for i in 1..=4 {
            row.extend_from_within(0..rows);
            // transform each new column
            for risk in row[(i * cols)..((i + 1) * cols)].iter_mut() {
                let inc = *risk + (i as u32);
                *risk = if inc > 9 { inc % 9 } else { inc };
            }
        }
    }

    // add rows
    for i in 1..=4 {
        risk_map.extend_from_within(0..rows);
        // transform each new row
        for row in risk_map[(i * rows)..((i + 1) * rows)].iter_mut() {
            for risk in row.iter_mut() {
                let inc = *risk + (i as u32);
                *risk = if inc > 9 { inc % 9 } else { inc };
            }
        }
    }
}
