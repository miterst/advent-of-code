use std::collections::{HashSet, VecDeque};

fn main() {
    let energy_map: Vec<Vec<u32>> = include_str!("input")
        .lines()
        .map(|line| line.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();

    part1(energy_map.clone());
    part2(energy_map);
}

fn part1(mut energy_map: Vec<Vec<u32>>) {
    let rows = energy_map.len();
    let cols = energy_map[0].len();
    let mut total_flashed = 0;

    for _step in 0..100 {
        let mut to_flash = vec![];

        for i in 0..rows {
            for j in 0..cols {
                energy_map[i][j] += 1;
                if energy_map[i][j] > 9 {
                    to_flash.push((i, j))
                }
            }
        }
        let mut flashed = HashSet::new();
        for point in to_flash {
            flash(point, &mut energy_map, &mut flashed);
        }
        total_flashed += flashed.len();
    }

    println!("Part 1: {}", total_flashed);
}

fn part2(mut energy_map: Vec<Vec<u32>>) {
    let rows = energy_map.len();
    let cols = energy_map[0].len();

    let mut step = 0;

    loop {
        step += 1;

        let mut to_flash = vec![];

        for r in 0..rows {
            for c in 0..cols {
                energy_map[r][c] += 1;
                if energy_map[r][c] > 9 {
                    to_flash.push((r, c))
                }
            }
        }

        let mut flashed = HashSet::new();
        for point in to_flash {
            flash(point, &mut energy_map, &mut flashed);
        }
        let synchronized = energy_map.iter().all(|inner| inner.iter().all(|x| *x == 0));
        if synchronized {
            break;
        }
    }

    println!("Part 2: {}", step);
}

// propagate the flash to all un-flashed points
fn flash(point: (usize, usize), grid: &mut [Vec<u32>], flashed: &mut HashSet<(usize, usize)>) {
    let mut queue = VecDeque::new();
    queue.push_back(point);

    while let Some(next_point @ (r, c)) = queue.pop_front() {
        if grid[r][c] > 9 {
            grid[r][c] = 0;

            flashed.insert(next_point);

            for neighbour @ (rr, cc) in neighbours(next_point, grid) {
                if !flashed.contains(&neighbour) {
                    grid[rr][cc] += 1;
                    queue.push_back(neighbour);
                }
            }
        }
    }
}

// generate all valid points in the eight directions
fn neighbours(point: (usize, usize), grid: &[Vec<u32>]) -> Vec<(usize, usize)> {
    let (i, j) = point as (usize, usize);

    let mut points = vec![];
    for ii in -1..=1 {
        for jj in -1..=1 {
            let i = i as isize + ii;
            let j = j as isize + jj;
            if is_valid((i, j), grid) {
                points.push((i as usize, j as usize));
            }
        }
    }
    points
}

fn is_valid(point: (isize, isize), grid: &[Vec<u32>]) -> bool {
    let (row, col) = point;
    let rows = grid.len() as isize;
    let cols = grid[0].len() as isize;

    (row >= 0 && row < rows) && (col >= 0 && col < cols)
}
