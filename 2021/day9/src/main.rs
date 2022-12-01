fn main() {
    let heightmap: Vec<Vec<u32>> = include_str!("input")
        .lines()
        .map(|line| line.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();

    part1(&heightmap);
    part2(heightmap);
}

fn part1(heightmap: &[Vec<u32>]) {
    let rows = heightmap.len();
    let cols = heightmap[0].len();

    let mut low_points = vec![];

    for i in 0..rows {
        for j in 0..cols {
            let point = heightmap[i][j];

            let neighbours = neighbours((i, j), heightmap);

            if neighbours
                .iter()
                .all(|(ii, jj)| point < heightmap[*ii][*jj])
            {
                low_points.push(point + 1);
            }
        }
    }

    println!("Part 1: {}", low_points.iter().sum::<u32>());
}

fn part2(mut heightmap: Vec<Vec<u32>>) {
    let rows = heightmap.len();
    let cols = heightmap[0].len();

    let mut basins = vec![];

    for i in 0..rows {
        for j in 0..cols {
            if heightmap[i][j] == 9 {
                continue;
            }

            let basin_size = find_basin(&mut heightmap, (i, j));
            basins.push(basin_size);
        }
    }

    basins.sort();

    println!("Part 2: {}", basins.iter().rev().take(3).product::<u32>());
}

// find the basin given a point
fn find_basin(heightmap: &mut Vec<Vec<u32>>, point: (usize, usize)) -> u32 {
    let mut stack = vec![point];
    let mut basin_size = 0;

    while let Some(point @ (row, col)) = stack.pop() {
        if heightmap[row][col] == 9 {
            continue;
        }
        basin_size += 1;
        // set the visited point to 9
        heightmap[row][col] = 9;

        let neighbours = neighbours(point, heightmap);

        stack.extend(neighbours);
    }

    basin_size
}

fn is_point_valid(point: (isize, isize), heightmap: &[Vec<u32>]) -> bool {
    let (row, col) = point;
    let rows = heightmap.len() as isize;
    let cols = heightmap[0].len() as isize;

    (row >= 0 && row < rows) && (col >= 0 && col < cols)
}

// generate all valid points in the four directions
fn neighbours(point: (usize, usize), heightmap: &[Vec<u32>]) -> Vec<(usize, usize)> {
    let (i, j) = point as (usize, usize);

    // all four directions
    [1, -1, 0, 0]
        .iter()
        .zip([0, 0, 1, -1])
        .map(|(di, dj)| (i as isize + di, j as isize + dj))
        .filter(|(i, j)| is_point_valid((*i, *j), heightmap))
        .map(|p| (p.0 as usize, p.1 as usize))
        .collect()
}
