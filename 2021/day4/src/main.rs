use std::collections::HashMap;

#[derive(Debug)]
struct Board {
    grid: HashMap<usize, (usize, usize)>,
    row_marked: Vec<usize>,
    col_marked: Vec<usize>,
}

impl Board {
    fn new() -> Self {
        Board {
            grid: Default::default(),
            row_marked: vec![0; 5],
            col_marked: vec![0; 5],
        }
    }

    fn add_row(&mut self, row: usize, nums: &[usize]) {
        for (col, num) in nums.iter().enumerate() {
            self.grid.insert(*num, (row, col));
        }
    }

    fn mark_and_check(&mut self, num: usize) -> bool {
        if let Some((row, col)) = self.grid.remove(&num) {
            self.row_marked[row] += 1;
            self.col_marked[col] += 1;

            return self.row_marked[row] == 5 || self.col_marked[col] == 5;
        }

        false
    }
}

fn main() {
    let mut input = include_str!("input").lines();

    let random_nums: Vec<usize> = input
        .next()
        .unwrap()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect();

    let (mut boards, _) = input.fold((Vec::<Board>::new(), 0), |(mut boards, row), line| {
        if line.is_empty() {
            let board = Board::new();
            boards.push(board);

            return (boards, 0);
        }
        let nums: Vec<usize> = line
            .split(' ')
            .filter(|x| !x.is_empty())
            .map(|x| x.trim().parse().unwrap())
            .collect();
        let last = boards.len() - 1;
        boards[last].add_row(row, &nums);

        (boards, row + 1)
    });

    let mut results = vec![];

    for board in boards.iter_mut() {
        for (i, n) in random_nums.iter().enumerate() {
            if board.mark_and_check(*n) {
                let unmarked: usize = board.grid.keys().sum();
                results.push((i, unmarked * n));
                break;
            }
        }
    }

    println!("{:?}", results.iter().min_by(|x, y| x.0.cmp(&y.0)).unwrap());
    println!("{:?}", results.iter().max_by(|x, y| x.0.cmp(&y.0)).unwrap());
}
