use crate::part1;

pub fn cnt_increasing_measurements(measurements: &[u32]) -> usize {
    let sliding_windows: Vec<_> = measurements
        .windows(3)
        .map(|window| window.iter().sum())
        .collect();

    part1::cnt_increasing_measurements(&sliding_windows)
}
