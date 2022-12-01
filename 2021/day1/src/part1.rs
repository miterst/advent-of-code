pub fn cnt_increasing_measurements(measurements: &[u32]) -> usize {
    measurements.windows(2).filter(|w| w[0] < w[1]).count()
}
