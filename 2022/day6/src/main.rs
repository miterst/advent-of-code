use itertools::Itertools;

fn find_marker_index(packet: &[char], marker_size: usize) -> usize {
    let (index, _) = packet
        .windows(marker_size)
        .enumerate()
        .find(|(_, marker)| marker.iter().duplicates().next().is_none())
        .unwrap();

    index
}

fn main() {
    let packet: Vec<_> = include_str!("test_input").chars().collect();

    println!("Part1: {0}", find_marker_index(&packet, 4) + 4);
    println!("Part2: {0}", find_marker_index(&packet, 14) + 14);
}
