fn main() {
    let mut calories: Vec<_> = 
        include_str!("../../input")
            .split("\n\n")
            .map(|line| line.lines().map(|n| n.parse::<u32>().unwrap()).sum::<u32>())
            .collect();
    
    calories.sort();
    
    println!("Part 1: {}", calories.last().unwrap());
    println!("Part 2: {}", calories.iter().rev().take(3).sum::<u32>());
}
