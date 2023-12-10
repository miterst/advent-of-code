use std::collections::HashMap;

fn main() {
    let mut hands: Vec<(Hand, u64)> = include_str!("example")
        .lines()
        .map(|line| {
            let (hand, bid) = line.split_once(' ').unwrap();
            let hand: [char; 5] = hand
                .trim()
                .chars()
                .map(|c| c.to_owned())
                .collect::<Vec<char>>()
                .try_into()
                .unwrap();
            let bid: u64 = bid.trim().parse().unwrap();

            (Hand(hand), bid)
        })
        .collect();

    hands.sort_by(|(a, _), (b, _)| cmp_rank(a, b));

    println!("{:?}", bids_total(&hands));

    hands.sort_by(|(a, _), (b, _)| cmp_rank_with_joker(a, b));

    println!("{:?}", bids_total(&hands));
}

fn bids_total(hands: &[(Hand, u64)]) -> u64 {
    hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i + 1) as u64 * bid)
        .sum::<u64>()
}

#[derive(Debug)]
struct Hand([char; 5]);

fn cmp_hand(a: &Hand, b: &Hand) -> std::cmp::Ordering {
    let a = a.0.map(to_digit);
    let b: [u32; 5] = b.0.map(to_digit);

    a.partial_cmp(&b).unwrap()
}

fn cmp_hand_joker(a: &Hand, b: &Hand) -> std::cmp::Ordering {
    let a = a.0.map(to_digit_joker);
    let b: [u32; 5] = b.0.map(to_digit_joker);

    a.partial_cmp(&b).unwrap()
}

fn cmp_rank(a: &Hand, b: &Hand) -> std::cmp::Ordering {
    rank(a).cmp(&rank(b)).then(cmp_hand(a, b))
}

fn cmp_rank_with_joker(a: &Hand, b: &Hand) -> std::cmp::Ordering {
    rank_with_joker(a)
        .cmp(&rank_with_joker(b))
        .then(cmp_hand_joker(a, b))
}

fn rank(hand: &Hand) -> Vec<u32> {
    let mut m = HashMap::new();
    for c in hand.0 {
        *m.entry(c).or_insert(0) += 1;
    }
    let mut v: Vec<u32> = m.values().cloned().collect();
    v.sort_by(|a, b| b.cmp(a));
    v
}

fn rank_with_joker(hand: &Hand) -> Vec<u32> {
    let mut m = HashMap::new();

    for c in hand.0 {
        *m.entry(c).or_insert(0) += 1;
    }

    let jokers_cnt = m.remove(&'J').unwrap_or(0);

    let mut v: Vec<u32> = m.values().cloned().collect();
    v.sort_by(|a, b| b.cmp(a));

    if v.is_empty() {
        v.push(jokers_cnt);
    } else {
        v[0] += jokers_cnt;
    }

    v
}

fn to_digit(c: char) -> u32 {
    match c {
        _ if c >= '2' && c <= '9' => c.to_digit(10).unwrap(),
        'T' => 10,
        'J' => 11,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _ => unreachable!(),
    }
}

fn to_digit_joker(c: char) -> u32 {
    match c {
        _ if c >= '2' && c <= '9' => c.to_digit(10).unwrap(),
        'J' => 1,
        'T' => 10,
        'Q' => 12,
        'K' => 13,
        'A' => 14,
        _ => unreachable!(),
    }
}
