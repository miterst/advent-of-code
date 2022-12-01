use std::collections::HashMap;

fn main() {
    let input: Vec<&str> = include_str!("input").split("\n\n").collect();

    let rules: HashMap<&str, char> = input[1].lines().fold(HashMap::new(), |mut map, line| {
        let pair: Vec<&str> = line.split(" -> ").collect();
        map.insert(pair[0], pair[1].chars().next().unwrap());
        map
    });

    let template: Vec<char> = input[0].chars().collect();

    let counter = explore_template(template.clone(), &rules, 10);
    println!(
        "Part 1: {}",
        counter.values().max().unwrap() - counter.values().min().unwrap()
    );

    let counter = explore_template(template.clone(), &rules, 40);
    println!(
        "Part 2: {}",
        counter.values().max().unwrap() - counter.values().min().unwrap()
    );
}

fn explore_template(
    template: Vec<char>,
    rules: &HashMap<&str, char>,
    steps: usize,
) -> HashMap<char, usize> {
    let mut counter: HashMap<char, usize> = HashMap::new();
    let mut cache = HashMap::new();

    for window in template.windows(2) {
        // dfs each pair and keep count on the output of the rules
        for (k, v) in dfs_pair(window.to_vec(), rules, &mut cache, steps - 1) {
            *counter.entry(k).or_default() += v;
        }
    }

    // add the count of the top-level string since before we only counted the output of the rules
    for c in template.iter() {
        *counter.entry(*c).or_default() += 1;
    }

    counter
}

fn dfs_pair(
    template: Vec<char>,
    rules: &HashMap<&str, char>,
    cache: &mut HashMap<(usize, String), HashMap<char, usize>>,
    steps: usize,
) -> HashMap<char, usize> {
    let pair = String::from_iter(template.iter());
    let rule_output = rules.get(pair.as_str()).unwrap();

    let mut counter: HashMap<char, usize> = HashMap::new();
    *counter.entry(*rule_output).or_default() += 1;

    if steps == 0 {
        return counter;
    }

    if let Some(counter) = cache.get(&(steps, pair.clone())) {
        return counter.clone();
    }

    for window in [template[0], *rule_output, template[1]].windows(2) {
        for (k, v) in dfs_pair(window.to_vec(), rules, cache, steps - 1) {
            *counter.entry(k).or_default() += v;
        }
    }

    cache.insert((steps, pair), counter.clone());

    counter
}
