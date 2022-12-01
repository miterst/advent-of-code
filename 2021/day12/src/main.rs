use std::collections::{HashMap, HashSet, VecDeque};

const START_NODE: &str = "start";
const END_NODE: &str = "end";

type Graph<'a> = HashMap<&'a str, HashSet<&'a str>>;

fn main() {
    let input = include_str!("input");
    let graph: Graph = construct_graph(input);

    // println!("Graph: {:?}", graph);

    part1(&graph);
    part2(&graph);
}

fn part2(graph: &Graph) {
    let mut paths: Vec<_> = vec![];
    let mut queue = VecDeque::new();

    let path = vec![];
    let counter: HashMap<_, usize> = HashMap::new();

    queue.push_back((START_NODE, path, counter));

    while let Some((node, mut path, mut lowercase_nodes_counter)) = queue.pop_front() {
        path.push(node);

        if node.chars().next().unwrap().is_lowercase() {
            *lowercase_nodes_counter.entry(node).or_default() += 1;

            if lowercase_nodes_counter[node] > 2 {
                continue;
            }

            // if we have more than one lowercase node visited twice skip this path
            if lowercase_nodes_counter.values().filter(|x| **x > 1).count() > 1 {
                continue;
            }
        }

        if node == END_NODE {
            paths.push(path.clone());
            continue;
        }

        for n in &graph[node] {
            // would be more efficient to use persistent data structures for `path` and `lowercase_nodes_counter` instead of cloning
            queue.push_back((n, path.clone(), lowercase_nodes_counter.clone()));
        }
    }

    // for p in &paths {
    //     println!("{}", p.join(","));
    // }

    println!("Part 2: {}", paths.len());
}

fn part1(graph: &Graph) {
    let mut paths: Vec<Vec<&str>> = vec![];
    let mut queue = VecDeque::new();
    let path: Vec<&str> = vec![];
    queue.push_back((START_NODE, path));

    while let Some((node, mut path)) = queue.pop_front() {
        if node.chars().next().unwrap().is_lowercase() && path.contains(&node) {
            continue;
        }

        path.push(node);

        if node == END_NODE {
            paths.push(path.clone());
            continue;
        }

        for n in &graph[node] {
            // would be more efficient to use a persistent data structure for `path`
            queue.push_back((n, path.clone()));
        }
    }

    // for p in &paths {
    //     println!("{}", p.join(","));
    // }

    println!("Part 1: {}", paths.len());
}

fn construct_graph(input: &str) -> Graph {
    input.lines().fold(HashMap::new(), |mut graph, edge| {
        let edge: Vec<_> = edge.split('-').collect();

        match edge.as_slice() {
            [start @ START_NODE, node] | [node, start @ START_NODE] => {
                let entry = graph.entry(start).or_default();
                entry.insert(node);
            }
            [end @ END_NODE, node] | [node, end @ END_NODE] => {
                let entry = graph.entry(node).or_default();
                entry.insert(end);
            }
            [from, to] => {
                let entry = graph.entry(from).or_default();
                entry.insert(to);

                let entry = graph.entry(to).or_default();
                entry.insert(from);
            }
            _ => unreachable!("I know what I'm doing"),
        }

        graph
    })
}
