use std::io::{self, Read};
use std::collections::HashMap;
use petgraph::graph::DiGraph;
use petgraph::dot::Dot;
use logic_parser::ast::{ScopeChild};
use logic_util::graph::{self, Node, Edge};

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let tree = match logic_parser::parse(&input) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            std::process::exit(1)
        }
    };

    let mut errored = false;
    for area in tree {
        if let ScopeChild::Scope(rooms) = area.children {
            for room in rooms {
                println!("Room {}", room.name);
                if let ScopeChild::Room(room) = room.children {
                    let graph = graph::make_graph(room);
                    if let Ok(graph) = graph {
                        let graph = remove_parallel_logic(graph);
                        let dot = Dot::new(&graph);
                        println!("{}", dot);
                    }
                    else {
                        errored = true;
                        for error in graph.unwrap_err() {
                            eprintln!("{}", error);
                        }
                    }
                }
            }
        }
    }

    // Exit with code 1 if anything failed
    if errored {
        std::process::exit(1)
    }

    Ok(())
}

// For nice printing, removes the logic from parallel edges with the same logic
fn remove_parallel_logic<'a, 'b>(graph: DiGraph<Node<'a>, Edge<'b>>) -> DiGraph<Node<'a>, Edge<'b>> {
    // Inefficient cause I'm tired, will rewrite when in the mood
    let mut graph = graph.clone();
    let mut edges = HashMap::<_, &Edge<'_>>::new();
    let mut parallel = Vec::new();
    for edge in graph.raw_edges().iter() {
        // If there is already an edge with the same logic in the opposite direction, mark as parallel
        if let Some(parallel_edge) = edges.get(&(edge.target(), edge.source())) {
            if &edge.weight.0 == &parallel_edge.0 {
                parallel.push(edge);
            }
        }
        else {
            edges.insert((edge.source(), edge.target()), &edge.weight);
        }
    }

    // Collect to end old parallel lifetime
    let parallel = parallel.into_iter().map(|e| graph.find_edge(e.source(), e.target()).unwrap()).collect::<Vec<_>>();
    for edge in parallel.into_iter() {
        let edge = graph.edge_weight_mut(edge).unwrap();
        *edge = Edge(Vec::new());
    }

    graph
}