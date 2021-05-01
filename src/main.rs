use std::io::{self, Read};
use petgraph::dot::Dot;
use logic_parser::ast::{ScopeChild};
use logic_util::graph;

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
