use std::io::{self, Read};
use petgraph::dot::Dot;
use logic_parser::ast::{ScopeChild};
use logic_util::graph;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let tree = logic_parser::parse(&input).unwrap();
    /*for c in tree.iter() {
        print!("{}", c);
    }*/

    for area in tree {
        if let ScopeChild::Scope(rooms) = area.children {
            for room in rooms {
                if let ScopeChild::Room(room) = room.children {
                    let graph = graph::make_graph(room);
                    if let Ok(graph) = graph {
                        let dot = Dot::new(&graph);
                        println!("{}", dot);
                    }
                    else {
                        println!("{}", graph.unwrap_err());
                    }
                }
            }
        }
    }

    Ok(())
}
