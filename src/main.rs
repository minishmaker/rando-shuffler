use petgraph::dot::Dot;
use rando_parser::logic::ast::ScopeChild;
use rando_util::graph;
use regex::Regex;
use std::collections::HashMap;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    /*let tree = match rando_parser::logic::parse(&input) {
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
                        let graph_string = format!("{}", dot);
                        println!("{}", concentrate_edges(graph_string));
                    } else {
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
    }*/

    Ok(())
}

fn concentrate_edges(graph: String) -> String {
    let regex = Regex::new(r#"(\d+) -> (\d+) \[ label = "([^"]+)" \]"#).unwrap(); // If this fails, it's a bug
    let mut edges = HashMap::<_, &str>::new();
    let mut output = graph.clone();
    for captures in regex.captures_iter(&graph) {
        let from = captures[1].parse::<u32>().unwrap();
        let to = captures[2].parse::<u32>().unwrap();
        if let Some(logic) = edges.get(&(from, to)) {
            if *logic == &captures[3] {
                let first = format!("{} -> {} [ label = \"{}\" ]", to, from, logic);
                let replacement = format!(
                    "{} -> {} [ label = \"{}\", dir = \"both\" ]",
                    to, from, &captures[3]
                );
                output = output.replace(&first, &replacement);
                output = output.replace(&captures[0], "");
            }
        } else {
            edges.insert((to, from), captures.get(3).unwrap().as_str());
        }
    }

    output
}
