use codespan_reporting::files::{self, SimpleFile};
use codespan_reporting::term::{self, Config};
use petgraph::dot::Dot;
use rando_parser::logic::ast::ScopeChild;
use rando_parser::logic::LogicError;
use rando_util::graph;
use regex::Regex;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{self, Read, Write};

fn main() -> io::Result<()> {
    let path = env::args().skip(1).next()
        .unwrap_or_else(|| panic!("Expected logic path as first argument"));

    let mut input = String::new();
    File::open(&path)
        .expect("unable to open file")
        .read_to_string(&mut input)
        .expect("unable to read string");

    let tree = match rando_parser::logic::parse(&input, &["area", "room"]) {
        Ok(t) => t,
        Err(e) => {
            output_errors(e, &input, &path).unwrap();
            std::process::exit(1)
        }
    };

    let tree = if let ScopeChild::Scope(areas) = tree {
        areas
    } else {
        unreachable!()
    };

    let mut errored = false;
    for area in tree {
        if let ScopeChild::Scope(rooms) = area.children {
            for room in rooms {
                println!("Room {}", room.ident.1);
                if let ScopeChild::Graph(graph) = room.children {
                    let graph = graph::make_graph(graph);
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
    }

    Ok(())
}

fn output_errors(error: LogicError<'_, '_>, source: &str, path: &str) -> Result<(), files::Error> {
    use term::termcolor::{ColorChoice, StandardStream};
    let file = SimpleFile::new(path, source);
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    let diagnostics = error.diagnostics((), source);
    let count = diagnostics.len();
    for diagnostic in diagnostics {
        term::emit(&mut stdout, &Config::default(), &file, &diagnostic)?;
    }

    writeln!(&mut stdout, "Parsing failed: {} errors detected", count)
        .unwrap();

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
