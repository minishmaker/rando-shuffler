use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use petgraph::prelude::*;
use petgraph::graph::DiGraph;
use petgraph::data::{Element, FromElements};
use logic_parser::lexer::{Ident, Arrow};
use logic_parser::ast::{RoomItem, Descriptor};

pub struct Node<'a>(Ident<'a>, Vec<Descriptor<'a>>);
pub struct Edge<'a>(Vec<Descriptor<'a>>);

impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Edge<'_> {
    fn fmt(&self, _: &mut Formatter) -> fmt::Result {
        Ok(())
    }
}

pub fn make_graph(room: Vec<RoomItem<'_>>) -> DiGraph<Node<'_>, Edge<'_>> {
    let mut graph = DiGraph::new();
    let mut nodes = HashMap::new();

    for item in room {
        match item {
            RoomItem::Node(node) => {
                if let Some(&graph_node) = nodes.get(&node.name) {
                    let graph_node = &mut graph[graph_node];

                    match graph_node {
                        Some(_) => if !node.modify { panic!("Duplicate nodes: {}", node.name) },
                        None => *graph_node = Some(Node(node.name, node.children))
                    }
                }
                else {
                    let index = graph.add_node(Some(Node(node.name, node.children)));
                    nodes.insert(node.name, index);
                }
            },
            RoomItem::Connection(connection) => {
                let &mut left = nodes.entry(connection.left).or_insert_with(|| graph.add_node(None));
                let &mut right = nodes.entry(connection.right).or_insert_with(|| graph.add_node(None));

                match connection.arrow {
                    Arrow::Right => graph.add_edge(left, right, Edge(connection.children)),
                    Arrow::Left => graph.add_edge(right, left, Edge(connection.children)),
                    Arrow::Both => {
                        graph.add_edge(left, right, Edge(connection.children.clone()));
                        graph.add_edge(right, left, Edge(connection.children))
                    }
                };
            }
        }
    }

    let (nodes, edges) = graph.into_nodes_edges();
    FromElements::from_elements(nodes.into_iter().map(|n| Element::Node { weight: n.weight.unwrap() })
        .chain(edges.into_iter().map(|e| Element::Edge { source: e.source().index(), target: e.target().index(), weight: e.weight })))
}