use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use petgraph::prelude::*;
use petgraph::graph::DiGraph;
use logic_parser::lexer::{Ident, Arrow};
use logic_parser::ast::{self, RoomItem, Descriptor, Connection};

#[derive(Debug)]
pub struct Node<'a>(Ident<'a>, Vec<Descriptor<'a>>);
#[derive(Debug)]
pub struct Edge<'a>(Vec<Descriptor<'a>>);

pub enum GraphError<'a> {
    DuplicateNode(Ident<'a>),
    MissingNode(Ident<'a>),
    DuplicateEdge(Ident<'a>, Arrow, Ident<'a>),
}

fn add_node<'a>(
    node: ast::Node<'a>,
    nodes: &mut HashMap<Ident<'a>, NodeIndex>,
    graph: &mut DiGraph<Node<'a>, Edge<'a>>
) -> Result<(), GraphError<'a>> {
    if let Some(&graph_node) = nodes.get(&node.name) {
        // Node already created, modify
        if node.modify {
            let _graph_node = &mut graph[graph_node];
            Ok(())
        }
        else {
            Err(GraphError::DuplicateNode(node.name))
        }
    }
    else {
        // Create node
        let index = graph.add_node(Node(node.name, node.children));
        nodes.insert(node.name, index);
        Ok(())
    }
}

fn add_connection<'a>(
    connection: Connection<'a>,
    nodes: &mut HashMap<Ident<'a>, NodeIndex>,
    graph: &mut DiGraph<Node<'a>, Edge<'a>>
) -> Result<(), GraphError<'a>> {
    let &mut left = nodes.get_mut(&connection.left).ok_or(GraphError::MissingNode(connection.left))?;
    let &mut right = nodes.get_mut(&connection.right).ok_or(GraphError::MissingNode(connection.right))?;

    match connection.arrow {
        Arrow::Right => {
            if !graph.contains_edge(left, right) {
                graph.add_edge(left, right, Edge(connection.children))
            }
            else {
                return Err(GraphError::DuplicateEdge(connection.left, Arrow::Right, connection.right))
            }
        },
        Arrow::Left => {
            if !graph.contains_edge(right, left) {
                graph.add_edge(right, left, Edge(connection.children))
            }
            else {
                return Err(GraphError::DuplicateEdge(connection.left, Arrow::Left, connection.right))
            }
        },
        Arrow::Both => {
            let already_right = graph.contains_edge(left, right);
            let already_left = graph.contains_edge(right, left);
            if let Some(arrow) = Arrow::new(already_left, already_right) {
                return Err(GraphError::DuplicateEdge(connection.left, arrow, connection.right))
            }
            else {
                graph.add_edge(left, right, Edge(connection.children.clone()));
                graph.add_edge(right, left, Edge(connection.children))
            }
        }
    };

    Ok(())
}

pub fn make_graph(room: Vec<RoomItem<'_>>) -> Result<DiGraph<Node<'_>, Edge<'_>>, GraphError<'_>> {
    let mut graph = DiGraph::new();
    let mut nodes = HashMap::new();

    for item in room {
        match item {
            RoomItem::Node(node) => add_node(node, &mut nodes, &mut graph)?,
            RoomItem::Connection(connection) => add_connection(connection, &mut nodes, &mut graph)?
        }
    }

    Ok(graph)
}

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

impl Display for GraphError<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            GraphError::DuplicateNode(name) => write!(f, "Duplicate node: {}", name),
            GraphError::MissingNode(name) => write!(f, "Node used before it was defined: {}", name),
            GraphError::DuplicateEdge(left, arrow, right) => write!(f, "Duplicate edge: {} {} {}", left, arrow, right)
        }
    }
}