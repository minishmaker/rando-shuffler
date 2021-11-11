use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use petgraph::prelude::*;
use petgraph::graph::DiGraph;
use logic_parser::Ident;
use logic_parser::logic::lexer::Arrow;
use logic_parser::logic::ast::{self, Connection, Descriptor, EdgeLogic, RoomItem};

#[derive(Clone, Debug)]
pub struct Node<'a>(pub Ident<'a>, pub Vec<Descriptor<'a>>);
#[derive(Clone, Debug)]
pub struct Edge<'a>(pub Vec<EdgeLogic<'a>>);

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
            let mut graph_node = &mut graph[graph_node];
            update_node(&mut graph_node, node);
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

fn update_node<'a>(graph_node: &mut Node<'a>, ast_node: ast::Node<'a>) {
    // This probably isn't the correct semantics yet.
    // Check for nodes that are modified vs added
    let (replace, add): (Vec<_>, Vec<_>) = graph_node.1.iter_mut()
        .zip(ast_node.children.into_iter())
        .partition(|(old, new)| old == &new);

    // Replace descriptors that are the same as old ones
    for (old, new) in replace.into_iter() {
        *old = new;
    }

    // Add descriptors that are added
    // Need to reallocate to fix lifetime issues
    let add = add.into_iter().map(|(_, new)| new).collect::<Vec<_>>();
    graph_node.1.reserve(add.len());
    graph_node.1.extend(add);
}

fn add_connection<'a>(
    connection: Connection<'a>,
    nodes: &mut HashMap<Ident<'a>, NodeIndex>,
    graph: &mut DiGraph<Node<'a>, Edge<'a>>
) -> Result<(), GraphError<'a>> {
    let &mut left = nodes.get_mut(&connection.left).ok_or(GraphError::MissingNode(connection.left))?;
    let &mut right = nodes.get_mut(&connection.right).ok_or(GraphError::MissingNode(connection.right))?;

    let logic = connection.logic;
    match connection.arrow {
        Arrow::Right => {
            if !graph.contains_edge(left, right) {
                graph.add_edge(left, right, Edge(logic))
            }
            else {
                return Err(GraphError::DuplicateEdge(connection.left, Arrow::Right, connection.right))
            }
        },
        Arrow::Left => {
            if !graph.contains_edge(right, left) {
                graph.add_edge(right, left, Edge(logic))
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
                graph.add_edge(left, right, Edge(logic.clone()));
                graph.add_edge(right, left, Edge(logic))
            }
        }
    };

    Ok(())
}

pub fn make_graph(room: Vec<RoomItem<'_>>) -> Result<DiGraph<Node<'_>, Edge<'_>>, Vec<GraphError<'_>>> {
    let mut graph = DiGraph::new();
    let mut nodes = HashMap::new();
    let mut errors = Vec::new();

    for item in room {
        match item {
            RoomItem::Node(node) => { add_node(node, &mut nodes, &mut graph).unwrap_or_else(|e| errors.push(e)); },
            RoomItem::Connection(connection) => { add_connection(connection, &mut nodes, &mut graph).unwrap_or_else(|e| errors.push(e)); }
        }
    }

    if errors.is_empty() {
        Ok(graph)
    }
    else {
        Err(errors)
    }
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Edge<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0.len() {
            0 => Ok(()),
            1 => write!(f, "{}", self.0[0]),
            _ => {
                write!(f, "({})", self.0[0])?;
                for item in self.0.iter().skip(1) {
                    write!(f, " & ({})", item)?
                }
                Ok(())
            }
        }
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