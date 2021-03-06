use petgraph::graph::DiGraph;
use petgraph::prelude::*;
use rando_parser::logic::ast::{self, Arrow, Descriptor, Edge as AstEdge, EdgeLogic, GraphItem};
use rando_parser::{Ident, Span};
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub struct Node<'a>(pub Ident<'a>, pub Vec<Descriptor<'a>>);
#[derive(Clone, Debug)]
pub struct Edge<'a>(pub Vec<EdgeLogic<'a>>);

pub enum GraphError<'a> {
    DuplicateNode(Span<Ident<'a>>),
    MissingNode(Span<Ident<'a>>),
    DuplicateEdge(Span<(Ident<'a>, Arrow, Ident<'a>)>),
}

fn add_node<'a>(
    node: ast::Node<'a>,
    nodes: &mut HashMap<Ident<'a>, NodeIndex>,
    graph: &mut DiGraph<Node<'a>, Edge<'a>>,
) -> Result<(), GraphError<'a>> {
    if let Some(&graph_node) = nodes.get(&node.ident.1) {
        // Node already created, modify
        if node.append {
            let mut graph_node = &mut graph[graph_node];
            update_node(&mut graph_node, node);
            Ok(())
        } else {
            Err(GraphError::DuplicateNode(node.ident))
        }
    } else {
        // Create node
        let index = graph.add_node(Node(node.ident.1, node.children));
        nodes.insert(node.ident.1, index);
        Ok(())
    }
}

fn update_node<'a>(graph_node: &mut Node<'a>, ast_node: ast::Node<'a>) {
    // This probably isn't the correct semantics yet.
    // Check for nodes that are modified vs added
    let (replace, add): (Vec<_>, Vec<_>) = graph_node
        .1
        .iter_mut()
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
    connection: AstEdge<'a>,
    nodes: &mut HashMap<Ident<'a>, NodeIndex>,
    graph: &mut DiGraph<Node<'a>, Edge<'a>>,
) -> Result<(), GraphError<'a>> {
    let &mut left = nodes
        .get_mut(&connection.left.1)
        .ok_or(GraphError::MissingNode(connection.left))?;
    let &mut right = nodes
        .get_mut(&connection.right.1)
        .ok_or(GraphError::MissingNode(connection.right))?;

    let logic = connection.logic;
    match connection.arrow.1 {
        Arrow::Right => {
            if !graph.contains_edge(left, right) {
                graph.add_edge(left, right, Edge(logic))
            } else {
                return Err(GraphError::DuplicateEdge(Span(
                    connection.left.0,
                    (connection.left.1, Arrow::Right, connection.right.1),
                    connection.right.2,
                )));
            }
        }
        Arrow::Left => {
            if !graph.contains_edge(right, left) {
                graph.add_edge(right, left, Edge(logic))
            } else {
                return Err(GraphError::DuplicateEdge(Span(
                    connection.left.0,
                    (connection.left.1, Arrow::Left, connection.right.1),
                    connection.right.2,
                )));
            }
        }
        Arrow::Both => {
            let already_right = graph.contains_edge(left, right);
            let already_left = graph.contains_edge(right, left);
            if let Some(arrow) = Arrow::new(already_left, already_right) {
                return Err(GraphError::DuplicateEdge(Span(
                    connection.left.0,
                    (connection.left.1, arrow, connection.right.1),
                    connection.right.2,
                )));
            } else {
                graph.add_edge(left, right, Edge(logic.clone()));
                graph.add_edge(right, left, Edge(logic))
            }
        }
    };

    Ok(())
}

pub fn make_graph(
    room: Vec<GraphItem<'_>>,
) -> Result<DiGraph<Node<'_>, Edge<'_>>, Vec<GraphError<'_>>> {
    let mut graph = DiGraph::new();
    let mut nodes = HashMap::new();
    let mut errors = Vec::new();

    for item in room {
        match item {
            GraphItem::Node(node) => {
                add_node(node, &mut nodes, &mut graph).unwrap_or_else(|e| errors.push(e));
            }
            GraphItem::Edge(connection) => {
                add_connection(connection, &mut nodes, &mut graph)
                    .unwrap_or_else(|e| errors.push(e));
            }
        }
    }

    if errors.is_empty() {
        Ok(graph)
    } else {
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
            GraphError::DuplicateNode(Span(_, name, _)) => write!(f, "Duplicate node: {}", name),
            GraphError::MissingNode(Span(_, name, _)) => {
                write!(f, "Node used before it was defined: {}", name)
            }
            GraphError::DuplicateEdge(Span(_, (left, arrow, right), _)) => {
                write!(f, "Duplicate edge: {} {} {}", left, arrow, right)
            }
        }
    }
}
