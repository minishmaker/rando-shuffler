use crate::{common::Span, FullIdent, Ident};

use self::error::{accumulate_errors, merge_results, ItemType};

use super::{
    ast::{Descriptor, Edge, EdgeLogic, GraphItem, Node, Scope, ScopeChild},
    parser::{Item, ItemHeader},
};

mod error;

#[cfg(test)]
mod test;

pub use error::TreeError;

fn flatten_idents<'a>(
    keyword: Span<&'a str>,
    mut idents: Vec<Span<FullIdent<'a>>>,
) -> Result<Span<Ident<'a>>, TreeError<'a, 'static>> {
    if idents.len() == 1 {
        let (start, ident, end) = idents.swap_remove(0);
        match &ident {
            FullIdent::Global { .. } => Err(TreeError::FullIdent {
                keyword,
                ident: (start, ident, end),
            }),
            FullIdent::Namespaced { idents } => match idents.len() {
                1 => Ok((start, idents[0], end)),
                _ => Err(TreeError::FullIdent {
                    keyword,
                    ident: (start, ident, end),
                }),
            },
        }
    } else {
        Err(TreeError::MultipleIdents { keyword, idents })
    }
}

fn convert_scope<'a, 'b>(
    item: Item<'a>,
    scopes: &[&'b str],
) -> Result<Scope<'a>, TreeError<'a, 'b>> {
    let item = ItemType::type_check(item, ItemType::Scope(scopes[0]))?;
    match item.header {
        ItemHeader::Node {
            keyword,
            idents,
            append,
        } => {
            let ident = flatten_idents(keyword, idents);
            let children = convert_scope_children(item.children, &scopes[1..]);
            let (ident, children) = merge_results(ident, children, TreeError::merge)?;

            Ok(Scope {
                keyword,
                ident,
                children,
                append,
            })
        }
        _ => unreachable!(),
    }
}

pub fn convert_scope_children<'a, 'b>(
    items: Vec<Item<'a>>,
    scopes: &[&'b str],
) -> Result<ScopeChild<'a>, TreeError<'a, 'b>> {
    if !scopes.is_empty() {
        items
            .into_iter()
            .map(|i| convert_scope(i, scopes))
            .fold(Ok(Vec::new()), accumulate_errors)
            .map(ScopeChild::Scope)
    } else {
        items
            .into_iter()
            .map(convert_graph_item)
            .fold(Ok(Vec::new()), accumulate_errors)
            .map(ScopeChild::Graph)
    }
}

fn convert_graph_item<'a>(item: Item<'a>) -> Result<GraphItem<'a>, TreeError<'a, 'static>> {
    let item = ItemType::type_check(item, ItemType::GraphItem)?;
    match item.header {
        ItemHeader::Edge { left, arrow, right } => item
            .children
            .into_iter()
            .map(convert_edge_logic)
            .fold(Ok(Vec::new()), accumulate_errors)
            .map(|logic| Edge {
                left,
                arrow,
                right,
                logic,
            })
            .map(GraphItem::Edge),
        ItemHeader::Node {
            keyword,
            idents,
            append,
        } => {
            let ident = flatten_idents(keyword, idents);
            let children = item
                .children
                .into_iter()
                .map(convert_descriptor)
                .fold(Ok(Vec::new()), accumulate_errors);
            merge_results(ident, children, TreeError::merge)
                .map(|(ident, children)| Node {
                    ident,
                    children,
                    append,
                })
                .map(GraphItem::Node)
        }
    }
}

fn convert_edge_logic<'a>(item: Item<'a>) -> Result<EdgeLogic<'a>, TreeError<'a, 'static>> {
    let item = ItemType::type_check(item, ItemType::Logic)?;
    match &item.header {
        ItemHeader::Node {
            keyword: (_, op @ ("and" | "or"), _),
            ..
        } => item
            .children
            .into_iter()
            .map(convert_edge_logic)
            .fold(Ok(Vec::new()), accumulate_errors)
            .map(|c| {
                if *op == "and" {
                    EdgeLogic::And(c)
                } else {
                    EdgeLogic::Or(c)
                }
            }),
        _ => convert_descriptor(item).map(EdgeLogic::Descriptor),
    }
}

fn convert_descriptor<'a>(item: Item<'a>) -> Result<Descriptor<'a>, TreeError<'a, 'static>> {
    let item = ItemType::type_check(item, ItemType::Descriptor)?;
    match item.header {
        ItemHeader::Node {
            keyword,
            idents,
            append,
        } => {
            let descriptor = Descriptor { keyword, idents };
            match (item.children.is_empty(), append) {
                (true, false) => Ok(descriptor),
                (false, _) => Err(TreeError::Children {
                    descriptor,
                    children: item.children,
                }),
                _ => Err(TreeError::Append { descriptor }),
            }
        }
        _ => unreachable!(),
    }
}
