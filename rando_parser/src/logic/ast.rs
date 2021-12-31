use std::{
    fmt::{self, Display, Formatter},
    ops::Range,
};

use crate::{common::span::Span, FullIdent, Ident};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Item<'a> {
    pub header: ItemHeader<'a>,
    pub children: Vec<Item<'a>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ItemHeader<'a> {
    Node {
        append: bool,
        keyword: Span<&'a str>,
        idents: Vec<Span<FullIdent<'a>>>,
    },
    Edge {
        left: Span<Ident<'a>>,
        arrow: Span<Arrow>,
        right: Span<Ident<'a>>,
    },
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Arrow {
    Right,
    Left,
    Both,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Scope<'a> {
    pub keyword: Span<&'a str>,
    pub ident: Span<Ident<'a>>,
    pub append: bool,
    pub children: ScopeChild<'a>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ScopeChild<'a> {
    Scope(Vec<Scope<'a>>),
    Graph(Vec<GraphItem<'a>>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum GraphItem<'a> {
    Node(Node<'a>),
    Edge(Edge<'a>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Node<'a> {
    pub ident: Span<Ident<'a>>,
    pub children: Vec<Descriptor<'a>>,
    pub append: bool,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Edge<'a> {
    pub left: Span<Ident<'a>>,
    pub right: Span<Ident<'a>>,
    pub arrow: Span<Arrow>,
    pub logic: Vec<EdgeLogic<'a>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Descriptor<'a> {
    pub keyword: Span<&'a str>,
    pub idents: Vec<Span<FullIdent<'a>>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum EdgeLogic<'a> {
    And(Vec<EdgeLogic<'a>>),
    Or(Vec<EdgeLogic<'a>>),
    Descriptor(Descriptor<'a>),
}

impl EdgeLogic<'_> {
    pub fn is_condition(&self) -> bool {
        matches!(self, EdgeLogic::And(_) | EdgeLogic::Or(_))
    }
}

impl Descriptor<'_> {
    pub fn code_range(&self) -> Range<usize> {
        let Span(start, _, kw_end) = self.keyword;
        let end = self.idents.last().map(|i| i.2).unwrap_or(kw_end);

        start..end
    }
}

impl Display for Descriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.keyword.1)?;
        for Span(_, ident, _) in &self.idents {
            write!(f, " {}", ident)?;
        }

        Ok(())
    }
}

impl Display for EdgeLogic<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let EdgeLogic::Descriptor(d) = self {
            return write!(f, "{}", d);
        }

        let (children, separator) = match self {
            EdgeLogic::And(c) => (c, "&"),
            EdgeLogic::Or(c) => (c, "|"),
            EdgeLogic::Descriptor(_) => unreachable!(),
        };

        let mut children = children.iter();
        let first = match children.next() {
            Some(f) => f,
            None => return Ok(()),
        };

        write!(f, "{}", first)?;
        for child in children {
            write!(f, " {} ", separator)?;
            if child.is_condition() {
                write!(f, "({})", child)?;
            } else {
                write!(f, "{}", child)?;
            }
        }

        Ok(())
    }
}

impl Arrow {
    pub fn new(left: bool, right: bool) -> Option<Arrow> {
        match (left, right) {
            (true, true) => Some(Arrow::Both),
            (true, false) => Some(Arrow::Left),
            (false, true) => Some(Arrow::Right),
            (false, false) => None,
        }
    }
}

impl Display for Arrow {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Arrow::Left => write!(f, "<-"),
            Arrow::Right => write!(f, "->"),
            Arrow::Both => write!(f, "<->"),
        }
    }
}
