use std::fmt::{self, Display, Formatter};

use crate::{common::Span, FullIdent, Ident};

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
    pub children: Option<ScopeChild<'a>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ScopeChild<'a> {
    Scope(Vec<Scope<'a>>),
    Room(Vec<RoomItem<'a>>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RoomItem<'a> {
    Node(Node<'a>),
    Edge(Edge<'a>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Node<'a> {
    pub name: Span<Ident<'a>>,
    pub children: Vec<Descriptor<'a>>,
    pub modify: bool,
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

impl Display for Descriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.keyword.1)?;
        for (_, ident, _) in &self.idents {
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
