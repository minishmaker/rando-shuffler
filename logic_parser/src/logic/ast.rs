use std::fmt::{self, Display, Formatter};

use super::lexer::Arrow;
use crate::{FullIdent, Ident};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Scope<'a> {
    pub keyword: &'a str,
    pub name: Ident<'a>,
    pub children: ScopeChild<'a>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum ScopeChild<'a> {
    Scope(Vec<Scope<'a>>),
    Room(Vec<RoomItem<'a>>),
    None,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum RoomItem<'a> {
    Node(Node<'a>),
    Connection(Connection<'a>),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Node<'a> {
    pub name: Ident<'a>,
    pub children: Vec<Descriptor<'a>>,
    pub modify: bool,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Connection<'a> {
    pub left: Ident<'a>,
    pub right: Ident<'a>,
    pub arrow: Arrow,
    pub logic: Vec<EdgeLogic<'a>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Descriptor<'a> {
    pub keyword: &'a str,
    pub idents: Vec<FullIdent<'a>>,
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
        write!(f, "{}", self.keyword)?;
        for ident in &self.idents {
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
