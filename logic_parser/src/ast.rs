use std::fmt::{self, Display, Formatter};

use super::lexer::{Ident, Arrow};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Scope<'a> {
    pub keyword: &'a str,
    pub name: Ident<'a>,
    pub children: ScopeChild<'a>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ScopeChild<'a> {
    Scope(Vec<Scope<'a>>),
    Room(Vec<RoomItem<'a>>),
    None
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum RoomItem<'a> {
    Node(Node<'a>),
    Connection(Connection<'a>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Node<'a> {
    pub name: Ident<'a>,
    pub children: Vec<Descriptor<'a>>,
    pub modify: bool
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Connection<'a> {
    pub left: Ident<'a>,
    pub right: Ident<'a>,
    pub arrow: Arrow,
    pub children: Vec<Descriptor<'a>>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Descriptor<'a> {
    pub data: DescriptorData<'a>,
    pub children: Vec<Descriptor<'a>>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DescriptorData<'a> {
    pub keyword: &'a str,
    pub name: Option<NamespacedIdent<'a>>
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct NamespacedIdent<'a> {
    pub idents: Vec<Ident<'a>>
}

impl Display for NamespacedIdent<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for name in self.idents.iter().take(self.idents.len() - 1) {
            write!(f, "{}.", name)?;
        }

        write!(f, "{}", self.idents.last().unwrap())
    }
}

impl Display for Descriptor<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        print_descriptor(self, 0, f)
    }
}

fn print_descriptor(descriptor: &Descriptor<'_>, indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", descriptor.data)?;

    if !descriptor.children.is_empty() {
        write!(f, ": ")?;
        for child in descriptor.children.iter() {
            writeln!(f)?;
            print_descriptor(child, indent + 1, f)?;
        }
    }

    Ok(())
}

impl Display for DescriptorData<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.keyword)?;
        if let Some(name) = &self.name {
            write!(f, " {}", name)?;
        }

        Ok(())
    }
}

/*impl Display for Scope<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        print_scope(self, 0, f)
    }
}

fn print_scope(scope: &Scope<'_>, indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(f, "{:>indent$} {}:", scope.keyword, scope.name, indent = indent * 2)?;

    match scope.children {
        ScopeChild::Room(room) => for item in room { print_item(item, indent + 1, f)? },
        ScopeChild::Scope(scopes) => for scope in scopes.iter() { print_scope(scope, indent + 1, f)? }
    }

    Ok(())
}

fn print_item(item: RoomItem, indent: usize, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(f, "{:>indent$}", indent = indent * 2)?;

    match item {
        RoomItem::Node(node) => print_node(none, indent, f),
        RoomItem::Connection(connection) => print_connection(connection, indent, f)
    }
}

fn print_connection(connection: Connection, ident: usize, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} {} {}", self.left, self.arrow, self.right)?;

}*/