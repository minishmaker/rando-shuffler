use std::fmt::{self, Display, Formatter};

pub mod descriptor;
pub mod logic;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Ident<'a> {
    Anon,
    Normal(&'a str),
    Escaped(&'a str),
}

impl Display for Ident<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Anon => write!(f, "_"),
            Ident::Normal(s) => write!(f, "{}", s),
            Ident::Escaped(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum FullIdent<'a> {
    Namespaced { idents: Vec<Ident<'a>> },
    Global { ident: Ident<'a> },
}

impl Display for FullIdent<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FullIdent::Namespaced { ref idents } => {
                // Write every ident, separated by .
                for name in idents.iter().take(idents.len() - 1) {
                    write!(f, "{}.", name)?;
                }

                write!(f, "{}", idents.last().unwrap())
            }
            FullIdent::Global { ref ident } => write!(f, "g{}", ident),
        }
    }
}
