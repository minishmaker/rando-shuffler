use std::{
    fmt::{self, Display, Formatter},
    ops::Range,
    slice,
};

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    common::span::Span,
    logic::ast::{Descriptor, Item, ItemHeader},
    FullIdent,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemType<'a> {
    Scope(&'a str),
    GraphItem,
    Descriptor,
    Logic,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TreeError<'a, 'b> {
    FullIdent {
        keyword: Span<&'a str>,
        ident: Span<FullIdent<'a>>,
    },
    MultipleIdents {
        keyword: Span<&'a str>,
        idents: Vec<Span<FullIdent<'a>>>,
    },
    WrongItem {
        item: Item<'a>,
        actual: ItemType<'a>,
        expected: ItemType<'b>,
    },
    Children {
        descriptor: Descriptor<'a>,
        children: Vec<Item<'a>>,
    },
    Append {
        descriptor: Descriptor<'a>,
    },
    Multiple(Vec<TreeError<'a, 'b>>),
}

impl<'a, 'b> TreeError<'a, 'b> {
    pub fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Multiple(mut v), Self::Multiple(mut o)) => {
                v.append(&mut o);
                Self::Multiple(v)
            }
            (Self::Multiple(mut v), o) | (o, Self::Multiple(mut v)) => {
                v.push(o);
                Self::Multiple(v)
            }
            (s, o) => Self::Multiple(vec![s, o]),
        }
    }
}

pub fn merge_results<T, U, E, F>(a: Result<T, E>, b: Result<U, E>, f: F) -> Result<(T, U), E>
where
    F: FnOnce(E, E) -> E,
{
    match (a, b) {
        (Ok(a), Ok(b)) => Ok((a, b)),
        (Err(a), Err(b)) => Err(f(a, b)),
        (Err(e), _) | (_, Err(e)) => Err(e),
    }
}

impl<'a> ItemType<'a> {
    fn matches(self, other: Self) -> bool {
        self == other || matches!((other, self), (ItemType::Logic, ItemType::Descriptor))
    }

    fn get_type(item: &ItemHeader<'a>, descriptor: bool) -> ItemType<'a> {
        match item {
            ItemHeader::Node {
                keyword: Span(_, keyword, _),
                ..
            } => match *keyword {
                "node" => ItemType::GraphItem,
                "and" | "or" => ItemType::Logic,
                _ if descriptor => ItemType::Descriptor,
                k => ItemType::Scope(k),
            },
            ItemHeader::Edge { .. } => ItemType::GraphItem,
        }
    }

    fn is_descriptor(self) -> bool {
        matches!(self, ItemType::Descriptor | ItemType::Logic)
    }

    pub fn type_check<'b>(
        item: Item<'a>,
        expected: ItemType<'b>,
    ) -> Result<Item<'a>, TreeError<'a, 'b>> {
        let actual = Self::get_type(&item.header, expected.is_descriptor());
        if actual.matches(expected) {
            Ok(item)
        } else {
            Err(TreeError::WrongItem {
                item,
                expected,
                actual,
            })
        }
    }
}

impl Display for ItemType<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ItemType::Scope(s) => write!(f, "\"{}\"", s),
            ItemType::GraphItem => write!(f, "graph item"),
            ItemType::Logic => write!(f, "logic"),
            ItemType::Descriptor => write!(f, "descriptor"),
        }
    }
}

impl TreeError<'_, '_> {
    fn single_diagnostic<F>(&self, file: F) -> Diagnostic<F> {
        let message = self.diagnostic_message();
        let label = Label::primary(file, self.get_range());

        Diagnostic::error()
            .with_message(message)
            .with_labels(vec![label])
    }

    pub fn diagnostics<'a, F: Clone + 'a>(
        &'a self,
        file: F,
    ) -> impl Iterator<Item = Diagnostic<F>> + 'a {
        let errors = if let TreeError::Multiple(errors) = self {
            &errors[..]
        } else {
            slice::from_ref(self)
        };

        errors
            .iter()
            .map(move |e| e.single_diagnostic(file.clone()))
    }

    fn diagnostic_message(&self) -> String {
        match self {
            TreeError::WrongItem {
                expected, actual, ..
            } => format!("Expected {}, found {}", expected, actual),
            TreeError::FullIdent { keyword, .. } if keyword.1 == "node" => {
                "Node names may not be namespaced or global".into()
            }
            TreeError::FullIdent { .. } => "Scope names may not be namespaced or global".into(),
            TreeError::Append { .. } => "Descriptors may not be in append mode".into(),
            TreeError::Children { .. } => "Descriptors may not have children".into(),
            TreeError::MultipleIdents { keyword, .. } if keyword.1 == "node" => {
                "Nodes may not have multiple identifiers".into()
            }
            TreeError::MultipleIdents { .. } => "Scopes may not have multiple identifiers".into(),
            TreeError::Multiple(_) => panic!("Only single errors have diagnostic messages"),
        }
    }

    fn get_range(&self) -> Range<usize> {
        match self {
            TreeError::WrongItem {
                item: Item { ref header, .. },
                ..
            } => header_range(header),
            TreeError::Append { descriptor } => descriptor.code_range(),
            TreeError::Children { descriptor, .. } => descriptor.code_range(),
            TreeError::FullIdent {
                ident: Span(start, _, end),
                ..
            } => *start..*end,
            TreeError::MultipleIdents { idents, .. } => {
                let Span(start, _, end) = Span::limits(&idents[1..]).unwrap();
                start..end
            }
            TreeError::Multiple(_) => unimplemented!(),
        }
    }
}

fn header_range(header: &ItemHeader<'_>) -> Range<usize> {
    match header {
        ItemHeader::Node {
            keyword: Span(start, _, kw_end),
            idents,
            ..
        } => {
            let end = idents.last().map(|i| i.2).unwrap_or(*kw_end);
            *start..end
        }
        ItemHeader::Edge {
            left: Span(start, ..),
            right: Span(.., end),
            ..
        } => *start..*end,
    }
}
