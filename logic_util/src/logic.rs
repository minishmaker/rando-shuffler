use std::fmt::{self, Display, Formatter};

use logic_parser::ast::{Descriptor};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Condition<'a> {
    And(Vec<Condition<'a>>),
    Or(Vec<Condition<'a>>),
    Other(Descriptor<'a>)
}

impl Condition<'_> {
    pub fn is_and(&self) -> bool {
        if let Condition::And(_) = self {
            true
        }
        else {
            false
        }
    }

    pub fn is_or(&self) -> bool {
        if let Condition::Or(_) = self {
            true
        }
        else {
            false
        }
    }

    pub fn is_operation(&self) -> bool {
        if let Condition::And(_) | Condition::Or(_) = self {
            true
        }
        else {
            false
        }
    }
}

pub fn into_logic(descriptor: Descriptor<'_>) -> Result<Condition<'_>, &str> {
    let value = match descriptor.data.keyword {
        "and" => Condition::And(
            descriptor.children.into_iter()
                .map(into_logic).collect::<Result<_, _>>()?
        ),
        "or" => Condition::Or(
            descriptor.children.into_iter()
                .map(into_logic).collect::<Result<_, _>>()?
        ),
        _ => return Ok(Condition::Other(descriptor))
    };

    if descriptor.data.name.is_some() && value.is_operation() {
        Err("Cannot give a name to a logical conjunction")
    }
    else {
        Ok(value)
    }
}

impl Display for Condition<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (delimiter, items) = match self {
            Condition::And(i) => ("&", i),
            Condition::Or(i) => ("|", i),
            Condition::Other(d) => return write!(f, "{}", d)
        };
        
        if !items.is_empty() {
            write!(f, "{}", items[0])?;
            for item in items.iter().skip(1) {
                write!(f, " {} {}", delimiter, item)?;
            }
        }

        Ok(())
    }
}