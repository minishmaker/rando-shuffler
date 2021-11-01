use std::fmt::{self, Display, Formatter};
use std::convert::Infallible;

use regex::Regex;
use lazy_static::lazy_static;

use crate::Ident;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexResult<'a> = Spanned<Tok<'a>, usize, Infallible>;

pub fn lex(text: &str) -> impl Iterator<Item = LexResult<'_>> {
    Lex {
        text,
        index: 0,
        indent_stack: Vec::new(),
        state: LexState::Normal
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Tok<'a> {
    Ident(Ident<'a>),
    Global(Ident<'a>),
    Keyword(&'a str),
    Arrow(Arrow),
    Whitespace(&'a str),
    Comment(&'a str),
    Unk(&'a str),
    Colon,
    Period,
    Comma,
    Newline,
    Indent,
    Dedent,
    Plus,
    Star,
    OpenParen,
    CloseParen,
    And,
    Or,
    Node,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Arrow {
    Right,
    Left,
    Both,
}

impl Arrow {
    pub fn new(left: bool, right: bool) -> Option<Arrow> {
        match (left, right) {
            (true, true) => Some(Arrow::Both),
            (true, false) => Some(Arrow::Left),
            (false, true) => Some(Arrow::Right),
            (false, false) => None
        }
    }
}

impl Display for Arrow {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Arrow::Left => write!(f, "<-"),
            Arrow::Right => write!(f, "->"),
            Arrow::Both => write!(f, "<->")
        }
    }
}

// Syntax inspired by plex, but this is MBE and (more importantly) compiles on the current compiler
macro_rules! lexer {
    (
        fn $fn_name:ident($match:ident : $life:lifetime) -> $output:ty;
        $($regex:literal => $value:expr,)*
    ) => {
        fn $fn_name<$life>(text: &mut &$life str) -> Option<($output, usize)> {
            $(
                {
                    lazy_static! {
                        static ref PATTERN: Regex = Regex::new($regex).expect("Failed to create regex");
                    }

                    if let Some(mat) = PATTERN.find(text) {
                        *text = &text[mat.end()..];
                        return Some(($value(mat.as_str()), mat.end()))
                    }
                }
            )*

            None
        }
    }
}

lexer! {
    fn next_token(tok: 'a) -> Tok<'a>;
    "^:" => |_| Tok::Colon,
    r"^\." => |_| Tok::Period,
    r"^," => |_| Tok::Comma,
    "^\n" => |_| Tok::Newline,
    r"^\+" => |_| Tok::Plus,
    r"^\*" => |_| Tok::Star,
    r"^\(" => |_| Tok::OpenParen,
    r"^\)" => |_| Tok::CloseParen,
    r"^&" => |_| Tok::And,
    r"^\|" => |_| Tok::Or,
    r"^->" => |_| Tok::Arrow(Arrow::Right),
    r"^<->" => |_| Tok::Arrow(Arrow::Both),
    "^node" => |_| Tok::Node,
    r"^g([A-Z]\w*)" => |t: &'a str| Tok::Global(Ident::Normal(&t[1..])),
    r#"^g"([A-Z]\w*)""# => |t: &'a str| Tok::Global(Ident::Escaped(&t[2..t.len()-1])),
    r#"^([A-Z]\w*)"# => |t| Tok::Ident(Ident::Normal(t)),
    "^_" => |_| Tok::Ident(Ident::Anon),
    r#"^"[^"]*""# => |t: &'a str| Tok::Ident(Ident::Escaped(&t[1..t.len()-1])),
    r"^[a-z]\w*" => Tok::Keyword,
    r"^[ \t\r]+" => Tok::Whitespace,
    "^#[^\n]*" => |t: &'a str| Tok::Comment(&t[..t.len()-1]),
    r"^\S+" => Tok::Unk,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum LexState {
    Normal,
    Newline,
    Dedent(usize),
}

#[derive(Clone)]
struct Lex<'a> {
    text: &'a str,
    index: usize,
    indent_stack: Vec<&'a str>, 
    state: LexState
}

impl<'a> Lex<'a> {
    /// Get the next token, of any type, and its length
    fn next_token(&mut self) -> Option<(Tok<'a>, usize)> {
        match self.state {
            LexState::Normal => self.next_normal(),
            LexState::Newline => self.next_newline(),
            LexState::Dedent(ref mut count) => {
                if *count > 0 {
                    *count -= 1;
                    Some((Tok::Dedent, 0))
                }
                else {
                    self.state = LexState::Normal;
                    self.next_normal()
                }
            }
        }
    }

    /// Get the next token under normal circumstances, filtering out whitespace/comments and checking for newlines
    fn next_normal(&mut self) -> Option<(Tok<'a>, usize)> {
        match next_token(&mut self.text) {
            Some((Tok::Newline, _)) => { // Comment includes newline
                // Ignore newline if next line is empty
                if self.line_empty() && self.text.len() > 0 {
                    self.next_normal()
                }
                else {
                    self.state = LexState::Newline;
                    Some((Tok::Newline, 1))
                }
            },
            Some((Tok::Whitespace(_), len))
                | Some((Tok::Comment(_), len)) => {
                    self.index += len;
                    self.next_normal()
            }, 
            None if self.indent_stack.len() > 0 => {
                // Add trailing newline
                self.text = "\n";
                self.next_normal()
            },
            t => t
        }
    }

    /// Get the next token after a newline and set up for dedents
    fn next_newline(&mut self) -> Option<(Tok<'a>, usize)> {
        // Match initial indent
        let mut indents = 0;
        let mut index = 0;
        for indent in self.indent_stack.iter().copied() {
            if self.text.len() < index + indent.len() {
                break; // Bounds check
            }

            let subsection = &self.text[index..index + indent.len()];
            if indent == subsection {
                indents += 1;
                index += indent.len();
            }
            else {
                break;
            }
        }

        self.text = &self.text[index..];
        self.index += index;

        let len = self.indent_stack.len();
        if indents < len {
            self.handle_dedent(len - indents)
        }
        else {
            self.handle_indent()
        }
    }

    /// Called if not all of the dedents are matched
    fn handle_dedent(&mut self, count: usize) -> Option<(Tok<'a>, usize)> {
        let mut lookahead = self.text;
        if let Some((Tok::Whitespace(space), len)) = next_token(&mut lookahead) {
            // Next token is whitespace, incorrect indentation!
            Some((Tok::Whitespace(space), len))
        }
        else {
            // Properly dedented, set dedent mode and stack
            self.state = LexState::Dedent(count);
            let len = self.indent_stack.len();
            self.indent_stack.truncate(len - count);
            self.next_token()
        }
    }

    /// Called if all of the dedents are matched
    fn handle_indent(&mut self) -> Option<(Tok<'a>, usize)> {
        self.state = LexState::Normal;
        let tok = next_token(&mut self.text);

        if let Some((Tok::Whitespace(space), len)) = tok {
            // Next token is whitespace, so it's another indent
            self.indent_stack.push(space);
            Some((Tok::Indent, len))
        }
        else {
            // Next token is not whitespace
            tok 
        }
    }

    fn line_empty(&mut self) -> bool {
        // Get first line
        let mut line = self.text.lines().next().unwrap_or("");
        
        // Tokenize
        while let Some(tok) = next_token(&mut line) {
            match tok {
                // Anything other than whitespace, newline, or comment means unempty
                (Tok::Whitespace(_), _) => continue,
                (Tok::Newline, _) => break,
                (Tok::Comment(_), _) => break,
                _ => return false
            }
        }

        true // Line ended without any tokens
    }
}

impl<'a> Iterator for Lex<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<LexResult<'a>> {
        let (tok, len) = self.next_token()?;
        let spanned = (self.index, tok, self.index + len);
        self.index += len;
        Some(Ok(spanned))
    }
}
