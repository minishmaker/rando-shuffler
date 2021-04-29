use std::fmt::{self, Display, Formatter};
use std::convert::Infallible;

use regex::Regex;
use lazy_static::lazy_static;

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
    Node
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Arrow {
    Right,
    Left,
    Both,
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

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Ident<'a> {
    Wildcard,
    Normal(&'a str),
    Escaped(&'a str)
}

impl Display for Ident<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Wildcard => write!(f, "_"),
            Ident::Normal(s) => write!(f, "{}", s),
            Ident::Escaped(s) => write!(f, "\"{}\"", s)
        }
    }
}

// Syntax inspired by plex, but this is MBE and (more importantly) compiles on the current compiler
macro_rules! lexer {
    (
        fn $fn_name:ident($match:ident : $life:lifetime) -> $output:ty;
        $($regex_name:ident $regex:literal => $value:expr,)*
    ) => {
        fn $fn_name<$life>(text: &mut &$life str) -> Option<($output, usize)> {
            lazy_static! {
                $(
                    static ref $regex_name: Regex = Regex::new($regex).expect("Failed to create regex");
                )*
            }

            $(
                if let Some(mat) = $regex_name.find(text) {
                    *text = &text[mat.end()..];
                    return Some(($value(mat.as_str()), mat.end()))
                }
            )*

            None
        }
    }
}

lexer! {
    fn next_token(tok: 'a) -> Tok<'a>;
    COLON "^:" => |_| Tok::Colon,
    PERIOD r"^\." => |_| Tok::Period,
    NEWLINE "^\n" => |_| Tok::Newline,
    PLUS r"^\+" => |_| Tok::Plus,
    STAR r"^\*" => |_| Tok::Star,
    OPEN_PAREN r"^\(" => |_| Tok::OpenParen,
    CLOSE_PAREN r"^\)" => |_| Tok::CloseParen,
    AND r"^&" => |_| Tok::And,
    OR r"^\|" => |_| Tok::Or,
    ARROW_RIGHT r"^->" => |_| Tok::Arrow(Arrow::Right),
    ARROW_BOTH r"^<->" => |_| Tok::Arrow(Arrow::Both),
    NODE "^node" => |_| Tok::Node,
    IDENT r#"^([A-Z]\w*)"# => |t| Tok::Ident(Ident::Normal(t)),
    WILDCARD "^_" => |_| Tok::Ident(Ident::Wildcard),
    ESCAPED r#"^"[^"]*""# => |t: &'a str| Tok::Ident(Ident::Escaped(&t[1..t.len()-1])),
    KEYWORD r"^[a-z]\w*" => |t| Tok::Keyword(t),
    WHITESPACE r"^[ \t\r]+" => |t| Tok::Whitespace(t),
    COMMENT "^#[^\n]*" => |t: &'a str| Tok::Comment(&t[..t.len()-1]),
    UNKNOWN r"^\S+" => |t| Tok::Unk(t),
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

    fn next_normal(&mut self) -> Option<(Tok<'a>, usize)> {
        match next_token(&mut self.text) {
            Some((Tok::Newline, _)) => { // Comment includes newline
                // Ignore newline if next line is empty
                if self.line_empty() {
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
            None if self.indent_stack.len() > 0 => { // Add closing newline and dedents
                self.state = LexState::Newline;
                Some((Tok::Newline, 0))
            },
            t => t
        }
    }

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
            self.state = LexState::Dedent(len - indents);
            self.indent_stack.truncate(indents);
            self.next_token()
        }
        else {
            self.state = LexState::Normal;
            let tok = next_token(&mut self.text);
            if let Some((Tok::Whitespace(space), len)) = tok {
                self.indent_stack.push(space);
                Some((Tok::Indent, len))
            }
            else {
                tok // Cannot be a comment, as line is not empty
            }
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
