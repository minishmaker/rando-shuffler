use std::ops::Range;

use nom::{IResult, Parser};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Span<T>(pub usize, pub T, pub usize);

impl<T> Span<T> {
    pub fn limits(spans: &[Self]) -> Option<Span<()>> {
        (spans.len() > 0).then(|| Span(spans[0].0, (), spans[spans.len() - 1].2))
    }

    pub fn including<U>(&self, other: &Span<U>) -> Span<()> {
        if self.0 > other.2 {
            other.including(self)
        } else {
            Span(self.0, (), other.2)
        }
    }

    pub fn map<U, F>(self, f: F) -> Span<U>
    where
        F: FnOnce(T) -> U,
    {
        Span(self.0, f(self.1), self.2)
    }

    pub fn inner(self) -> T {
        self.1
    }

    pub fn range(&self) -> Range<usize> {
        self.0..self.2
    }
}

pub fn substr_index<'a, 'b>(full: &'a str, sub: &'b str) -> Option<usize> {
    assert!(full.len() <= (isize::MAX as usize));
    let full = full.as_bytes().as_ptr_range();
    let sub = sub.as_bytes().as_ptr_range();
    assert!(sub.start <= sub.end && full.start <= full.end);
    if sub.start >= full.start && sub.end <= full.end {
        unsafe {
            // Safety: sub is known to be within full
            Some(sub.start.offset_from(full.start) as usize)
        }
    } else {
        None
    }
}

pub fn span<'a: 'b, 'b, F: 'b, O, E>(
    full: &'a str,
    mut parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Span<O>, E>
where
    F: Parser<&'a str, O, E>,
{
    move |input| {
        let (next, out) = parser.parse(input)?;
        let end = substr_index(full, next).unwrap_or_else(|| {
            eprintln!("full {:p}, input {:p}", full.as_ptr(), next.as_ptr());
            panic!("full and input are from different strings!")
        });
        let len = input.len() - next.len();
        Ok((next, Span(end - len, out, end)))
    }
}


pub fn span_ok<'a: 'b, 'b, F: 'b, T, U, E>(
    full: &'a str,
    mut parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, Result<Span<T>, U>, E>
where
    F: Parser<&'a str, Result<T, U>, E>,
{
    move |input| {
        let (next, out) = parser.parse(input)?;

        let out = out.map(|s| {
            let end = substr_index(full, next).unwrap_or_else(|| {
                eprintln!("full {:p}, input {:p}", full.as_ptr(), next.as_ptr());
                panic!("full and input are from different strings!")
            });
            let len = input.len() - next.len();
            Span(end - len, s, end)
        });
        
        Ok((next, out))
    }
}