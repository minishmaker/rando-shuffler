use assert_matches::assert_matches;
use codespan_reporting::diagnostic::Diagnostic;
use nom::{branch::alt, bytes::complete::tag, combinator::map_res, Err as NomErr, Parser};

use super::error::{many0_accumulate, CommonError, RandoError};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum TestParseError<'a> {
    Common(CommonError<'a>),
    B,
    C,
}

#[test]
fn test_accumulate() {
    let mut parser = many0_accumulate(alt((
        tag("a").map(Ok),
        map_res(tag("b"), |_| Err(TestParseError::B)),
        tag("c").map(|_| Err(vec![TestParseError::C])),
    )));

    let input = "aaaa";
    assert_eq!(parser(input), Ok(("", Ok(vec!["a"; 4]))));

    let input = "aabba";
    assert_matches!(parser(input), Err(NomErr::Failure(e)) => {
        assert_eq!(e.custom(), &[TestParseError::B]);
    });

    let input = "aacc";
    assert_matches!(parser(input), Ok(("", Err(e))) => {
        assert_eq!(&e[..], &[TestParseError::C, TestParseError::C]);
    });

    let input = "aacbca";
    assert_matches!(parser(input), Err(NomErr::Failure(e)) => {
        assert_matches!(e.custom(), &[.., TestParseError::B]);
    });
}

#[test]
fn test_many0_accumulate() {
    let mut parser = many0_accumulate::<_, TestParseError, _>(tag("a").map(Ok));

    let input = "aaaa";
    assert_eq!(parser(input), Ok(("", Ok(vec!["a"; 4]))));

    let input = "aaba";
    assert_eq!(parser(input), Ok(("ba", Ok(vec!["a"; 2]))));

    let input = "baaa";
    assert_eq!(parser(input), Ok(("baaa", Ok(vec![]))));
}

impl<'a> RandoError<'a> for TestParseError<'a> {
    fn from_common(common: CommonError<'a>) -> Self {
        TestParseError::Common(common)
    }

    fn diagnostic<F: Clone>(&self, _file: &F, _input: &str) -> Diagnostic<F> {
        unimplemented!()
    }
}
