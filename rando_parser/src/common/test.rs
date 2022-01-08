use codespan_reporting::diagnostic::Diagnostic;
use nom::{bytes::complete::tag, branch::alt, Err as NomErr};
use assert_matches::assert_matches;

use crate::common::error::cut_custom;

use super::error::{many0_accumulate, RandoError, CommonError, throw, recoverable};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum TestParseError<'a> {
    Common(CommonError<'a>),
    B,
    C,
}

#[test]
fn test_accumulate() {
    let mut parser = many0_accumulate(alt((
        tag("a"),
        cut_custom(throw(tag("b"), |_| TestParseError::B)),
        recoverable(cut_custom(throw(tag("c"), |_| TestParseError::C)))
    )));

    let input = "aaaa";
    assert_eq!(parser(input), Ok(("", vec!["a"; 4])));

    let input = "aabba";
    assert_matches!(parser(input), Err(NomErr::Failure(e)) => {
        assert!(!e.recoverable());
        assert_eq!(e.custom(), &[TestParseError::B]);
        assert_eq!(e.remaining(), "ba")
    });

    let input = "aacc";
    assert_matches!(parser(input), Err(NomErr::Failure(e)) => {
        assert!(e.recoverable());
        assert_eq!(e.custom(), &[TestParseError::C, TestParseError::C]);
        assert_eq!(e.remaining(), "");
    });

    let input = "aacbca";
    assert_matches!(parser(input), Err(NomErr::Failure(e)) => {
        assert!(!e.recoverable());
        assert_eq!(e.custom(), &[TestParseError::C, TestParseError::B]);
        assert_eq!(e.remaining(), "ca");
    });
}

#[test]
fn test_many0_accumulate() {
    let mut parser = many0_accumulate::<_, TestParseError, _>(tag("a"));

    let input = "aaaa";
    assert_eq!(parser(input), Ok(("", vec!["a"; 4])));

    let input = "aaba";
    assert_eq!(parser(input), Ok(("ba", vec!["a"; 2])));

    let input = "baaa";
    assert_eq!(parser(input), Ok(("baaa", vec![])));
}

impl<'a> RandoError<'a> for TestParseError<'a> {
    fn from_common(common: CommonError<'a>) -> Self {
        TestParseError::Common(common)
    }

    fn diagnostic<F: Clone>(&self, _file: &F, _input: &str) -> Diagnostic<F> {
        unimplemented!()
    }
}