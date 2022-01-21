use assert_matches::assert_matches;
use nom::Err as NomErr;

use super::*;
use crate::{descriptor::typecheck::DescriptorType, FullIdent};

#[test]
fn test_rules() {
    assert_matches!(rules("rule()=3\nrule()=true"), Ok(_))
}

#[test]
fn test_rule() {
    let parser = |input| rule(input, input);
    assert_matches!(
        parser("rule(Full, gLob, vAr)\n\t=\n true  "),
        Ok(("", RuleDef { .. }))
    );
    assert_matches!(parser("rule()\n\t=\n 3  \n"), Ok(("", RuleDef { .. })));
}

#[test]
fn test_expr() {
    let parser = |input| expr::<RuleBodyTruthy>(input, input);
    assert_matches!(
        parser("true ; false, true ; ool "),
        Ok((" ", RuleBodyTruthy::Or(_)))
    );
    assert_matches!(parser("1 ; 2, 4 ; 3 "), Err(NomErr::Failure(e)) => {
        assert_eq!(e.custom().len(), 4);
        for e in &e.custom()[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::Truthy,
                actual: Span(_, DescriptorType::County, _)
            });
        }
    });

    let parser = |input| expr::<RuleBodyCounty>(input, input);
    assert_matches!(parser("1 ; 2, 4 ; 3 "), Ok((" ", RuleBodyCounty::Max(_))));
    assert_matches!(parser("true ; false, true ; ool "), Err(NomErr::Failure(e)) => {
        for e in &e.custom()[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::County,
                actual: Span(_, DescriptorType::Truthy, _)
            });
        }
    });
}

#[test]
fn test_and() {
    let parser = |input| and::<RuleBodyTruthy>(input, input);
    assert_matches!(
        parser("true , false , ool "),
        Ok((" ", RuleBodyTruthy::And(_)))
    );
    assert_matches!(parser("1 , 2 , 3 "), Err(NomErr::Failure(e)) => {
        for e in &e.custom()[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::Truthy,
                actual: Span(_, DescriptorType::County, _)
            });
        }
    });

    let parser = |input| and::<RuleBodyCounty>(input, input);
    assert_matches!(parser("1 , 2 , 3 "), Ok((" ", RuleBodyCounty::Min(_))));
    assert_matches!(parser("true , false , ool "), Err(NomErr::Failure(e)) => {
        for e in &e.custom()[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::County,
                actual: Span(_, DescriptorType::Truthy, _)
            });
        }
    });
}

#[test]
fn test_cmp() {
    let parser = |input| compare::<RuleBody>(input, input);
    assert_matches!(parser("true "), Ok((" ", RuleBody::Truthy(_))));
    assert_matches!(
        parser("2 >= 3 "),
        Ok((" ", RuleBody::Truthy(RuleBodyTruthy::Compare(..))))
    );
    assert_matches!(
        parser("2 * 2 >= 3 "),
        Ok((" ", RuleBody::Truthy(RuleBodyTruthy::Compare(..))))
    );
    assert_matches!(parser("true >= 2"), Err(NomErr::Failure(p)) => {
        assert_matches!(&p.custom()[..], &[DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
}

#[test]
fn test_comb() {
    let parser = |input| comb_start::<RuleBody>(input, input);
    assert_matches!(parser("true "), Ok((" ", RuleBody::Truthy(_))));
    assert_matches!(
        parser("2 * 2 "),
        Ok((" ", RuleBody::County(RuleBodyCounty::LinearComb(_))))
    );
    assert_matches!(
        parser("2 + 2 "),
        Ok((" ", RuleBody::County(RuleBodyCounty::LinearComb(_))))
    );
    assert_matches!(parser("true * 2"), Err(NomErr::Failure(p)) => {
        assert_matches!(&p.custom()[..], &[DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
    assert_matches!(parser("true + 2"), Err(NomErr::Failure(p)) => {
        assert_matches!(&p.custom()[..], &[DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
}

#[test]
fn test_item() {
    // Untyped
    let parser = |input| item::<RuleBody>(input, input);
    assert_matches!(parser("ref() "), Ok((" ", RuleBody::Reference(_))));
    assert_matches!(
        parser("12 "),
        Ok((" ", RuleBody::County(RuleBodyCounty::Constant(12))))
    );
    assert_matches!(parser("true "), Ok((" ", RuleBody::Truthy(_))));
    assert_matches!(
        parser("? vAlpha <- rel - gB (true) "),
        Ok((" ", RuleBody::Truthy(_)))
    );
    assert_matches!(
        parser("+ vAlpha <- rel - gB (true) "),
        Ok((" ", RuleBody::County(_)))
    );
    assert_matches!(parser("?[~vA] "), Ok((" ", RuleBody::Truthy(_))));
    assert_matches!(
        parser("( false ; ool ; true ) "),
        Ok((" ", RuleBody::Or(_)))
    );

    // Truthy
    let parser = |input| item::<RuleBodyTruthy>(input, input);
    assert_matches!(parser("ref() "), Ok((" ", RuleBodyTruthy::Reference(_))));
    assert_matches!(
        parser("true "),
        Ok((" ", RuleBodyTruthy::Constant(Oolean::True)))
    );
    assert_matches!(
        parser("( false ; ool ; true ) "),
        Ok((" ", RuleBodyTruthy::Or(_)))
    );
    assert_matches!(parser("12 "), Err(NomErr::Failure(e)) => {
        assert_matches!(&e.custom()[..], [DescriptorError::Type {
            expected: DescriptorType::Truthy,
            actual: Span(_, DescriptorType::County, _)
        }]);
    });

    // County
    let parser = |input| item::<RuleBodyCounty>(input, input);
    assert_matches!(parser("ref() "), Ok((" ", RuleBodyCounty::Reference(_))));
    assert_matches!(parser("12 "), Ok((" ", RuleBodyCounty::Constant(12))));
    assert_matches!(parser("true "), Err(NomErr::Failure(e)) => {
        assert_matches!(&e.custom()[..], [DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
}

#[test]
fn test_exists() {
    let parser = |input| exists(input, input);

    assert_matches!(
        parser("? vAlpha <- rel - gB (true) "),
        Ok((" ", RuleBodyTruthy::Exists(..)))
    );
    assert_matches!(
        parser("∃ vAlpha <- rel - gB (true) "),
        Ok((" ", RuleBodyTruthy::Exists(..)))
    );
    assert!(parser("? Alpha <- rel - gB (true) ").is_err());
    assert_matches!(parser("? vAlpha - rel -> gB (3) "), Err(NomErr::Failure(e)) => {
        assert_matches!(&e.custom()[..], [DescriptorError::Type {
            expected: DescriptorType::Truthy,
            actual: Span(_, DescriptorType::County, _)
        }]);
    });
}

#[test]
fn test_count() {
    let parser = |input| count(input, input);

    assert_matches!(
        parser("+ vAlpha <- rel - gB (true) "),
        Ok((" ", RuleBodyCounty::Count(..)))
    );
    assert!(parser("+ Alpha <- rel - gB (true) ").is_err());
    assert_matches!(parser("+ vAlpha - rel -> gB (3) "), Err(NomErr::Failure(e)) => {
        assert_matches!(&e.custom()[..], [DescriptorError::Type {
            expected: DescriptorType::Truthy,
            actual: Span(_, DescriptorType::County, _)
        }]);
    });
}

#[test]
fn test_relation() {
    let parser = |input| relation(input, input);

    assert_matches!(
        parser("<- name - "),
        Ok((
            " ",
            Relation {
                left_to_right: false,
                name: Span(_, "name", _)
            }
        ))
    );

    assert_matches!(
        parser("-Name-> "),
        Ok((
            " ",
            Relation {
                left_to_right: true,
                name: Span(_, "Name", _)
            }
        ))
    );
}

#[test]
fn test_state() {
    let parser = |input| state_check(input, input);

    assert_matches!(
        parser("?[ ~vA ] "),
        Ok((" ", RuleBodyTruthy::CheckPrior(_)))
    );
    assert_matches!(
        parser("![ ~vA ] "),
        Ok((" ", RuleBodyTruthy::CheckPosterior(_)))
    );
}

#[test]
fn test_access() {
    let input = "[ r ( A, vB, C) ]";
    assert!(access(input, input).is_ok())
}

#[test]
fn test_oolean() {
    assert_eq!(oolean("false"), Ok(("", Oolean::False)));
    assert_eq!(oolean("true"), Ok(("", Oolean::True)));
    assert_eq!(oolean("ool"), Ok(("", Oolean::Ool)));
    assert!(oolean("falsee").is_err())
}

#[test]
fn test_state_body() {
    assert_matches!(state_body("¬vAr"), Ok(("", StateBody::NotSet(_))));
    assert_matches!(state_body("~vAr"), Ok(("", StateBody::NotSet(_))));
    assert_matches!(state_body("vAr"), Ok(("", StateBody::Set(_))));
}

#[test]
fn test_reference() {
    let parser = |input| reference(input, input);

    assert_matches!(parser("ref ( ) "), Ok((" ", Reference {
        keyword: Span(_, "ref", _),
        values
    })) => {
        assert!(values.is_empty());
    });

    assert_matches!(parser("r ( gA , vB, C.D ) "), Ok((" ", Reference {
        keyword: Span(_, "r", _),
        values
    })) => {
        assert_matches!(&values[..], [
            Span(_, Value::Const(FullIdent::Global { .. }), _),
            Span(_, Value::Var(Ident::Normal(_)), _),
            Span(_, Value::Const(FullIdent::Namespaced { .. }), _),
        ]);
    });
}

#[test]
fn test_value() {
    assert_matches!(
        value_name("vVariable"),
        Ok(("", Value::Var(Ident::Normal("Variable"))))
    );
    assert_matches!(value_name("\"const\""), Ok(("", Value::Const(_))));
    assert!(value_name("keyword").is_err())
}

type Err<'a> = ParseError<'a, DescriptorError<'a>>;
#[test]
fn test_cs() {
    let mut parser = cs::<_, _, Err>(char('.'));

    assert_matches!(parser("."), Ok(("", '.')));
    assert_matches!(
        parser(" /* comment */ \n\r\t.\t\r // comment \n "),
        Ok(("", '.'))
    );
    assert_matches!(parser(" . //comment \n abcdefg"), Ok(("abcdefg", '.')));
    assert!(parser("     \n\n\r").is_err())
}
