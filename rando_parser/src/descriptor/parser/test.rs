use assert_matches::assert_matches;
use rando_core::algebra::Oolean;

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
        Ok(("", Ok(RuleDefUntyped { .. })))
    );
    assert_matches!(
        parser("rule()\n\t=\n 3  \n"),
        Ok(("", Ok(RuleDefUntyped { .. })))
    );
}

#[test]
fn test_expr() {
    let parser = |input| expr::<RuleBodyTruthy>(input, input);
    assert_matches!(
        parser("true ; false, true ; ool "),
        Ok((" ", Ok(RuleBodyTruthy::Or(_))))
    );
    assert_matches!(parser("1 ; 2, 4 ; 3 "), Ok((" ", Err(e))) => {
        assert_eq!(e.len(), 4);
        for e in &e[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::Truthy,
                actual: Span(_, DescriptorType::County, _)
            });
        }
    });

    let parser = |input| expr::<RuleBodyCounty>(input, input);
    assert_matches!(
        parser("1 ; 2, 4 ; 3 "),
        Ok((" ", Ok(RuleBodyCounty::Max(_))))
    );
    assert_matches!(parser("true ; false, true ; ool "), Ok((" ", Err(e))) => {
        assert_eq!(e.len(), 4);
        for e in &e[..] {
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
        Ok((" ", Ok(RuleBodyTruthy::And(_))))
    );
    assert_matches!(parser("1 , 2 , 3 "), Ok((" ", Err(e))) => {
        for e in &e[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::Truthy,
                actual: Span(_, DescriptorType::County, _)
            });
        }
    });

    let parser = |input| and::<RuleBodyCounty>(input, input);
    assert_matches!(parser("1 , 2 , 3 "), Ok((" ", Ok(RuleBodyCounty::Min(_)))));
    assert_matches!(parser("true , false , ool "), Ok((" ", Err(e))) => {
        for e in &e[..] {
            assert_matches!(e, DescriptorError::Type {
                expected: DescriptorType::County,
                actual: Span(_, DescriptorType::Truthy, _)
            });
        }
    });
}

#[test]
fn test_cmp() {
    let parser = |input| compare::<RuleBodyUntyped>(input, input);
    assert_matches!(parser("true "), Ok((" ", Ok(RuleBodyUntyped::Truthy(_)))));
    assert_matches!(
        parser("2 >= 3 "),
        Ok((
            " ",
            Ok(RuleBodyUntyped::Truthy(RuleBodyTruthy::Compare(..)))
        ))
    );
    assert_matches!(
        parser("2 * 2 >= 3 "),
        Ok((
            " ",
            Ok(RuleBodyUntyped::Truthy(RuleBodyTruthy::Compare(..)))
        ))
    );
    assert_matches!(parser("true >= 2"), Ok(("", Err(e))) => {
        assert_matches!(&e[..], &[DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
}

#[test]
fn test_comb() {
    let parser = |input| comb_start::<RuleBodyUntyped>(input, input);
    assert_matches!(parser("true "), Ok((" ", Ok(RuleBodyUntyped::Truthy(_)))));
    assert_matches!(
        parser("2 * 2 "),
        Ok((
            " ",
            Ok(RuleBodyUntyped::County(RuleBodyCounty::LinearComb(_)))
        ))
    );
    assert_matches!(
        parser("2 + 2 "),
        Ok((
            " ",
            Ok(RuleBodyUntyped::County(RuleBodyCounty::LinearComb(_)))
        ))
    );
    assert_matches!(parser("true * 2"), Ok(("", Err(e))) => {
        assert_matches!(&e[..], &[DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
    assert_matches!(parser("true + 2"), Ok(("", Err(e))) => {
        assert_matches!(&e[..], &[DescriptorError::Type {
            expected: DescriptorType::County,
            actual: Span(_, DescriptorType::Truthy, _)
        }]);
    });
}

#[test]
fn test_item() {
    // Untyped
    let parser = |input| item::<RuleBodyUntyped>(input, input);
    assert_matches!(
        parser("ref() "),
        Ok((" ", Ok(RuleBodyUntyped::Reference(_))))
    );
    assert_matches!(
        parser("12 "),
        Ok((
            " ",
            Ok(RuleBodyUntyped::County(RuleBodyCounty::Constant(12)))
        ))
    );
    assert_matches!(parser("true "), Ok((" ", Ok(RuleBodyUntyped::Truthy(_)))));
    assert_matches!(
        parser("? vAlpha <- rel - gB (true) "),
        Ok((" ", Ok(RuleBodyUntyped::Truthy(_))))
    );
    assert_matches!(
        parser("+ vAlpha <- rel - gB (true) "),
        Ok((" ", Ok(RuleBodyUntyped::County(_))))
    );
    assert_matches!(parser("?[~vA] "), Ok((" ", Ok(RuleBodyUntyped::Truthy(_)))));
    assert_matches!(
        parser("( false ; ool ; true ) "),
        Ok((" ", Ok(RuleBodyUntyped::Or(_))))
    );

    // Truthy
    let parser = |input| item::<RuleBodyTruthy>(input, input);
    assert_matches!(
        parser("ref() "),
        Ok((" ", Ok(RuleBodyTruthy::Reference(_))))
    );
    assert_matches!(
        parser("true "),
        Ok((" ", Ok(RuleBodyTruthy::Constant(Oolean::True))))
    );
    assert_matches!(
        parser("( false ; ool ; true ) "),
        Ok((" ", Ok(RuleBodyTruthy::Or(_))))
    );
    assert_matches!(parser("12 "), Ok((" ", Err(e))) => {
        assert_matches!(&e[..], [DescriptorError::Type {
            expected: DescriptorType::Truthy,
            actual: Span(_, DescriptorType::County, _)
        }]);
    });

    // County
    let parser = |input| item::<RuleBodyCounty>(input, input);
    assert_matches!(
        parser("ref() "),
        Ok((" ", Ok(RuleBodyCounty::Reference(_))))
    );
    assert_matches!(parser("12 "), Ok((" ", Ok(RuleBodyCounty::Constant(12)))));
    assert_matches!(parser("true "), Ok((" ", Err(e))) => {
        assert_matches!(&e[..], [DescriptorError::Type {
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
        Ok((" ", Ok(RuleBodyTruthy::Exists(..))))
    );
    assert_matches!(
        parser("∃ vAlpha <- rel - gB (true) "),
        Ok((" ", Ok(RuleBodyTruthy::Exists(..))))
    );
    assert!(parser("? Alpha <- rel - gB (true) ").is_err());
    assert_matches!(parser("? vAlpha - rel -> gB (3) "), Ok((" ", Err(e))) => {
        assert_matches!(&e[..], [DescriptorError::Type {
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
        Ok((" ", Ok(RuleBodyCounty::Count(..))))
    );
    assert!(parser("+ Alpha <- rel - gB (true) ").is_err());
    assert_matches!(parser("+ vAlpha - rel -> gB (3) "), Ok((" ", Err(e))) => {
        assert_matches!(&e[..], [DescriptorError::Type {
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
            Ok(Relation {
                left_to_right: false,
                name: Span(_, "name", _)
            })
        ))
    );

    assert_matches!(
        parser("-Name-> "),
        Ok((
            " ",
            Ok(Relation {
                left_to_right: true,
                name: Span(_, "Name", _)
            })
        ))
    );
}

#[test]
fn test_state() {
    let parser = |input| state_check(input, input);

    assert_matches!(
        parser("?[ ~vA ] "),
        Ok((" ", Ok(RuleBodyTruthy::CheckPrior(_))))
    );
    assert_matches!(
        parser("![ ~vA ] "),
        Ok((" ", Ok(RuleBodyTruthy::CheckPosterior(_))))
    );
}

#[test]
fn test_access() {
    let input = "[ r ( A, vB, C) ]";
    assert!(access(input, input).is_ok())
}

#[test]
fn test_oolean() {
    assert_eq!(oolean("false"), Ok(("", Ok(Oolean::False))));
    assert_eq!(oolean("true"), Ok(("", Ok(Oolean::True))));
    assert_eq!(oolean("ool"), Ok(("", Ok(Oolean::Ool))));
    assert!(oolean("falsee").is_err())
}

#[test]
fn test_state_body() {
    assert_matches!(state_body("¬vAr"), Ok(("", Ok(StateBody::NotSet(_)))));
    assert_matches!(state_body("~vAr"), Ok(("", Ok(StateBody::NotSet(_)))));
    assert_matches!(state_body("vAr"), Ok(("", Ok(StateBody::Set(_)))));
}

#[test]
fn test_reference() {
    let parser = |input| reference(input, input);

    assert_matches!(parser("ref ( ) "), Ok((" ", Ok(Reference {
        keyword: Span(_, "ref", _),
        values
    }))) => {
        assert!(values.is_empty());
    });

    assert_matches!(parser("r ( gA , vB, C.D ) "), Ok((" ", Ok(Reference {
        keyword: Span(_, "r", _),
        values
    }))) => {
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
        Ok(("", Ok(Value::Var(Ident::Normal("Variable")))))
    );
    assert_matches!(value_name("\"const\""), Ok(("", Ok(Value::Const(_)))));
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
