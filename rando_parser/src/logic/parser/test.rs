use assert_matches::assert_matches;

use crate::{
    common::span::Span,
    logic::{
        ast::Arrow,
        parser::{self, ItemHeader},
    },
    Ident,
};

#[test]
fn test_logic_parser() {
    let input = include_str!("test.logic");
    let logic = parser::parse_items(input);
    assert_matches!(logic, Ok(i) => {
        assert_eq!(i.len(), 2);
        assert_eq!(i[0].children.len(), 2);
        assert_eq!(i[0].children[0].children[0].children.len(), 3);
        assert_eq!(i[0].children[0].children[0].children[0].children.len(), 3);
    })
}

#[test]
fn test_logic_sugar() {
    let input = "(desc A | desc B)";
    let logic = parser::logic_sugar(input, input);
    assert_matches!(
        logic,
        Ok(("", Ok(l))) => {
            assert_eq!(l.children.len(), 2);
            assert_matches!(l.header, ItemHeader::Node { keyword: Span(0, "or", 17), .. })
        }
    );

    let input = "(desc A & desc B & (desc C | desc D))";
    let logic = parser::logic_sugar(input, input);
    assert_matches!(
        logic,
        Ok(("", Ok(l))) => {
            assert_eq!(l.children.len(), 3);
            assert_matches!(l.header, ItemHeader::Node { keyword: Span(0, "and", 37), .. })
        }
    );

    let input = "(desc A & desc B | desc C)";
    let logic = parser::logic_sugar(input, input);
    assert_matches!(logic, Ok(("", Err(_))));
}

#[test]
fn test_node_header() {
    let input = "node A +";
    let header = parser::node_header(input, input);
    assert_matches!(
        header,
        Ok((
            "",
            Ok(ItemHeader::Node {
                append: true,
                keyword: Span(0, "node", 4),
                ..
            })
        ))
    );

    let input = "and";
    let header = parser::node_header(input, input);
    assert_eq!(
        header,
        Ok((
            "",
            Ok(ItemHeader::Node {
                append: false,
                keyword: Span(0, "and", 3),
                idents: Vec::new()
            })
        ))
    );

    let input = "foo Bar.Baz gQux";
    let header = parser::node_header(input, input);
    assert_matches!(
        header,
        Ok((
            "",
            Ok(ItemHeader::Node {
                append: false,
                keyword: Span(0, "foo", 3),
                ..
            })
        ))
    );
}

#[test]
fn test_edge_header() {
    let input = "A        -> B";
    let header = parser::edge_header(input, input);
    assert_eq!(
        header,
        Ok((
            "",
            Ok(ItemHeader::Edge {
                left: Span(0, Ident::Normal("A"), 1),
                arrow: Span(9, Arrow::Right, 11),
                right: Span(12, Ident::Normal("B"), 13)
            })
        ))
    );
}

#[test]
fn test_arrow() {
    let arrow = parser::arrow("<-");
    assert_eq!(arrow, Ok(("", Ok(Arrow::Left))));

    let arrow = parser::arrow("->");
    assert_eq!(arrow, Ok(("", Ok(Arrow::Right))));

    let arrow = parser::arrow("<->");
    assert_eq!(arrow, Ok(("", Ok(Arrow::Both))));

    let arrow = parser::arrow("-");
    assert_matches!(arrow, Err(_));
}

#[test]
fn test_comment_line_end() {
    let line = parser::comment_line_end("     \t\n");
    assert_eq!(line, Ok(("", Ok(None))));

    let line = parser::comment_line_end("     \t#Comment\n");
    assert_eq!(line, Ok(("", Ok(Some("Comment")))));

    let line = parser::comment_line_end("     apple\n");
    assert_matches!(line, Err(_));

    let line = parser::comment_line_end("     \t");
    assert_eq!(line, Ok(("", Ok(None))));
}
