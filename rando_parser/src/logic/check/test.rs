use super::*;

#[test]
fn test_descriptor() {
    let keyword = (0, "foo", 3);
    let idents = vec![
        (
            4,
            FullIdent::Global {
                ident: Ident::Normal("Bar"),
            },
            8,
        ),
        (
            9,
            FullIdent::Namespaced {
                idents: vec![Ident::Normal("Baz")],
            },
            12,
        ),
    ];

    let item = Item {
        header: ItemHeader::Node {
            append: false,
            keyword,
            idents: idents.clone(),
        },
        children: vec![],
    };

    let descriptor = convert_descriptor(item).unwrap();
    assert_eq!(descriptor.keyword, keyword);
    assert_eq!(descriptor.idents, idents);
}
