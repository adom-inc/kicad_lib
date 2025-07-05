use kicad_sexpr_lexer::*;

#[track_caller]
fn assert_parsed<K: Keyword>(input: &str, expected: &[Token<K>]) {
    let lexer = Lexer::new(input);

    let tokens = lexer
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|e| panic!("Failed to parse input: {e}"));

    assert_eq!(tokens, expected);
}

#[track_caller]
fn assert_error(input: &str, expected: Error) {
    let lexer: Lexer<'_, DefaultKeywords> = Lexer::new(input);

    let error = lexer
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .expect_err("expected lexer to return an error");

    assert_eq!(error, expected);
}

#[test]
fn test_parse_empty_list() {
    assert_parsed::<DefaultKeywords>(
        "(test)",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(5, 6),
            },
        ],
    );
}

#[test]
fn test_parse_empty_list_with_whitespace() {
    assert_parsed::<DefaultKeywords>(
        "(  test  )",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(3, 7),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(9, 10),
            },
        ],
    );
}

#[test]
fn test_parse_number() {
    assert_parsed::<DefaultKeywords>(
        "123",
        &[Token {
            kind: TokenKind::Number(123.0),
            span: Span::new(0, 3),
        }],
    );
    assert_parsed::<DefaultKeywords>(
        "-123",
        &[Token {
            kind: TokenKind::Number(-123.0),
            span: Span::new(0, 4),
        }],
    );
    assert_parsed::<DefaultKeywords>(
        "123.456",
        &[Token {
            kind: TokenKind::Number(123.456),
            span: Span::new(0, 7),
        }],
    );
    assert_parsed::<DefaultKeywords>(
        "-123.456",
        &[Token {
            kind: TokenKind::Number(-123.456),
            span: Span::new(0, 8),
        }],
    );
}

#[test]
fn test_parse_string() {
    assert_parsed::<DefaultKeywords>(
        r#""Hello, world!""#,
        &[Token {
            kind: TokenKind::String("Hello, world!"),
            span: Span::new(0, 15),
        }],
    );
    assert_parsed::<DefaultKeywords>(
        r#""Hello, \"world!""#,
        &[Token {
            kind: TokenKind::String(r#"Hello, \"world!"#),
            span: Span::new(0, 17),
        }],
    );
}

#[test]
fn test_parse_symbol() {
    assert_parsed::<DefaultKeywords>(
        "yes",
        &[Token {
            kind: TokenKind::Symbol("yes"),
            span: Span::new(0, 3),
        }],
    );
    assert_parsed::<DefaultKeywords>(
        "04740ea2-db09-4cc1-b2d4-53506044432e",
        &[Token {
            kind: TokenKind::Symbol("04740ea2-db09-4cc1-b2d4-53506044432e"),
            span: Span::new(0, 36),
        }],
    );
    assert_parsed::<DefaultKeywords>(
        "2349f563-989d-4999-a369-9f24d984ce74",
        &[Token {
            kind: TokenKind::Symbol("2349f563-989d-4999-a369-9f24d984ce74"),
            span: Span::new(0, 36),
        }],
    );
}

#[test]
fn test_parse_list_with_list_attribute() {
    assert_parsed::<DefaultKeywords>(
        "(test (nested))",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(6, 7),
            },
            Token {
                kind: TokenKind::Symbol("nested"),
                span: Span::new(7, 13),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(13, 14),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(14, 15),
            },
        ],
    );
}

#[test]
fn test_parse_list_with_multiple_nested_attributes() {
    assert_parsed::<DefaultKeywords>(
        "(test (nested_one) (nested_two (nested_three)))",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(6, 7),
            },
            Token {
                kind: TokenKind::Symbol("nested_one"),
                span: Span::new(7, 17),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(17, 18),
            },
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(19, 20),
            },
            Token {
                kind: TokenKind::Symbol("nested_two"),
                span: Span::new(20, 30),
            },
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(31, 32),
            },
            Token {
                kind: TokenKind::Symbol("nested_three"),
                span: Span::new(32, 44),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(44, 45),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(45, 46),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(46, 47),
            },
        ],
    );
}

#[test]
fn test_parse_list_with_number_attribute() {
    assert_parsed::<DefaultKeywords>(
        "(test 0 0.1)",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::Number(0.),
                span: Span::new(6, 7),
            },
            Token {
                kind: TokenKind::Number(0.1),
                span: Span::new(8, 11),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(11, 12),
            },
        ],
    );
}

#[test]
fn test_parse_list_with_string_attribute() {
    assert_parsed::<DefaultKeywords>(
        r#"(test "this is some text" "this is some more text")"#,
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::String("this is some text"),
                span: Span::new(6, 25),
            },
            Token {
                kind: TokenKind::String("this is some more text"),
                span: Span::new(26, 50),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(50, 51),
            },
        ],
    );
}

#[test]
fn test_parse_list_with_symbol_attribute() {
    assert_parsed::<DefaultKeywords>(
        r#"(test arbitrary symbol)"#,
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::Symbol("arbitrary"),
                span: Span::new(6, 15),
            },
            Token {
                kind: TokenKind::Symbol("symbol"),
                span: Span::new(16, 22),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(22, 23),
            },
        ],
    );
}

#[test]
fn test_parse_list_with_uuid_value_attribute() {
    assert_parsed::<DefaultKeywords>(
        r#"(uuid 2349f563-989d-4999-a369-9f24d984ce74)"#,
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("uuid"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::Symbol("2349f563-989d-4999-a369-9f24d984ce74"),
                span: Span::new(6, 42),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(42, 43),
            },
        ],
    );
}

#[test]
fn test_parse_unterminated_string() {
    assert_error(
        r#"(uuid "2349f563-989d-4999-a369-9f24d984ce74)"#,
        Error {
            kind: ErrorKind::UnterminatedString,
            line_number: 1,
            column_number: 45,
        },
    );
}

#[test]
fn test_parse_multiline_string() {
    assert_parsed::<DefaultKeywords>(
        "\"first line\nsecond line\"",
        &[Token {
            kind: TokenKind::String("first line\nsecond line"),
            span: Span::new(0, 24),
        }],
    );
}

#[test]
fn test_strip_comments() {
    assert_parsed::<DefaultKeywords>(
        "(test #)\n)",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(9, 10),
            },
        ],
    );
}

#[test]
fn test_parse_keywords() {
    #[derive(Debug, Clone, Copy, PartialEq, strum::EnumString)]
    #[strum(serialize_all = "snake_case")]
    enum Keywords {
        Uuid,
    }

    impl Keyword for Keywords {}

    assert_parsed::<Keywords>(
        "(test uuid)",
        &[
            Token {
                kind: TokenKind::OpenParen,
                span: Span::new(0, 1),
            },
            Token {
                kind: TokenKind::Symbol("test"),
                span: Span::new(1, 5),
            },
            Token {
                kind: TokenKind::Keyword(Keywords::Uuid),
                span: Span::new(6, 10),
            },
            Token {
                kind: TokenKind::CloseParen,
                span: Span::new(10, 11),
            },
        ],
    );
}
