//! This crate implements a zero-copy lexer for the KiCad S-Expression format
//! which follows closely from the official implemenation. This implementation
//! seeks to be equally fast while supporting better error reporting and
//! ergonomics.

use std::{iter::Peekable, str::Chars};

pub use token::*;

mod token;

/// An error that can occur during lexing
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("{kind} (at {line_number}:{column_number})")]
pub struct Error {
    /// The kind of error which was encountered
    pub kind: ErrorKind,
    /// The line number in the source file associated with the error (different
    /// meaning for different error kinds)
    pub line_number: usize,
    /// The cloumn number in the source file associated with the error
    /// (different meaning for different error kinds)
    pub column_number: usize,
}

/// An kind of error which can be encountered while lexing
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ErrorKind {
    /* Expectations */
    /// We expected to find a `(` token but found something else
    #[error("expected opening parenthesis")]
    ExpectedOpenParen,
    /// We expected to find a `)` token but found something else
    #[error("expected closing parenthesis")]
    ExpectedCloseParen,
    /// We expected to find a generic symbol token but found something else
    #[error("expected a symbol")]
    ExpectedSymbol,
    /// We expected to find a keyword token but found something else
    #[error("expected a keyword")]
    ExpectedKeyword,
    /// We expected to find a delimited string token but found something else
    #[error("expected a string")]
    ExpectedString,
    /// We expected to find a number token but found something else
    #[error("expected a number")]
    ExpectedNumber,
    /* Strings */
    /// A string was opened with a `"`, but we reached the end of the file
    /// before finding a matching unescapted `"`
    #[error("expected string to terminate but reached EOF")]
    UnterminatedString,
}

/// A zero-copy lexer for the KiCad S-Expression format. Lexer implements
/// [`Iterator`] as well as offering various helper functions like
/// [`expect_open_paren`](Self::expect_open_paren) and
/// [`expect_number`](Self::expect_number) for convenient implementation of
/// format parsers.
///
/// Custom keyword lists are supported through the generic argument `K`. Any
/// type which implements [`FromStr`](core::str::FromStr) can be used as the
/// keyword type. If no generic type is specified, the
/// [`DefaultKeywords`] type will be used instead which will never return
/// [`TokenKind::Keyword`].
pub struct Lexer<'source, K: Keyword = DefaultKeywords> {
    source: &'source str,
    position: usize,
    line_number: usize,
    /// We could trivially compute this from the position, but since we may need
    /// to compute it often its best to cache it here
    column: usize,
    chars: Peekable<Chars<'source>>,
    previous: Option<Token<'source, K>>,
    has_errored: bool,
}

impl<'source, K: Keyword> Lexer<'source, K> {
    /// Constructs a new Lexer from a [`SourceFile`]
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            position: 0,
            line_number: 0,
            column: 0,
            previous: None,
            has_errored: false,
        }
    }

    /// Returns true if the lexer has reached the end of it's input stream
    pub fn is_eof(&self) -> bool {
        self.position >= self.source.len()
    }

    /// Returns a reference to the intput stream
    pub fn source(&self) -> &'source str {
        self.source
    }

    /// Returns a 1-based line index for use in position formatting
    pub fn line_number(&self) -> usize {
        self.line_number + 1
    }

    /// Returns a 1-based column index for use in position formatting
    pub fn column(&self) -> usize {
        self.column + 1
    }

    /// Returns the token which was returned by the last call to
    /// [`Self::next`] or `None`.
    pub fn previous(&self) -> Option<&Token<'source, K>> {
        self.previous.as_ref()
    }
}

/* Helper functions for parser implementations */

macro_rules! expect_helper {
    ($name:ident, $error_kind:ident, $pattern:pat) => {
        paste::paste! {
            pub fn [<expect_ $name>](&mut self) -> Result<Token<'source, K>, Error> {
                let error = Error {
                    kind: ErrorKind::$error_kind,
                    line_number: self.line_number,
                    column_number: self.column,
                };

                let token = self.next().ok_or(error.clone())??;

                if !matches!(token.kind, TokenKind::$pattern) {
                    return Err(error);
                }

                Ok(token)
            }
        }
    };
}

impl<'source, K: Keyword> Lexer<'source, K> {
    expect_helper!(open_paren, ExpectedOpenParen, OpenParen);
    expect_helper!(close_paren, ExpectedCloseParen, CloseParen);
    expect_helper!(symbol, ExpectedSymbol, Symbol(_));
    expect_helper!(keyword, ExpectedKeyword, Keyword(_));
    expect_helper!(string, ExpectedString, String(_));
    expect_helper!(number, ExpectedNumber, Number(_));
}

/* Helper functions for lexing the input */

impl<'source, K: Keyword> Lexer<'source, K> {
    fn create_error(&self, kind: ErrorKind) -> Error {
        Error {
            kind,
            line_number: self.line_number(),
            column_number: self.column(),
        }
    }

    fn create_span(&self, start: usize) -> Span {
        Span {
            start,
            end: self.position,
        }
    }

    #[track_caller]
    fn consume_char(&mut self) -> char {
        let c = self.chars.next().unwrap();

        self.column += 1;
        self.position += 1;

        if c == '\n' {
            self.line_number += 1;
            self.column = 0;
        }

        c
    }

    fn ignore_whitespace(&mut self) {
        while let Some(c) = self.chars.peek().copied() {
            if !c.is_ascii_whitespace() {
                break;
            }

            self.consume_char();
        }
    }

    fn ignore_line(&mut self) {
        while let Some(c) = self.chars.peek().copied() {
            if c == '\n' {
                break;
            }

            self.consume_char();
        }
    }

    fn create_token(&self, kind: TokenKind<'source, K>, start: usize) -> Token<'source, K> {
        Token {
            span: self.create_span(start),
            kind,
        }
    }

    fn read_single(&mut self, kind: TokenKind<'source, K>) -> Token<'source, K> {
        let start_position = self.position;

        self.consume_char();

        self.create_token(kind, start_position)
    }

    fn read_string(&mut self) -> Result<Token<'source, K>, Error> {
        let start_position = self.position;

        assert_eq!(self.consume_char(), '"');

        while let Some(c) = self.chars.peek().copied() {
            // Consume chars within the wrapped literal
            self.consume_char();

            // If we encountered an escape sequence, keep going
            if c == '\\' && self.chars.peek().is_some_and(|c| *c == '"') {
                self.consume_char();
            }

            if c == '"' {
                let span = self.create_span(start_position);

                let value = &self.source[span.start..span.end];
                let kind = TokenKind::String(&value[1..value.len() - 1]);

                return Ok(Token { kind, span });
            }
        }

        Err(self.create_error(ErrorKind::UnterminatedString))
    }

    fn read_until_separator(&mut self) -> Span {
        let start_position = self.position;

        while let Some(c) = self.chars.peek().copied() {
            if c.is_ascii_whitespace() || c == '(' || c == ')' {
                break;
            }

            self.consume_char();
        }

        self.create_span(start_position)
    }
}

impl<'source, K: Keyword> Iterator for Lexer<'source, K> {
    type Item = Result<Token<'source, K>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.has_errored {
            return None;
        }

        while let Some(c) = self.chars.peek().copied() {
            let token = match c {
                // Ignore whitespace
                c if c.is_whitespace() => {
                    self.ignore_whitespace();
                    continue;
                }

                // Inline comments
                '#' => {
                    self.ignore_line();
                    continue;
                }

                // List delimiters
                '(' => self.read_single(TokenKind::OpenParen),
                ')' => self.read_single(TokenKind::CloseParen),

                // Try to read a delimited string
                '"' => match self.read_string() {
                    Ok(v) => v,
                    Err(e) => {
                        self.has_errored = true;
                        return Some(Err(e));
                    }
                },

                // All other values
                _ => {
                    let span = self.read_until_separator();
                    let value = &self.source[span.start..span.end];

                    let kind = if let Ok(integer) = value.parse::<i32>() {
                        TokenKind::Number(integer as f32)
                    } else if let Ok(float) = value.parse::<f32>() {
                        TokenKind::Number(float)
                    } else if let Ok(keyword) = value.parse::<K>() {
                        TokenKind::Keyword(keyword)
                    } else {
                        TokenKind::Symbol(value)
                    };

                    Token { kind, span }
                }
            };

            self.previous = Some(token.clone());
            return Some(Ok(token));
        }

        None
    }
}
