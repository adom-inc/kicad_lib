use std::str::FromStr;

/// A trait which represents a type that can be used as a keyword in the
/// [`Lexer`](crate::Lexer). This is mostly just a supertrait of the various
/// required traits, but has some extra requirements. The
/// [`FromStr`] implementation for this type will never be invoked for any
/// strings that start with a delimiter ( `"`, `(`, or `)` ), or contain ASCII
/// whitespace.
pub trait Keyword: core::fmt::Debug + Clone + Copy + PartialEq + FromStr {}

/// The default keyword type which is used if no generic type is specified in
/// the [`Lexer`](crate::Lexer). This type is functionally equivalent to
/// [`Infallible`](core::convert::Infallible).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DefaultKeywords {}

impl FromStr for DefaultKeywords {
    type Err = ();

    fn from_str(_: &str) -> Result<Self, Self::Err> {
        Err(())
    }
}

impl Keyword for DefaultKeywords {}

/// A token which has been parsed from the input source file
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Token<'source, K: Keyword = DefaultKeywords> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub kind: TokenKind<'source, K>,
    pub span: Span,
}

/// A type of token which we can parse
#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(rename_all = "snake_case", tag = "type", content = "value")
)]
pub enum TokenKind<'source, K: Keyword = DefaultKeywords> {
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,

    Keyword(K),
    Symbol(&'source str),
    String(&'source str),
    Number(f32),
}

macro_rules! as_helper {
    ($name:ident, $variant:ident, $ty:ty $(, $tt:tt)?) => {
        paste::paste! {
            pub fn [<as_ $name>](&self) -> Option<$ty> {
              if let Self::$variant(v) = self {
                  Some($($tt)? v)
              } else {
                  None
              }
            }

            #[track_caller]
            pub fn [<expect_ $name>](&self) -> $ty {
                self.[<as_ $name>]().unwrap()
            }
        }
    };
}

impl<'source, K: Keyword> TokenKind<'source, K> {
    as_helper!(keyword, Keyword, K, *);
    as_helper!(symbol, Symbol, &'source str);
    as_helper!(string, String, &'source str);
    as_helper!(number, Number, f32, *);
}

/// A start and end position within the source file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Span {
    /// The index of the first character (inclusive)
    pub start: usize,
    /// The index of the last character (exclusive)
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
