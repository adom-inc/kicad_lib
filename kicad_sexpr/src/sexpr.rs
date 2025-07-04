//! KiCad S-Expression Parser

use std::fmt::Display;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1, satisfy},
    combinator::{eof, recognize},
    multi::{many1, separated_list1},
    sequence::tuple,
    IResult,
};
use thiserror::Error;

use super::number::parse_number;
use super::string::parse_string;

pub type SexprList = Vec<Sexpr>;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum Sexpr {
    List(SexprList),
    Number(f32),
    String(String),
    Symbol(String),
}

impl Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", to_string(self))
    }
}

macro_rules! sexpr_as {
    ($fn_name:ident, $return_type:ty, $variant:ident) => {
        pub fn $fn_name(&self) -> Option<&$return_type> {
            if let Self::$variant(value) = self {
                Some(value)
            } else {
                None
            }
        }
    };
}

macro_rules! sexpr_as_mut {
    ($fn_name:ident, $return_type:ty, $variant:ident) => {
        pub fn $fn_name(&mut self) -> Option<&mut $return_type> {
            if let Self::$variant(value) = self {
                Some(value)
            } else {
                None
            }
        }
    };
}

macro_rules! sexpr_take {
    ($fn_name:ident, $return_type:ty, $variant:ident) => {
        pub fn $fn_name(self) -> Option<$return_type> {
            if let Self::$variant(value) = self {
                Some(value)
            } else {
                None
            }
        }
    };
}

macro_rules! sexpr_construct {
    ($fn_name:ident, $input_type:ty, $variant:ident) => {
        pub fn $fn_name(value: impl Into<$input_type>) -> Self {
            Self::$variant(value.into())
        }
    };
}

macro_rules! sexpr_construct_with_name {
    ($fn_name:ident, $input_type:ty, $variant:ident) => {
        pub fn $fn_name(name: impl Into<String>, value: impl Into<$input_type>) -> Self {
            Self::list([
                Some(Self::symbol(name.into())),
                Some(Self::$variant(value.into())),
            ])
        }
    };
}

#[allow(unused)]
impl Sexpr {
    sexpr_as!(as_list, SexprList, List);
    sexpr_as!(as_number, f32, Number);
    sexpr_as!(as_string, String, String);
    sexpr_as!(as_symbol, String, Symbol);

    sexpr_as_mut!(as_list_mut, SexprList, List);
    sexpr_as_mut!(as_number_mut, f32, Number);
    sexpr_as_mut!(as_string_mut, String, String);
    sexpr_as_mut!(as_symbol_mut, String, Symbol);

    sexpr_take!(take_list, SexprList, List);
    sexpr_take!(take_number, f32, Number);
    sexpr_take!(take_string, String, String);
    sexpr_take!(take_symbol, String, Symbol);

    /// Takes a list of values and constructs a new [`Sexpr::List`]. Crucially,
    /// the list argument takes in an array of [`Option`] values, which
    /// makes it very convenient to serialize a struct with optional fields
    /// into an S-expression.
    ///
    /// # Example
    ///
    /// ```
    /// use kicad_sexpr::Sexpr;
    ///
    /// struct Test {
    ///     a: f32,
    ///     b: Option<f32>,
    ///     c: Option<f32>,
    /// }
    ///
    /// let test = Test {
    ///     a: 1.0,
    ///     b: Some(2.0),
    ///     c: None,
    /// };
    ///
    /// assert_eq!(
    ///     Sexpr::list([
    ///         Some(Sexpr::number(test.a)),
    ///         test.b.map(Sexpr::number),
    ///         test.c.map(Sexpr::number),
    ///     ]),
    ///     Sexpr::List(
    ///         vec![
    ///             Sexpr::Number(1.0),
    ///             Sexpr::Number(2.0),
    ///         ]    
    ///     )
    /// )
    ///
    /// ```
    pub fn list(value: impl Into<Vec<Option<Sexpr>>>) -> Self {
        Self::List(value.into().into_iter().flatten().collect::<Vec<_>>())
    }

    sexpr_construct!(number, f32, Number);
    sexpr_construct!(string, String, String);
    sexpr_construct!(symbol, String, Symbol);

    /// Works the same way as [`Sexpr::list`], but puts the given name
    /// as a [`Sexpr::Symbol`] in the first position of the list.
    ///
    /// # Example
    ///
    /// ```
    /// use kicad_sexpr::Sexpr;
    ///
    /// assert_eq!(
    ///     Sexpr::list_with_name("test", [
    ///         Some(Sexpr::number(1.0)),
    ///         Some(Sexpr::string("hello, world")),
    ///     ]),
    ///     Sexpr::List(
    ///         vec![
    ///             Sexpr::Symbol("test".to_string()),
    ///             Sexpr::Number(1.0),
    ///             Sexpr::String("hello, world".to_string()),
    ///         ]    
    ///     )
    /// )
    ///
    /// ```
    pub fn list_with_name(name: impl Into<String>, values: impl Into<Vec<Option<Sexpr>>>) -> Self {
        let mut list = vec![Some(Self::symbol(name.into()))];
        list.extend(values.into());
        Self::list(list)
    }

    sexpr_construct_with_name!(number_with_name, f32, Number);
    sexpr_construct_with_name!(string_with_name, String, String);
    sexpr_construct_with_name!(symbol_with_name, String, Symbol);

    pub fn bool_with_name(name: impl Into<String>, value: bool) -> Self {
        Self::symbol_with_name(name, if value { "yes" } else { "no" })
    }

    pub fn alt_bool_with_name(name: impl Into<String>, value: bool) -> Self {
        Self::symbol_with_name(name, if value { "true" } else { "false" })
    }
}

#[derive(Debug, Error, PartialEq, Clone)]
#[error("S-expression parse error")]
pub struct SexprParseError;

pub fn from_str(input: &str) -> Result<Sexpr, SexprParseError> {
    let (input, sexpr) = parse_sexpr(input.trim()).map_err(|_| SexprParseError)?;

    if !input.is_empty() {
        return Err(SexprParseError);
    }

    Ok(sexpr)
}

pub fn to_string(sexpr: &Sexpr) -> String {
    to_string_recursive(sexpr, 0)
}

fn to_string_recursive(sexpr: &Sexpr, depth: usize) -> String {
    let mut result = String::new();

    match sexpr {
        Sexpr::List(list) => {
            if depth != 0 {
                result += "\n";
            }

            result.push_str(&" ".repeat(depth * 2));

            result += "(";

            for (i, attribute) in list.iter().enumerate() {
                result += &to_string_recursive(attribute, depth + 1);

                if i != list.len() - 1 {
                    result += " ";
                }
            }

            result += ")";
        }
        Sexpr::Number(number) => {
            if number.fract() == 0.0 {
                result += &(*number as i64).to_string();
            } else {
                result += number.to_string().trim_end_matches('0');
            }
        }
        Sexpr::String(string) => {
            result += &format!("{:?}", string);
        }
        Sexpr::Symbol(symbol) => {
            result += symbol;
        }
    }

    result
}

fn parse_sexpr(input: &str) -> IResult<&str, Sexpr> {
    let (input, sexpr) = alt((
        parse_sexpr_list,
        parse_sexpr_number,
        parse_sexpr_string,
        parse_sexpr_symbol,
    ))(input)?;

    Ok((input, sexpr))
}

fn parse_sexpr_list(input: &str) -> IResult<&str, Sexpr> {
    // Parse opening tag
    let (input, _) = tag("(")(input)?;
    let (input, _) = multispace0(input)?;

    // Parse list children
    let (input, children) = separated_list1(multispace1, parse_sexpr)(input)?;

    // Parse closing tag
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, Sexpr::List(children)))
}

fn parse_sexpr_number(input: &str) -> IResult<&str, Sexpr> {
    // Look ahead to see if the number is terminated by a space or a closing paren
    // If it's not, then it could be a value like a UUID
    let _ = recognize(tuple((parse_number, alt((multispace1, tag(")"), eof)))))(input)?;

    let (input, number) = parse_number(input)?;

    Ok((input, Sexpr::Number(number)))
}

fn parse_sexpr_string(input: &str) -> IResult<&str, Sexpr> {
    let (input, string) = parse_string(input)?;

    Ok((input, Sexpr::String(string)))
}

fn parse_sexpr_symbol(input: &str) -> IResult<&str, Sexpr> {
    let (input, chars) = many1(satisfy(|c| {
        c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '+' || c == '/' || c == '='
    }))(input)?;

    Ok((input, Sexpr::Symbol(chars.iter().collect())))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parsed(input: &str, expected: Sexpr) {
        let (input, sexpr) = parse_sexpr(input).unwrap();

        assert_eq!(input, "");
        assert_eq!(sexpr, expected);
    }

    #[test]
    fn test_parse_empty_list() {
        assert_parsed(
            "(test)",
            Sexpr::List(vec![Sexpr::Symbol("test".to_string())]),
        );
    }

    #[test]
    fn test_parse_empty_list_with_whitespace() {
        assert_parsed(
            "(  test  )",
            Sexpr::List(vec![Sexpr::Symbol("test".to_string())]),
        );
    }

    #[test]
    fn test_parse_number() {
        assert_parsed("123", Sexpr::Number(123.0));
        assert_parsed("-123", Sexpr::Number(-123.0));
        assert_parsed("123.456", Sexpr::Number(123.456));
        assert_parsed("-123.456", Sexpr::Number(-123.456));
    }

    #[test]
    fn test_parse_string() {
        assert_parsed(
            r#""Hello, world!""#,
            Sexpr::String("Hello, world!".to_string()),
        );
        assert_parsed(
            r#""Hello, \"world\"!""#,
            Sexpr::String(r#"Hello, "world"!"#.to_string()),
        );
    }

    #[test]
    fn test_parse_symbol() {
        assert_parsed("yes", Sexpr::Symbol("yes".to_string()));
        assert_parsed(
            "04740ea2-db09-4cc1-b2d4-53506044432e",
            Sexpr::Symbol("04740ea2-db09-4cc1-b2d4-53506044432e".to_string()),
        );
        assert_parsed(
            "2349f563-989d-4999-a369-9f24d984ce74",
            Sexpr::Symbol("2349f563-989d-4999-a369-9f24d984ce74".to_string()),
        );
    }

    #[test]
    fn test_parse_list_with_list_attribute() {
        assert_parsed(
            "(test (nested))",
            Sexpr::List(vec![
                Sexpr::Symbol("test".to_string()),
                Sexpr::List(vec![Sexpr::Symbol("nested".to_string())]),
            ]),
        );
    }

    #[test]
    fn test_parse_list_with_multiple_nested_attributes() {
        assert_parsed(
            "(test (nested_one) (nested_two (nested_three)))",
            Sexpr::List(vec![
                Sexpr::Symbol("test".to_string()),
                Sexpr::List(vec![Sexpr::Symbol("nested_one".to_string())]),
                Sexpr::List(vec![
                    Sexpr::Symbol("nested_two".to_string()),
                    Sexpr::List(vec![Sexpr::Symbol("nested_three".to_string())]),
                ]),
            ]),
        );
    }

    #[test]
    fn test_parse_list_with_number_attribute() {
        assert_parsed(
            "(test 0 0.1)",
            Sexpr::List(vec![
                Sexpr::Symbol("test".to_string()),
                Sexpr::Number(0.0),
                Sexpr::Number(0.1),
            ]),
        );
    }

    #[test]
    fn test_parse_list_with_string_attribute() {
        assert_parsed(
            r#"(test "this is some text" "this is some more text")"#,
            Sexpr::List(vec![
                Sexpr::Symbol("test".to_string()),
                Sexpr::String("this is some text".to_string()),
                Sexpr::String("this is some more text".to_string()),
            ]),
        );
    }

    #[test]
    fn test_parse_list_with_symbol_attribute() {
        assert_parsed(
            r#"(test arbitrary symbol)"#,
            Sexpr::List(vec![
                Sexpr::Symbol("test".to_string()),
                Sexpr::Symbol("arbitrary".to_string()),
                Sexpr::Symbol("symbol".to_string()),
            ]),
        );
    }

    #[test]
    fn test_parse_list_with_uuid_value_attribute() {
        assert_parsed(
            r#"(uuid 2349f563-989d-4999-a369-9f24d984ce74)"#,
            Sexpr::List(vec![
                Sexpr::Symbol("uuid".to_string()),
                Sexpr::Symbol("2349f563-989d-4999-a369-9f24d984ce74".to_string()),
            ]),
        );
    }
}
