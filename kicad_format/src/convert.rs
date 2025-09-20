//! Conversion traits and utilities for converting between KiCad's S-expressions
//! and Rust types.
//!
//! This module provides traits like [`FromSexpr`] and [`ToSexpr`]

use std::{iter::Peekable, vec::IntoIter};

use kicad_sexpr::{Sexpr, SexprList};

use crate::{KiCadParseError, SexprKind};

/* ========= DESERIALIZATION ========= */

pub trait FromSexpr: Sized {
    fn from_sexpr(parser: Parser) -> Result<Self, KiCadParseError>;
}

pub trait MaybeFromSexpr {
    fn is_present(sexpr: &SexprList) -> bool;
}

pub trait FromSexprWithName: Sized {
    fn from_sexpr_with_name(parser: Parser, name: &str) -> Result<Self, KiCadParseError>;
}

pub trait MaybeFromSexprWithName {
    fn is_present_with_name(sexpr: &SexprList, name: &str) -> bool {
        sexpr.first_symbol().is_some_and(|s| s == name)
    }
}

pub trait SexprListExt {
    fn first_symbol(&self) -> Option<&str>;
}

impl SexprListExt for SexprList {
    fn first_symbol(&self) -> Option<&str> {
        self.first()?.as_symbol().map(|s| s.as_str())
    }
}

/// A parser used internally for easily lifting fields out of S-expression trees.
#[derive(Debug, Clone)]
pub struct Parser {
    inner: Peekable<IntoIter<Sexpr>>,
}

impl Parser {
    pub fn new(inner: SexprList) -> Self {
        Self {
            inner: inner.into_iter().peekable(),
        }
    }

    fn expect_next(&mut self) -> Result<Sexpr, KiCadParseError> {
        self.inner
            .next()
            .ok_or(KiCadParseError::UnexpectedEndOfList)
    }

    pub fn peek_next(&mut self) -> Option<&Sexpr> {
        self.inner.peek()
    }

    /// Expects the next sexpr to be a list and returns a parser for it.
    ///
    /// If the next sexpr is not a list, an error is returned.
    pub fn expect_list(&mut self) -> Result<Parser, KiCadParseError> {
        let next = self.expect_next()?;

        let Sexpr::List(list) = next else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::List,
            });
        };

        Ok(Parser::new(list))
    }

    /// Expects the next sexpr to be a list with the given string matching the
    /// first symbol in the list, returning a parser for it.
    ///
    /// If the next sexpr is not a list or the first symbol does not match, an
    /// error is returned.
    pub fn expect_list_with_name(&mut self, name: &str) -> Result<Parser, KiCadParseError> {
        let mut list = self.expect_list()?;
        list.expect_symbol_matching(name)?;

        Ok(list)
    }

    /// Expects the next sexpr to be a symbol and returns it.
    ///
    /// If the next sexpr is not a symbol, an error is returned.
    pub fn expect_symbol(&mut self) -> Result<String, KiCadParseError> {
        let next = self.expect_next()?;

        let Sexpr::Symbol(symbol) = next else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::Symbol,
            });
        };

        Ok(symbol)
    }

    pub fn expect_symbol_with_name(&mut self, name: &str) -> Result<String, KiCadParseError> {
        self.expect_list_with_name(name)?.expect_symbol()
    }

    /// Expects the next sexpr to be a symbol and checks if it matches the
    /// expected symbol.
    ///
    /// If the next sexpr is not a symbol or it does not match the expected
    /// symbol, an error is returned.
    pub fn expect_symbol_matching(&mut self, expected: &str) -> Result<(), KiCadParseError> {
        let symbol = self.expect_symbol()?;

        if symbol != expected {
            return Err(KiCadParseError::NonMatchingSymbol {
                found: symbol,
                expected: expected.into(),
            });
        }

        Ok(())
    }

    pub fn expect_symbol_matching_any(
        &mut self,
        expected: &[&str],
    ) -> Result<String, KiCadParseError> {
        let symbol = self.expect_symbol()?;

        if !expected.contains(&symbol.as_str()) {
            return Err(KiCadParseError::NonMatchingSymbol {
                found: symbol,
                expected: expected.join(", "),
            });
        }

        Ok(symbol)
    }

    /// Expects the next sexpr to be a string and returns it.
    ///
    /// If the next sexpr is not a string, an error is returned.
    pub fn expect_string(&mut self) -> Result<String, KiCadParseError> {
        let next = self.expect_next()?;

        let Sexpr::String(string) = next else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::String,
            });
        };

        Ok(string)
    }

    pub fn expect_string_with_name(&mut self, name: &str) -> Result<String, KiCadParseError> {
        self.expect_list_with_name(name)?.expect_string()
    }

    pub fn expect_symbol_or_string_with_name(
        &mut self,
        name: &str,
    ) -> Result<String, KiCadParseError> {
        let next = self.expect_list_with_name(name)?.expect_next()?;
        match next {
            Sexpr::String(string) => Ok(string),
            Sexpr::Symbol(string) => Ok(string),
            _ => Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::String,
            }),
        }
    }

    /// Expects the next sexpr to be a number and returns it.
    ///
    /// If the next sexpr is not a number, an error is returned.
    pub fn expect_number(&mut self) -> Result<f32, KiCadParseError> {
        let next = self.expect_next()?;

        let Sexpr::Number(number) = next else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::Number,
            });
        };

        Ok(number)
    }

    pub fn expect_bool(&mut self) -> Result<bool, KiCadParseError> {
        let symbol = self.expect_symbol()?;
        match symbol.as_str() {
            "yes" => Ok(true),
            "no" => Ok(false),
            _ => Err(KiCadParseError::InvalidEnumValue {
                value: symbol,
                enum_name: "bool",
            }),
        }
    }

    pub fn expect_number_with_name(&mut self, name: &str) -> Result<f32, KiCadParseError> {
        self.expect_list_with_name(name)?.expect_number()
    }

    pub fn expect_bool_with_name(&mut self, name: &str) -> Result<bool, KiCadParseError> {
        let result = self.expect_symbol_with_name(name)?;

        match result.as_str() {
            "yes" => Ok(true),
            "no" => Ok(false),
            _ => Err(KiCadParseError::InvalidEnumValue {
                value: result,
                enum_name: "bool",
            }),
        }
    }

    pub fn expect_alt_bool_with_name(&mut self, name: &str) -> Result<bool, KiCadParseError> {
        let result = self.expect_symbol_with_name(name)?;

        match result.as_str() {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => Err(KiCadParseError::InvalidEnumValue {
                value: result,
                enum_name: "bool",
            }),
        }
    }

    /// Expects the end of the list.
    pub fn expect_end(mut self) -> Result<(), KiCadParseError> {
        if let Some(next) = self.inner.next() {
            return Err(KiCadParseError::ExpectedEndOfList { found: next });
        }

        Ok(())
    }

    /// Expects the next sexpr to be of a specific type and returns it.
    pub fn expect<T>(&mut self) -> Result<T, KiCadParseError>
    where
        T: FromSexpr,
    {
        T::from_sexpr(self.expect_list()?)
    }

    pub fn maybe<T>(&mut self) -> Result<Option<T>, KiCadParseError>
    where
        T: FromSexpr + MaybeFromSexpr,
    {
        // If there are no more tokens, return None
        let Some(sexpr) = self.peek_next() else {
            return Ok(None);
        };

        // If the next sexpr is not a list, return None
        let Sexpr::List(list) = sexpr else {
            return Ok(None);
        };

        T::is_present(list).then(|| self.expect::<T>()).transpose()
    }

    pub fn expect_with_name<T>(&mut self, name: &str) -> Result<T, KiCadParseError>
    where
        T: FromSexprWithName,
    {
        T::from_sexpr_with_name(self.expect_list()?, name)
    }

    pub fn maybe_with_name<T>(&mut self, name: &str) -> Result<Option<T>, KiCadParseError>
    where
        T: FromSexprWithName + MaybeFromSexprWithName,
    {
        // If there are no more tokens, return None
        let Some(sexpr) = self.peek_next() else {
            return Ok(None);
        };

        // If the next sexpr is not a list, return None
        let Sexpr::List(list) = sexpr else {
            return Ok(None);
        };

        T::is_present_with_name(list, name)
            .then(|| self.expect_with_name::<T>(name))
            .transpose()
    }

    pub fn expect_many<T>(&mut self) -> Result<Vec<T>, KiCadParseError>
    where
        T: FromSexpr + MaybeFromSexpr,
    {
        let mut result = Vec::new();

        while let Some(item) = self.maybe::<T>()? {
            result.push(item);
        }

        Ok(result)
    }

    pub fn expect_many_symbols(&mut self) -> Result<Vec<String>, KiCadParseError> {
        let mut result = Vec::new();

        while let Some(symbol) = self.maybe_symbol() {
            result.push(symbol);
        }

        Ok(result)
    }

    pub fn expect_many_strings(&mut self) -> Result<Vec<String>, KiCadParseError> {
        let mut result = Vec::new();

        while let Some(string) = self.maybe_string() {
            result.push(string);
        }

        Ok(result)
    }

    /// Returns a parser for the next sexpr if it is a list.
    ///
    /// If the next sexpr is not a list or there are no more tokens in the
    /// parser, `None` is returned.
    pub fn maybe_list(&mut self) -> Option<Parser> {
        let next = self.peek_next()?;

        let Sexpr::List(_) = next else {
            return None;
        };

        Some(self.expect_list().unwrap())
    }

    /// Returns the next sexpr as a symbol if it is a symbol.
    ///
    /// If the next sexpr is not a symbol or there are no more tokens in the
    /// parser, `None` is returned.
    pub fn maybe_symbol(&mut self) -> Option<String> {
        let next = self.peek_next()?;

        let Sexpr::Symbol(_) = next else {
            return None;
        };

        Some(self.expect_symbol().unwrap())
    }

    /// Returns the next sexpr as a string if it is a string.
    ///
    /// If the next sexpr is not a string or there are no more tokens in the
    /// parser, `None` is returned.
    pub fn maybe_string(&mut self) -> Option<String> {
        let next = self.peek_next()?;

        let Sexpr::String(_) = next else {
            return None;
        };

        Some(self.expect_string().unwrap())
    }

    /// Returns the next sexpr as a number if it is a number.
    ///
    /// If the next sexpr is not a number or there are no more tokens in the
    /// parser, `None` is returned.
    pub fn maybe_number(&mut self) -> Option<f32> {
        let next = self.peek_next()?;

        let Sexpr::Number(_) = next else {
            return None;
        };

        Some(self.expect_number().unwrap())
    }

    pub fn maybe_list_with_name(&mut self, name: &str) -> Option<Parser> {
        let next = self.peek_next()?;

        let Sexpr::List(list) = next else {
            return None;
        };

        if list.is_empty() {
            return None;
        }

        let first = &list[0];

        let Sexpr::Symbol(symbol) = first else {
            return None;
        };

        if symbol != name {
            return None;
        }

        Some(self.expect_list_with_name(name).unwrap())
    }

    pub fn maybe_empty_list_with_name(&mut self, name: &str) -> Result<bool, KiCadParseError> {
        Ok(self
            .maybe_list_with_name(name)
            .map(|p| {
                p.expect_end()?;
                Ok::<_, KiCadParseError>(())
            })
            .transpose()?
            .is_some())
    }

    pub fn maybe_symbol_with_name(
        &mut self,
        name: &str,
    ) -> Result<Option<String>, KiCadParseError> {
        self.maybe_list_with_name(name)
            .map(|mut d| d.expect_symbol())
            .transpose()
    }

    pub fn maybe_string_with_name(
        &mut self,
        name: &str,
    ) -> Result<Option<String>, KiCadParseError> {
        self.maybe_list_with_name(name)
            .map(|mut d| d.expect_string())
            .transpose()
    }

    pub fn maybe_number_with_name(&mut self, name: &str) -> Result<Option<f32>, KiCadParseError> {
        self.maybe_list_with_name(name)
            .map(|mut d| d.expect_number())
            .transpose()
    }

    pub fn maybe_bool_with_name(&mut self, name: &str) -> Result<Option<bool>, KiCadParseError> {
        self.maybe_list_with_name(name)
            .map(|mut d| d.expect_bool())
            .transpose()
    }

    pub fn maybe_symbol_matching(&mut self, expected: &str) -> bool {
        let Some(symbol) = self.peek_symbol() else {
            return false;
        };

        if symbol != expected {
            return false;
        }

        self.expect_symbol().unwrap();

        true
    }

    pub fn maybe_symbol_or_boolean_with_name(&mut self, name: &str) -> bool {
        if self.maybe_symbol_matching(name) {
            return true;
        }

        if let Ok(Some(value)) = self.maybe_bool_with_name(name) {
            return value;
        }

        false
    }

    pub fn maybe_symbol_with_optional_boolean(&mut self, name: &str) -> bool {
        let result = self
            .maybe_list_with_name(name)
            .map(|mut d| {
                let next = d.maybe_symbol();
                if let Some(symbol) = next {
                    return match symbol.as_str() {
                        "yes" => Ok(true),
                        "no" => Ok(false),
                        _ => Err(KiCadParseError::InvalidEnumValue {
                            value: symbol,
                            enum_name: "bool",
                        }),
                    };
                }

                Ok(true)
            })
            .transpose();

        match result {
            Ok(value) => value.unwrap_or(false),
            Err(_) => false,
        }
    }

    pub fn maybe_symbol_matching_any(&mut self, expected: &[&str]) -> Option<String> {
        let symbol = self.peek_symbol()?;

        if !expected.contains(&symbol) {
            return None;
        }

        Some(self.expect_symbol().unwrap())
    }

    pub fn peek_list(&mut self) -> Option<&SexprList> {
        self.peek_next()?.as_list()
    }

    pub fn peek_symbol(&mut self) -> Option<&str> {
        self.peek_next()?.as_symbol().map(|s| s.as_str())
    }
}

/* ========= SERIALIZATION ========= */

// TODO: investigate a syntax like this:
//
// Sexpr::list_with_name("symbol", |s| {
//     s.append(&self.id.to_sexpr());
//     s.append_bool_with_name("power", self.power);
//     s.append_bool_with_name("hide_pin_numbers", self.hide_pin_numbers);
//     s.append_maybe("pin_names", &self.pin_names);
//     s.append_bool_with_name("in_bom", self.in_bom);
//     s.append_bool_with_name("on_board", self.on_board);
//     s.append_many("properties", &self.properties);
//     s.append_many("units", &self.units);
// })

// The KiCad file spec has evolved over time, and some serialization tasks require detecting these
// quirks for 1:1 file recreation. This struct allows us to pass important metadata from the top
// level (where detection usually happens) to child structs.
#[derive(Debug, Clone, Copy)]
pub struct SerializationContext {
    pub uuids_as_strings: bool,
    pub explicit_booleans: bool,
}

impl SerializationContext {
    pub fn pre_v8() -> Self {
        Self {
            uuids_as_strings: false,
            explicit_booleans: false,
        }
    }
}

impl Default for SerializationContext {
    fn default() -> Self {
        SerializationContext {
            uuids_as_strings: true,
            explicit_booleans: true,
        }
    }
}

pub trait ToSexpr {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr;
}

pub trait ToSexprWithName {
    fn to_sexpr_with_name(&self, name: &str, context: SerializationContext) -> Sexpr;
}

pub trait VecToMaybeSexprVec {
    fn into_sexpr_vec(self, context: SerializationContext) -> Vec<Option<Sexpr>>;
}

impl<T> VecToMaybeSexprVec for &[T]
where
    T: ToSexpr,
{
    fn into_sexpr_vec(self, context: SerializationContext) -> Vec<Option<Sexpr>> {
        self.iter()
            .map(|i| i.to_sexpr(context))
            .map(Option::Some)
            .collect()
    }
}
