//! Symbol library file format (`.kicad_sym` files)

use kicad_sexpr::Sexpr;

use crate::{
    common::symbol::{LibSymbol, LibraryId, SymbolProperty},
    convert::{FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, VecToMaybeSexprVec},
    simple_maybe_from_sexpr, KiCadParseError,
};

/// Stores a collection of symbols which may or may not be derived from other
/// symbols within the library
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SymbolLibraryFile {
    /// The `version` token attribute defines the symbol library version using
    /// the YYYYMMDD date format.
    pub version: u32,
    /// The `generator` token attribute defines the program used to write the file.
    pub generator: String,
    /// The symbol definitions go here. Symbol library files can have zero or more symbols.
    pub symbols: Vec<SymbolDefinition>,
}

impl FromSexpr for SymbolLibraryFile {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("kicad_symbol_lib")?;

        let version = parser.expect_number_with_name("version")? as u32;
        let generator = parser.expect_symbol_with_name("generator")?;
        let symbols = parser.expect_many::<SymbolDefinition>()?;

        parser.expect_end()?;

        Ok(Self {
            version,
            generator,
            symbols,
        })
    }
}

impl ToSexpr for SymbolLibraryFile {
    fn to_sexpr(&self, context: SerializationContext) -> kicad_sexpr::Sexpr {
        Sexpr::list_with_name(
            "kicad_symbol_lib",
            [
                &[
                    Some(Sexpr::number_with_name("version", self.version as f32)),
                    Some(Sexpr::symbol_with_name("generator", &self.generator)),
                ][..],
                &self.symbols.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

/// A symbol definition can be a root symbol or a derived symbol
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum SymbolDefinition {
    RootSymbol(LibSymbol),
    DerivedSymbol(DerivedLibSymbol),
}

impl FromSexpr for SymbolDefinition {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        // We need to look ahead to determine if this is a derived symbol or a
        // root symbol, so we make a copy of the original parser. That way, we
        // can do out initial inspection to determine the symbol type (which
        // consumes tokens), and then send it to the correct FromSexpr
        // implementation later.
        let backup = parser.clone();

        // Both types will have a `symbol` token and a string ID.
        parser.expect_symbol_matching("symbol")?;
        let _id = parser.expect_string()?.parse::<LibraryId>()?;

        // The derived symbol will have an `extends` token and a string ID.
        let extends = parser.maybe_string_with_name("extends")?;

        Ok(match extends {
            Some(_) => Self::DerivedSymbol(DerivedLibSymbol::from_sexpr(backup)?),
            None => Self::RootSymbol(LibSymbol::from_sexpr(backup)?),
        })
    }
}

simple_maybe_from_sexpr!(SymbolDefinition, symbol);

impl ToSexpr for SymbolDefinition {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        match self {
            Self::RootSymbol(symbol) => symbol.to_sexpr(context),
            Self::DerivedSymbol(symbol) => symbol.to_sexpr(context),
        }
    }
}

/// A symbol which has been derived from another (root) symbol within the
/// library
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct DerivedLibSymbol {
    pub id: LibraryId,
    pub extends: String,
    pub properties: Vec<SymbolProperty>,
}

impl FromSexpr for DerivedLibSymbol {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("symbol")?;

        let id = parser.expect_string()?.parse::<LibraryId>()?;
        let extends = parser.expect_string_with_name("extends")?;
        let properties = parser.expect_many::<SymbolProperty>()?;

        parser.expect_end()?;

        Ok(Self {
            id,
            extends,
            properties,
        })
    }
}

impl ToSexpr for DerivedLibSymbol {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "symbol",
            [
                &[
                    Some(self.id.to_sexpr(context)),
                    Some(Sexpr::string_with_name("extends", &self.extends)),
                ][..],
                &self.properties.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}
