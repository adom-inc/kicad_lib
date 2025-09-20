use common::LayerId;
use convert::{FromSexpr, Parser, SerializationContext, ToSexpr};
use footprint_library::FootprintLibraryFile;
use kicad_sexpr::Sexpr;
use netlist::NetlistFile;
use pcb::PcbFile;
use schematic::SchematicFile;
use symbol_library::SymbolLibraryFile;
use thiserror::Error;

pub mod common;
pub mod convert;
pub mod footprint_library;
pub mod netlist;
pub mod pcb;
pub mod schematic;
pub mod symbol_library;

/// The type of an S-expression token without the inner data.
///
/// Used in error messages to indicate what type of S-expression was expected.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SexprKind {
    List,
    Symbol,
    String,
    Number,
}

/// Errors that can occur when parsing KiCad files.
#[derive(Debug, Error, PartialEq, Clone)]
pub enum KiCadParseError {
    #[error(transparent)]
    SexprParseError(#[from] kicad_sexpr::SexprParseError),
    #[error("Unexpected end of list")]
    UnexpectedEndOfList,
    #[error("Expected end of list. Found: {found}")]
    ExpectedEndOfList { found: Sexpr },
    #[error("Unexpected sexpr type. Expected: `{expected:?}`")]
    UnexpectedSexprType { expected: SexprKind },
    #[error("Non-matching symbol. Expected: `{expected}`; Found: `{found}`")]
    NonMatchingSymbol { found: String, expected: String },
    #[error("Invalid Layer definition: `{0}`")]
    InvalidLayer(String),
    #[error("Invalid value `{value}` for enum `{enum_name}`.")]
    InvalidEnumValue {
        value: String,
        enum_name: &'static str,
    },
    #[error("Invalid UUID: {0}")]
    InvalidUuid(#[from] uuid::Error),
    #[error("Incorrect number of points!: Expected: {expected}; Found: {found}")]
    IncorrectNumberOfPoints { found: usize, expected: usize },
    #[error("Found mutually exclusive fields: {field1} and {field2}")]
    FoundMutuallyExclusiveFields { field1: String, field2: String },
    #[error("Expected mutually exclusive field: {field1} or {field2}")]
    ExpectedMutuallyExclusiveField { field1: String, field2: String },
    #[error("Invalid unit identifier: `{0}`")]
    InvalidUnitIdentifier(String),
    #[error("Invalid library identifier: `{0}`")]
    InvalidLibraryIdentifier(String),
    #[error("Layer ID did not match layer id name: `{id}` != `{layer:?}`")]
    NonMatchingLayerId { id: u8, layer: LayerId },
    #[error("Invalid layer bit field: `{raw}` - {error}")]
    InvalidLayerBitField {
        raw: String,
        error: std::num::ParseIntError,
    },
    #[error("Expected field")]
    ExpectedField,
}

impl KiCadParseError {
    pub fn invalid_enum_value<T>(value: impl Into<String>) -> Self {
        Self::InvalidEnumValue {
            value: value.into(),
            enum_name: std::any::type_name::<T>(),
        }
    }
}

macro_rules! simple_to_from_string {
    ($name:ident, $( $string:ident <-> $variant:ident ),+ $(,)?) => {
        impl std::str::FromStr for $name {
            type Err = $crate::KiCadParseError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(match s {
                    $(
                        stringify!($string) => Self::$variant,
                    )*
                    _ => return Err($crate::KiCadParseError::invalid_enum_value::<Self>(s)),
                })
            }
        }

        impl From<$name> for String {
            fn from(value: $name) -> String {
                match value {
                    $(
                        $name::$variant => stringify!($string).to_string(),
                    )*
                }
            }
        }

        impl std::string::ToString for $name {
            fn to_string(&self) -> String {
                match self {
                    $(
                        Self::$variant => stringify!($string).to_string(),
                    )*
                }
            }
        }
    };
}

pub(crate) use simple_to_from_string;

macro_rules! simple_maybe_from_sexpr {
    ($name:ident, $string:ident) => {
        impl $crate::convert::MaybeFromSexpr for $name {
            fn is_present(sexpr: &::kicad_sexpr::SexprList) -> bool {
                sexpr
                    .first_symbol()
                    .is_some_and(|s| s == stringify!($string))
            }
        }
    };
}

pub(crate) use simple_maybe_from_sexpr;

/* File Helper Functions */

fn parse_file<T: FromSexpr>(input: &str) -> Result<T, KiCadParseError> {
    let sexpr = kicad_sexpr::from_str(input)?;

    let Some(list) = sexpr.take_list() else {
        return Err(KiCadParseError::UnexpectedSexprType {
            expected: SexprKind::List,
        });
    };

    T::from_sexpr(Parser::new(list))
}

fn serialize_file<T: ToSexpr>(file: T) -> String {
    let sexpr = file.to_sexpr(SerializationContext::default());

    kicad_sexpr::to_string(&sexpr)
}

/* Exposed APIs */

/// Parses a Footprint Library file from a string.
pub fn parse_footprint_library_file(input: &str) -> Result<FootprintLibraryFile, KiCadParseError> {
    parse_file(input)
}

/// Serializes a Footprint Library file to a string.
pub fn serialize_footprint_library_file(footprint_library: FootprintLibraryFile) -> String {
    serialize_file(footprint_library)
}

/// Parses a Symbol Library file from a string.
pub fn parse_symbol_library_file(input: &str) -> Result<SymbolLibraryFile, KiCadParseError> {
    parse_file(input)
}

/// Serializes a Symbol Library file to a string.
pub fn serialize_symbol_library_file(symbol_library: SymbolLibraryFile) -> String {
    serialize_file(symbol_library)
}

/// Parses a Schematic file from a string.
pub fn parse_schematic_file(input: &str) -> Result<SchematicFile, KiCadParseError> {
    parse_file(input)
}

/// Serializes a Schematic file to a string.
pub fn serialize_schematic_file(schematic: SchematicFile) -> String {
    serialize_file(schematic)
}

/// Parses a PCB file from a string.
pub fn parse_pcb_file(input: &str) -> Result<PcbFile, KiCadParseError> {
    parse_file(input)
}

/// Serializes a PCB file to a string.
pub fn serialize_pcb_file(pcb: PcbFile) -> String {
    serialize_file(pcb)
}

/// Parses a Netlist file from a string.
pub fn parse_netlist_file(input: &str) -> Result<NetlistFile, KiCadParseError> {
    parse_file(input)
}

/// Serializes a Netlist file to a string.
pub fn serialize_netlist_file(netlist: NetlistFile) -> String {
    serialize_file(netlist)
}
