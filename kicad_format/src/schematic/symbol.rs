//! Common structures related to symbols within a schematic.
//!
//! Not to be confused with [`LibSymbol`](crate::common::symbol::LibSymbol),
//! [`DerivedLibSymbol`](crate::symbol_library::DerivedLibSymbol), or
//! [`LibSymbolSubUnit`](crate::common::symbol::LibSymbolSubUnit).

use kicad_sexpr::Sexpr;

use crate::{
    common::{
        symbol::{LibraryId, SymbolProperty},
        Position, Uuid,
    },
    convert::{FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, VecToMaybeSexprVec},
    simple_maybe_from_sexpr, KiCadParseError,
};

/// A symbol instance within a schematic
///
/// Not to be confused with [`LibSymbol`](crate::common::symbol::LibSymbol),
/// [`DerivedLibSymbol`](crate::symbol_library::DerivedLibSymbol), or
/// [`LibSymbolSubUnit`](crate::common::symbol::LibSymbolSubUnit).
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub lib_name: Option<String>,
    pub lib_id: LibraryId,
    pub position: Position,
    pub mirror: Option<Mirror>,
    pub unit: u16,
    pub exclude_from_sim: Option<bool>,
    pub convert: Option<SymbolConversion>,
    pub in_bom: bool,
    pub on_board: bool,
    pub dnp: bool,
    pub fields_autoplaced: bool,
    pub uuid: Uuid,
    pub properties: Vec<SymbolProperty>,
    pub pins: Vec<Pin>,
    pub instances: Option<Vec<SymbolInstanceProject>>,
}

impl FromSexpr for Symbol {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("symbol")?;

        let lib_name = parser.maybe_string_with_name("lib_name")?;
        let lib_id = parser
            .expect_string_with_name("lib_id")?
            .parse::<LibraryId>()?;
        let position = parser.expect::<Position>()?;
        let mirror = parser.maybe::<Mirror>()?;
        let unit = parser.expect_number_with_name("unit")? as u16;
        let exclude_from_sim = parser.maybe_bool_with_name("exclude_from_sim")?;
        let convert = parser
            .maybe_number_with_name("convert")?
            .map(|c| c as u8)
            .map(TryFrom::try_from)
            .transpose()?;
        let in_bom = parser.expect_bool_with_name("in_bom")?;
        let on_board = parser.expect_bool_with_name("on_board")?;
        let dnp = parser.expect_bool_with_name("dnp")?;
        let fields_autoplaced = parser.maybe_symbol_with_optional_boolean("fields_autoplaced");
        let uuid = parser.expect::<Uuid>()?;
        let properties = parser.expect_many::<SymbolProperty>()?;
        let pins = parser.expect_many::<Pin>()?;
        let instances = parser
            .maybe_list_with_name("instances")
            .map(|mut p| {
                let instances = p.expect_many::<SymbolInstanceProject>()?;
                p.expect_end()?;

                Ok::<_, KiCadParseError>(instances)
            })
            .transpose()?;

        parser.expect_end()?;

        Ok(Self {
            lib_name,
            lib_id,
            position,
            mirror,
            unit,
            exclude_from_sim,
            convert,
            in_bom,
            on_board,
            dnp,
            fields_autoplaced,
            uuid,
            properties,
            pins,
            instances,
        })
    }
}

simple_maybe_from_sexpr!(Symbol, symbol);

impl ToSexpr for Symbol {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        // Around v8, a normalization effort took place and many core fields are serialized
        // differently. The existence of the `exclude_from_sim` field is a signal that we're
        // working with the updated format, and can follow those rules instead.
        let is_normalized = self.exclude_from_sim.is_some();
        let context = if is_normalized {
            SerializationContext::default()
        } else {
            SerializationContext::pre_v8()
        };

        let exclude_from_sim = self
            .exclude_from_sim
            .map(|value| Sexpr::bool_with_name("exclude_from_sim", value));

        let fields_autoplaced = if context.explicit_booleans {
            self.fields_autoplaced
                .then(|| Sexpr::bool_with_name("fields_autoplaced", self.fields_autoplaced))
        } else {
            self.fields_autoplaced
                .then(|| Sexpr::list_with_name("fields_autoplaced", []))
        };

        Sexpr::list_with_name(
            "symbol",
            [
                &[
                    self.lib_name
                        .as_ref()
                        .map(|n| Sexpr::string_with_name("lib_name", n)),
                    Some(Sexpr::list_with_name(
                        "lib_id",
                        [Some(self.lib_id.to_sexpr(context))],
                    )),
                    Some(self.position.to_sexpr(context)),
                    self.mirror
                        .as_ref()
                        .filter(|m| m.x || m.y)
                        .map(|i| i.to_sexpr(context)),
                    Some(Sexpr::number_with_name("unit", self.unit as f32)),
                    self.convert
                        .map(|c| Sexpr::number_with_name("convert", c as u8 as f32)),
                    exclude_from_sim,
                    Some(Sexpr::bool_with_name("in_bom", self.in_bom)),
                    Some(Sexpr::bool_with_name("on_board", self.on_board)),
                    Some(Sexpr::bool_with_name("dnp", self.dnp)),
                    fields_autoplaced,
                    Some(self.uuid.to_sexpr(context)),
                ][..],
                &self.properties.into_sexpr_vec(context),
                &self.pins.into_sexpr_vec(context),
                &[self
                    .instances
                    .as_ref()
                    .map(|i| Sexpr::list_with_name("instances", i.into_sexpr_vec(context)))][..],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Mirror {
    pub x: bool,
    pub y: bool,
}

impl FromSexpr for Mirror {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("mirror")?;

        let x = parser.maybe_symbol_matching("x");
        let y = parser.maybe_symbol_matching("y");

        parser.expect_end()?;

        Ok(Self { x, y })
    }
}

simple_maybe_from_sexpr!(Mirror, mirror);

impl ToSexpr for Mirror {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "mirror",
            [
                self.x.then(|| Sexpr::symbol("x")),
                self.y.then(|| Sexpr::symbol("y")),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum SymbolConversion {
    Base = 1,
    Demorgan = 2,
}

impl TryFrom<u8> for SymbolConversion {
    type Error = KiCadParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Base),
            2 => Ok(Self::Demorgan),
            _ => Err(KiCadParseError::invalid_enum_value::<Self>(
                value.to_string(),
            )),
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Pin {
    pub number: String,
    pub uuid: Uuid,
    pub alternate: Option<String>,
}

impl FromSexpr for Pin {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pin")?;

        let number = parser.expect_string()?;
        let uuid = parser.expect::<Uuid>()?;
        let alternate = parser.maybe_string_with_name("alternate")?;

        parser.expect_end()?;

        Ok(Self {
            number,
            uuid,
            alternate,
        })
    }
}

simple_maybe_from_sexpr!(Pin, pin);

impl ToSexpr for Pin {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "pin",
            [
                Some(Sexpr::string(&self.number)),
                Some(self.uuid.to_sexpr(context)),
                self.alternate
                    .as_ref()
                    .map(|a| Sexpr::string_with_name("alternate", a)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SymbolInstanceProject {
    pub project: String,
    pub instances: Vec<SymbolInstance>,
}

impl FromSexpr for SymbolInstanceProject {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("project")?;

        let project = parser.expect_string()?;

        let instances = parser.expect_many::<SymbolInstance>()?;

        parser.expect_end()?;

        Ok(Self { project, instances })
    }
}

simple_maybe_from_sexpr!(SymbolInstanceProject, project);

impl ToSexpr for SymbolInstanceProject {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "project",
            [
                &[Some(Sexpr::string(&self.project))][..],
                &self.instances.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SymbolInstance {
    pub path: String,
    pub reference: String,
    pub unit: u16,
}

impl FromSexpr for SymbolInstance {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("path")?;

        let path = parser.expect_string()?;
        let reference = parser.expect_string_with_name("reference")?;
        let unit = parser.expect_number_with_name("unit")? as u16;

        parser.expect_end()?;

        Ok(Self {
            path,
            reference,
            unit,
        })
    }
}

simple_maybe_from_sexpr!(SymbolInstance, path);

impl ToSexpr for SymbolInstance {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "path",
            [
                Some(Sexpr::string(&self.path)),
                Some(Sexpr::string_with_name("reference", &self.reference)),
                Some(Sexpr::number_with_name("unit", self.unit as f32)),
            ],
        )
    }
}
