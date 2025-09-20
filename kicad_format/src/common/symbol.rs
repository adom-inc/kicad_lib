//! Common structures related to symbols within a symbol library or a
//! schematic.

use std::{fmt::Display, str::FromStr};

use kicad_sexpr::{Sexpr, SexprList};
use regex::Regex;

use crate::{
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName, VecToMaybeSexprVec,
    },
    simple_maybe_from_sexpr, simple_to_from_string, KiCadParseError, SexprKind,
};

use super::{
    shape::{Shape, ShapeFillMode},
    Position, Stroke, TextEffects, Vec2D,
};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LibSymbol {
    pub id: LibraryId,
    pub power: bool,
    pub hide_pin_numbers: bool,
    pub pin_names: Option<PinNames>,
    pub exclude_from_sim: Option<bool>,
    pub in_bom: bool,
    pub on_board: bool,
    pub properties: Vec<SymbolProperty>,
    pub units: Vec<LibSymbolSubUnit>,
    pub embedded_fonts: Option<bool>,
}

impl FromSexpr for LibSymbol {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("symbol")?;

        let id = parser.expect_string()?.parse::<LibraryId>()?;
        let power = parser.maybe_empty_list_with_name("power")?;
        let hide_pin_numbers = parser
            .maybe_list_with_name("pin_numbers")
            .map(|mut p| {
                if p.maybe_symbol_matching("hide") {
                    return Ok::<_, KiCadParseError>(());
                }
                p.maybe_bool_with_name("hide")?;
                p.expect_end()?;
                Ok::<_, KiCadParseError>(())
            })
            .transpose()?
            .is_some();
        let pin_names = parser.maybe::<PinNames>()?;
        let exclude_from_sim = parser.maybe_bool_with_name("exclude_from_sim")?;
        let in_bom = parser.expect_bool_with_name("in_bom")?;
        let on_board = parser.expect_bool_with_name("on_board")?;
        let properties = parser.expect_many::<SymbolProperty>()?;
        let units = parser.expect_many::<LibSymbolSubUnit>()?;
        let embedded_fonts = parser.maybe_bool_with_name("embedded_fonts")?;

        parser.expect_end()?;

        Ok(Self {
            id,
            power,
            hide_pin_numbers,
            pin_names,
            exclude_from_sim,
            in_bom,
            on_board,
            properties,
            units,
            embedded_fonts,
        })
    }
}

simple_maybe_from_sexpr!(LibSymbol, symbol);

impl ToSexpr for LibSymbol {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let exclude_from_sim = self
            .exclude_from_sim
            .map(|value| Sexpr::bool_with_name("exclude_from_sim", value));

        let hide_pin_numbers = if context.explicit_booleans {
            self.hide_pin_numbers.then(|| {
                Sexpr::list_with_name("pin_numbers", &[Some(Sexpr::bool_with_name("hide", true))])
            })
        } else {
            self.hide_pin_numbers
                .then(|| Sexpr::symbol_with_name("pin_numbers", "hide"))
        };

        Sexpr::list_with_name(
            "symbol",
            [
                &[
                    Some(self.id.to_sexpr(context)),
                    self.power.then(|| Sexpr::list_with_name("power", [])),
                    hide_pin_numbers,
                    self.pin_names.as_ref().map(|i| i.to_sexpr(context)),
                    exclude_from_sim,
                    Some(Sexpr::bool_with_name("in_bom", self.in_bom)),
                    Some(Sexpr::bool_with_name("on_board", self.on_board)),
                ][..],
                &self.properties.into_sexpr_vec(context),
                &self.units.into_sexpr_vec(context),
                &[self
                    .embedded_fonts
                    .map(|value| Sexpr::bool_with_name("embedded_fonts", value))],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LibSymbolSubUnit {
    pub id: UnitId,
    pub unit_name: Option<String>,
    pub graphic_items: Vec<LibSymbolGraphicsItem>,
    pub pins: Vec<Pin>,
}

impl FromSexpr for LibSymbolSubUnit {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("symbol")?;

        let id = parser.expect_string()?.parse::<UnitId>()?;
        let unit_name = parser.maybe_string_with_name("unit_name")?;
        let graphic_items = parser.expect_many::<LibSymbolGraphicsItem>()?;
        let pins = parser.expect_many::<Pin>()?;

        parser.expect_end()?;

        Ok(Self {
            id,
            unit_name,
            graphic_items,
            pins,
        })
    }
}

simple_maybe_from_sexpr!(LibSymbolSubUnit, symbol);

impl ToSexpr for LibSymbolSubUnit {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "symbol",
            [
                &[
                    Some(self.id.to_sexpr(context)),
                    self.unit_name
                        .as_ref()
                        .map(|s| Sexpr::string_with_name("unit_name", s)),
                ][..],
                &self.graphic_items.into_sexpr_vec(context),
                &self.pins.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

// ############################################################################

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct LibraryId {
    pub library_nickname: Option<String>,
    pub entry_name: String,
}

impl LibraryId {
    pub fn new(library_nickname: Option<&str>, entry_name: &str) -> Self {
        Self {
            library_nickname: library_nickname.map(ToString::to_string),
            entry_name: entry_name.to_string(),
        }
    }

    pub fn parts(&self) -> (Option<&str>, &str) {
        (self.library_nickname.as_deref(), &self.entry_name)
    }
}

impl FromStr for LibraryId {
    type Err = KiCadParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^(?:([^:]+):)?([^:]+)$").unwrap();

        let Some(captures) = re.captures(s) else {
            return Err(KiCadParseError::InvalidLibraryIdentifier(s.to_string()));
        };

        let [_full, library_nickname, Some(entry_name)] = &captures.iter().collect::<Vec<_>>()[..]
        else {
            return Err(KiCadParseError::InvalidLibraryIdentifier(s.to_string()));
        };

        Ok(Self {
            library_nickname: library_nickname.map(|s| s.as_str().to_string()),
            entry_name: entry_name.as_str().to_string(),
        })
    }
}

impl Display for LibraryId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(library_nickname) = &self.library_nickname {
            write!(f, "{}:{}", library_nickname, self.entry_name)
        } else {
            write!(f, "{}", self.entry_name)
        }
    }
}

impl ToSexpr for LibraryId {
    fn to_sexpr(&self, _: SerializationContext) -> Sexpr {
        let result = if let Some(library_nickname) = &self.library_nickname {
            format!("{}:{}", library_nickname, self.entry_name)
        } else {
            self.entry_name.clone()
        };

        Sexpr::string(result)
    }
}

#[cfg(test)]
mod lib_id_tests {
    use crate::KiCadParseError;

    #[test]
    fn lib_id_from_str() {
        use super::LibraryId;

        let id = "Some Library:Some Part 1".parse::<LibraryId>().unwrap();

        assert_eq!(id.library_nickname, Some("Some Library".to_string()));
        assert_eq!(id.entry_name, "Some Part 1".to_string());

        let id = "Some Part 2".parse::<LibraryId>().unwrap();

        assert_eq!(id.library_nickname, None);
        assert_eq!(id.entry_name, "Some Part 2".to_string());

        let id = "Some:Illegal:Identifier".parse::<LibraryId>();

        assert_eq!(
            id,
            Err(KiCadParseError::InvalidLibraryIdentifier(
                "Some:Illegal:Identifier".to_string()
            ))
        );
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct UnitId {
    pub parent: String,
    pub unit: u16,
    pub style: u8,
}

impl UnitId {
    pub fn new(parent: &str, unit: u16, style: u8) -> Self {
        Self {
            parent: parent.to_string(),
            unit,
            style,
        }
    }
}

impl FromStr for UnitId {
    type Err = KiCadParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^(.+)_(\d+)_(\d)$").unwrap();

        let Some(captures) = re.captures(s) else {
            return Err(KiCadParseError::InvalidUnitIdentifier(s.to_string()));
        };

        let (_, [name, unit, style]) = captures.extract();

        Ok(UnitId {
            parent: name.to_string(),
            unit: unit.parse::<u16>().unwrap(),
            style: style.parse::<u8>().unwrap(),
        })
    }
}

impl ToSexpr for UnitId {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::string(format!("{}_{}_{}", self.parent, self.unit, self.style))
    }
}

#[cfg(test)]
mod unit_id_tests {
    use crate::KiCadParseError;

    #[test]
    fn unit_id_from_str() {
        use super::UnitId;

        let id = "SOME_NAME_20_1".parse::<UnitId>().unwrap();

        assert_eq!(&id.parent, "SOME_NAME");
        assert_eq!(id.unit, 20);
        assert_eq!(id.style, 1);

        let id = "PSMN5R2-60YL_0_1".parse::<UnitId>().unwrap();

        assert_eq!(&id.parent, "PSMN5R2-60YL");
        assert_eq!(id.unit, 0);
        assert_eq!(id.style, 1);

        let id = "PSMN5R2-60YL".parse::<UnitId>();

        assert_eq!(
            id,
            Err(KiCadParseError::InvalidUnitIdentifier(
                "PSMN5R2-60YL".to_string()
            ))
        );
    }
}

// ############################################################################

/// See `pin_names` in [`LibSymbol`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PinNames {
    pub offset: Option<f32>,
    pub hide: bool,
}

impl FromSexpr for PinNames {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pin_names")?;

        let offset = parser.maybe_number_with_name("offset")?;
        let hide = parser.maybe_symbol_or_boolean_with_name("hide");

        parser.expect_end()?;

        Ok(Self { offset, hide })
    }
}

simple_maybe_from_sexpr!(PinNames, pin_names);

impl ToSexpr for PinNames {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let hide = if context.explicit_booleans {
            self.hide.then(|| Sexpr::symbol_with_name("hide", "yes"))
        } else {
            self.hide.then(|| Sexpr::symbol("hide"))
        };

        Sexpr::list_with_name(
            "pin_names",
            [
                self.offset.map(|o| Sexpr::number_with_name("offset", o)),
                hide,
            ],
        )
    }
}

// ############################################################################

/// The `property` token defines a symbol property when used inside a symbol definition.
///
/// NOTE: Symbol properties are different than general purpose properties defined above.
///
/// TODO: rename to "field" as used in the kicad codebase?
///
/// https://dev-docs.kicad.org/en/file-formats/sexpr-intro/#_symbol_properties
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct SymbolProperty {
    /// The "KEY" string defines the name of the property and must be unique.
    pub key: String,
    /// The "VALUE" string defines the value of the property.
    pub value: String,
    /// The `POSITION_IDENTIFIER` defines the X and Y coordinates and rotation angle of the property.
    pub position: Position,
    /// UNDOCUMENTED
    pub show_name: bool,
    /// UNDOCUMENTED
    pub do_not_autoplace: bool,
    /// The `TEXT_EFFECTS` section defines how the text is displayed.
    pub effects: TextEffects,
}

impl FromSexpr for SymbolProperty {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("property")?;

        let key = parser.expect_string()?;
        let value = parser.expect_string()?;
        let position = parser.expect::<Position>()?;
        let show_name = parser.maybe_empty_list_with_name("show_name")?;
        let do_not_autoplace = parser.maybe_empty_list_with_name("do_not_autoplace")?;
        let effects = parser.expect::<TextEffects>()?;

        parser.expect_end()?;

        Ok(Self {
            key,
            value,
            position,
            show_name,
            do_not_autoplace,
            effects,
        })
    }
}

simple_maybe_from_sexpr!(SymbolProperty, property);

impl ToSexpr for SymbolProperty {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "property",
            [
                Some(Sexpr::string(&self.key)),
                Some(Sexpr::string(&self.value)),
                Some(self.position.to_sexpr(context)),
                self.show_name
                    .then(|| Sexpr::list_with_name("show_name", [])),
                self.do_not_autoplace
                    .then(|| Sexpr::list_with_name("do_not_autoplace", [])),
                Some(self.effects.to_sexpr(context)),
            ],
        )
    }
}

// ############################################################################

/// This section documents the various graphical objects used in symbol
/// definitions.
///
/// https://dev-docs.kicad.org/en/file-formats/sexpr-intro/#_symbol_graphic_items
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum LibSymbolGraphicsItem {
    Shape(Shape),
    Text(LibSymbolText),
    TextBox(LibSymbolTextBox),
}

impl FromSexpr for LibSymbolGraphicsItem {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let Some(symbol) = parser.peek_symbol() else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::Symbol,
            });
        };

        match symbol {
            "arc" => Shape::from_sexpr(parser).map(LibSymbolGraphicsItem::Shape),
            "circle" => Shape::from_sexpr(parser).map(LibSymbolGraphicsItem::Shape),
            "rectangle" => Shape::from_sexpr(parser).map(LibSymbolGraphicsItem::Shape),
            "polyline" => Shape::from_sexpr(parser).map(LibSymbolGraphicsItem::Shape),
            "bezier" => Shape::from_sexpr(parser).map(LibSymbolGraphicsItem::Shape),
            "text" => LibSymbolText::from_sexpr(parser).map(LibSymbolGraphicsItem::Text),
            "text_box" => LibSymbolTextBox::from_sexpr(parser).map(LibSymbolGraphicsItem::TextBox),
            _ => Err(KiCadParseError::invalid_enum_value::<Self>(symbol)),
        }
    }
}

impl MaybeFromSexpr for LibSymbolGraphicsItem {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &[
            "arc",
            "circle",
            "rectangle",
            "polyline",
            "bezier",
            "text",
            "text_box",
        ];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for LibSymbolGraphicsItem {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        match self {
            LibSymbolGraphicsItem::Shape(shape) => shape.to_sexpr(context),
            LibSymbolGraphicsItem::Text(text) => text.to_sexpr(context),
            LibSymbolGraphicsItem::TextBox(text_box) => text_box.to_sexpr(context),
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LibSymbolText {
    pub private: bool,
    pub text: String,
    pub position: Position,
    pub effects: TextEffects,
}

impl FromSexpr for LibSymbolText {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("text")?;

        let private = parser.maybe_symbol_matching("private");
        let text = parser.expect_string()?;
        let position = parser.expect::<Position>()?;
        let effects = parser.expect::<TextEffects>()?;

        parser.expect_end()?;

        Ok(Self {
            private,
            text,
            position,
            effects,
        })
    }
}

impl ToSexpr for LibSymbolText {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "text",
            [
                self.private.then(|| Sexpr::symbol("private")),
                Some(Sexpr::string(&self.text)),
                Some(self.position.to_sexpr(context)),
                Some(self.effects.to_sexpr(context)),
            ],
        )
    }
}

/// UNDOCUMENTED
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct LibSymbolTextBox {
    pub private: bool,
    pub text: String,
    pub position: Position,
    pub size: Vec2D,
    pub stroke: Stroke,
    pub fill: ShapeFillMode,
    pub effects: TextEffects,
}

impl FromSexpr for LibSymbolTextBox {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("text_box")?;

        let private = parser.maybe_symbol_matching("private");
        let text = parser.expect_string()?;
        let position = parser.expect::<Position>()?;
        let size = parser.expect_with_name::<Vec2D>("size")?;
        let stroke = parser.expect::<Stroke>()?;
        let fill = parser.expect::<ShapeFillMode>()?;
        let effects = parser.expect::<TextEffects>()?;

        parser.expect_end()?;

        Ok(Self {
            private,
            text,
            position,
            size,
            stroke,
            fill,
            effects,
        })
    }
}

impl ToSexpr for LibSymbolTextBox {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "text_box",
            [
                self.private.then(|| Sexpr::symbol("private")),
                Some(Sexpr::string(&self.text)),
                Some(self.position.to_sexpr(context)),
                Some(self.size.to_sexpr_with_name("size", context)),
                Some(self.stroke.to_sexpr(context)),
                Some(self.fill.to_sexpr(context)),
                Some(self.effects.to_sexpr(context)),
            ],
        )
    }
}

// ############################################################################

/// The pin token defines a pin in a symbol definition.
///
/// https://dev-docs.kicad.org/en/file-formats/sexpr-intro/#_symbol_pin
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Pin {
    /// The `PIN_ELECTRICAL_TYPE` defines the pin electrical connection. See
    /// table below for valid pin electrical connection types and descriptions.
    pub electrical_kind: PinElectricalKind,
    /// The `PIN_GRAPHICAL_STYLE` defines the graphical style used to draw the
    /// pin. See table below for valid pin graphical styles and descriptions.
    pub graphical_style: PinGraphicalStyle,
    /// The `POSITION_IDENTIFIER` defines the X and Y coordinates and rotation
    /// angle of the connection point of the pin relative to the symbol origin
    /// position. The only supported rotation angles for pins are 0, 90, 180,
    /// and 270 degrees.
    ///
    /// TODO: Pin position angles can only be 0, 90, 180, or 270 degrees.
    pub position: Position,
    /// The length token attribute defines the LENGTH of the pin.
    pub length: f32,
    /// UNDOCUMENTED
    pub hide: bool,
    /// The name token defines a quoted string containing the NAME of the pin
    /// and the TEXT_EFFECTS defines how the text is displayed.
    pub name: String,
    pub name_effects: TextEffects,
    /// The number token defines a quoted string containing the NUMBER of the
    /// pin and the TEXT_EFFECTS defines how the text is displayed.
    pub number: String,
    pub number_effects: TextEffects,
    /// UNDOCUMENTED
    pub alternates: Vec<PinAlternate>,
}

impl FromSexpr for Pin {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pin")?;

        let electrical_kind = parser.expect_symbol()?.parse::<PinElectricalKind>()?;
        let graphical_style = parser.expect_symbol()?.parse::<PinGraphicalStyle>()?;
        let position = parser.expect::<Position>()?;
        let length = parser.expect_number_with_name("length")?;
        let hide = parser.maybe_symbol_or_boolean_with_name("hide");
        let (name, name_effects) = parser.expect_list_with_name("name").and_then(|mut p| {
            let name = p.expect_string()?;
            let name_effects = p.expect::<TextEffects>()?;
            p.expect_end()?;

            Ok((name, name_effects))
        })?;
        let (number, number_effects) =
            parser.expect_list_with_name("number").and_then(|mut p| {
                let number = p.expect_string()?;
                let number_effects = p.expect::<TextEffects>()?;
                p.expect_end()?;

                Ok((number, number_effects))
            })?;
        let alternates = parser.expect_many::<PinAlternate>()?;

        parser.expect_end()?;

        Ok(Self {
            electrical_kind,
            graphical_style,
            position,
            length,
            hide,
            name,
            name_effects,
            number,
            number_effects,
            alternates,
        })
    }
}

simple_maybe_from_sexpr!(Pin, pin);

impl ToSexpr for Pin {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let hide = if context.explicit_booleans {
            self.hide.then(|| Sexpr::symbol_with_name("hide", "yes"))
        } else {
            self.hide.then(|| Sexpr::symbol("hide"))
        };

        Sexpr::list_with_name(
            "pin",
            [
                &[
                    Some(Sexpr::symbol(self.electrical_kind)),
                    Some(Sexpr::symbol(self.graphical_style)),
                    Some(self.position.to_sexpr(context)),
                    Some(Sexpr::number_with_name("length", self.length)),
                    hide,
                    Some(Sexpr::list_with_name(
                        "name",
                        [
                            Some(Sexpr::string(&self.name)),
                            Some(self.name_effects.to_sexpr(context)),
                        ],
                    )),
                    Some(Sexpr::list_with_name(
                        "number",
                        [
                            Some(Sexpr::string(&self.number)),
                            Some(self.number_effects.to_sexpr(context)),
                        ],
                    )),
                ][..],
                &self.alternates.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

/// See `electrical_kind` in [`Pin`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub enum PinElectricalKind {
    /// Pin is an input.
    #[default]
    Input,
    /// Pin is an output.
    Output,
    /// Pin can be both input and output.
    Bidirectional,
    /// Pin is a tri-state output.
    TriState,
    /// Pin is electrically passive.
    Passive,
    /// Not internally connected.
    Free,
    /// Pin does not have a specified electrical type.
    Unspecified,
    /// Pin is a power input.
    PowerIn,
    /// Pin is a power output.
    PowerOut,
    /// Pin is an open collector output.
    OpenCollector,
    /// Pin is an open emitter output.
    OpenEmitter,
    /// Pin has no electrical connection.
    NoConnect,
}

simple_to_from_string! {
    PinElectricalKind,
    input <-> Input,
    output <-> Output,
    bidirectional <-> Bidirectional,
    tri_state <-> TriState,
    passive <-> Passive,
    free <-> Free,
    unspecified <-> Unspecified,
    power_in <-> PowerIn,
    power_out <-> PowerOut,
    open_collector <-> OpenCollector,
    open_emitter <-> OpenEmitter,
    no_connect <-> NoConnect,
}

/// See `graphical_style` in [`Pin`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub enum PinGraphicalStyle {
    #[default]
    Line,
    Inverted,
    Clock,
    InvertedClock,
    InputLow,
    ClockLow,
    OutputLow,
    EdgeClockHigh,
    NonLogic,
}

simple_to_from_string! {
    PinGraphicalStyle,
    line <-> Line,
    inverted <-> Inverted,
    clock <-> Clock,
    inverted_clock <-> InvertedClock,
    input_low <-> InputLow,
    clock_low <-> ClockLow,
    output_low <-> OutputLow,
    edge_clock_high <-> EdgeClockHigh,
    non_logic <-> NonLogic,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PinAlternate {
    pub name: String,
    pub electrical_kind: PinElectricalKind,
    pub graphical_style: PinGraphicalStyle,
}

impl FromSexpr for PinAlternate {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("alternate")?;

        let name = parser.expect_string()?;
        let electrical_kind = parser.expect_symbol()?.parse::<PinElectricalKind>()?;
        let graphical_style = parser.expect_symbol()?.parse::<PinGraphicalStyle>()?;

        parser.expect_end()?;

        Ok(Self {
            name,
            electrical_kind,
            graphical_style,
        })
    }
}

simple_maybe_from_sexpr!(PinAlternate, alternate);

impl ToSexpr for PinAlternate {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "alternate",
            [
                Some(Sexpr::string(&self.name)),
                Some(Sexpr::symbol(self.electrical_kind)),
                Some(Sexpr::symbol(self.graphical_style)),
            ],
        )
    }
}
