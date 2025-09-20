//! Footprint text and text box definitions.

use kicad_sexpr::Sexpr;

use crate::{
    common::{CoordinatePointList, LayerId, Stroke, TextEffects, Uuid, Vec2D},
    convert::{FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, ToSexprWithName},
    simple_to_from_string, KiCadParseError,
};

/// A footprint text element.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintText {
    pub kind: FootprintTextKind,
    pub locked: bool,
    pub text: String,
    pub position: FootprintTextPosition,
    pub layer: LayerId,
    pub knockout: bool,
    pub hide: bool,
    pub effects: TextEffects,
    pub tstamp: Uuid,
}

impl FromSexpr for FootprintText {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fp_text")?;

        let kind = parser.expect_symbol()?.parse()?;
        let locked = parser.maybe_symbol_matching("locked");
        let text = parser.expect_string()?;
        let position = parser.expect::<FootprintTextPosition>()?;
        let (layer, knockout) = parser.expect_list_with_name("layer").and_then(|mut list| {
            let layer = list.expect_string()?.parse()?;
            let knockout = list.maybe_symbol_matching("knockout");
            Ok((layer, knockout))
        })?;
        let hide = parser.maybe_symbol_matching("hide");
        let effects = parser.expect::<TextEffects>()?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;

        Ok(Self {
            kind,
            locked,
            text,
            position,
            layer,
            knockout,
            hide,
            effects,
            tstamp,
        })
    }
}

impl ToSexpr for FootprintText {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "fp_text",
            [
                Some(Sexpr::symbol(self.kind)),
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(Sexpr::string(&self.text)),
                Some(self.position.to_sexpr(context)),
                Some(Sexpr::list_with_name(
                    "layer",
                    [
                        Some(Sexpr::string(self.layer)),
                        self.knockout.then(|| Sexpr::symbol("knockout")),
                    ],
                )),
                self.hide.then(|| Sexpr::symbol("hide")),
                Some(self.effects.to_sexpr(context)),
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
            ],
        )
    }
}

/// The types of text that can be used in a footprint.
///
/// Reference and value always live on silkscreen (on the footprint side);
/// other texts are planned to go on whatever layer the user wants.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FootprintTextKind {
    Reference,
    Value,
    User,
}

simple_to_from_string! {
    FootprintTextKind,
    reference <-> Reference,
    value <-> Value,
    user <-> User,
}

/// An extension of the normal [`Position`](crate::common::Position) struct that includes an `unlocked`
/// field
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintTextPosition {
    pub x: f32,
    pub y: f32,
    pub angle: Option<f32>,
    pub unlocked: bool,
}

impl FromSexpr for FootprintTextPosition {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("at")?;

        let x = parser.expect_number()?;
        let y = parser.expect_number()?;
        let angle = parser.maybe_number();
        let unlocked = parser.maybe_symbol_matching("unlocked");

        Ok(Self {
            x,
            y,
            angle,
            unlocked,
        })
    }
}

impl ToSexpr for FootprintTextPosition {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "at",
            [
                Some(Sexpr::number(self.x)),
                Some(Sexpr::number(self.y)),
                self.angle.map(Sexpr::number),
                self.unlocked.then(|| Sexpr::symbol("unlocked")),
            ],
        )
    }
}

// FIXME: Really should be an enum because there are 2 valid types of text boxes
// (axis aligned and partially rotated)
/// A footprint text box element.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintTextBox {
    pub locked: bool,
    pub text: String,
    pub start: Option<Vec2D>,
    pub end: Option<Vec2D>,
    pub points: Option<[Vec2D; 4]>,
    pub angle: Option<f32>,
    pub layer: LayerId,
    pub tstamp: Uuid,
    pub effects: TextEffects,
    pub stroke: Option<Stroke>,
}

impl FromSexpr for FootprintTextBox {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fp_text_box")?;

        let locked = parser.maybe_symbol_matching("locked");
        let text = parser.expect_string()?;
        let start = parser.maybe_with_name::<Vec2D>("start")?;
        let end = parser.maybe_with_name::<Vec2D>("end")?;
        let points = parser
            .maybe::<CoordinatePointList>()?
            .map(|v| {
                v.try_into()
                    .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                        expected: 4,
                        found: v.len(),
                    })
            })
            .transpose()?;
        let angle = parser.maybe_number_with_name("angle")?;
        let layer = parser.expect_string_with_name("layer")?.parse()?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;
        let effects = parser.expect::<TextEffects>()?;
        let stroke = parser.maybe::<Stroke>()?;

        if parser
            .peek_list()
            .is_some_and(|l| l.first_symbol().is_some_and(|s| s == "render_cache"))
        {
            unimplemented!(
                "Found render_cache list in FootprintTextBox: {:?}",
                parser.peek_next().unwrap()
            );
        };

        parser.expect_end()?;

        Ok(Self {
            locked,
            text,
            start,
            end,
            points,
            angle,
            layer,
            tstamp,
            effects,
            stroke,
        })
    }
}

impl ToSexpr for FootprintTextBox {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "fp_text_box",
            [
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(Sexpr::string(&self.text)),
                self.start
                    .as_ref()
                    .map(|v| v.to_sexpr_with_name("start", context)),
                self.end
                    .as_ref()
                    .map(|v| v.to_sexpr_with_name("end", context)),
                self.points.as_ref().map(|v| v.to_vec().to_sexpr(context)),
                self.angle.map(|n| Sexpr::number_with_name("angle", n)),
                Some(Sexpr::string_with_name("layer", self.layer)),
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
                Some(self.effects.to_sexpr(context)),
                self.stroke.as_ref().map(|i| i.to_sexpr(context)),
            ],
        )
    }
}
