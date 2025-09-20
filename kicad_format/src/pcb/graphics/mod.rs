//! This module contains the common graphics items that are used in KiCad

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    convert::{FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr},
    KiCadParseError, SexprKind,
};

use self::{
    dimension::PcbDimension,
    shape::PcbShape,
    text::{PcbText, PcbTextBox},
};

pub mod dimension;
pub mod shape;
pub mod text;

// TODO: Dimension

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum PcbGraphicsItem {
    Text(PcbText),
    TextBox(PcbTextBox),
    Shape(PcbShape),
    Dimension(PcbDimension),
}

impl FromSexpr for PcbGraphicsItem {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let Some(symbol) = parser.peek_symbol() else {
            return Err(KiCadParseError::UnexpectedSexprType {
                expected: SexprKind::Symbol,
            });
        };

        match symbol {
            "gr_text" => PcbText::from_sexpr(parser).map(PcbGraphicsItem::Text),
            "gr_text_box" => PcbTextBox::from_sexpr(parser).map(PcbGraphicsItem::TextBox),
            "gr_line" => PcbShape::from_sexpr(parser).map(PcbGraphicsItem::Shape),
            "gr_rect" => PcbShape::from_sexpr(parser).map(PcbGraphicsItem::Shape),
            "gr_circle" => PcbShape::from_sexpr(parser).map(PcbGraphicsItem::Shape),
            "gr_arc" => PcbShape::from_sexpr(parser).map(PcbGraphicsItem::Shape),
            "gr_poly" => PcbShape::from_sexpr(parser).map(PcbGraphicsItem::Shape),
            "bezier" => PcbShape::from_sexpr(parser).map(PcbGraphicsItem::Shape),
            "dimension" => PcbDimension::from_sexpr(parser).map(PcbGraphicsItem::Dimension),
            _ => Err(KiCadParseError::invalid_enum_value::<Self>(symbol)),
        }
    }
}

impl MaybeFromSexpr for PcbGraphicsItem {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &[
            "gr_text",
            "gr_text_box",
            "gr_line",
            "gr_rect",
            "gr_circle",
            "gr_arc",
            "gr_poly",
            "bezier",
            "dimension",
        ];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for PcbGraphicsItem {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        match self {
            PcbGraphicsItem::Text(text) => text.to_sexpr(context),
            PcbGraphicsItem::TextBox(text_box) => text_box.to_sexpr(context),
            PcbGraphicsItem::Shape(shape) => shape.to_sexpr(context),
            PcbGraphicsItem::Dimension(dimension) => dimension.to_sexpr(context),
        }
    }
}
