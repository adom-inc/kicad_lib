use kicad_sexpr::Sexpr;

use crate::{
    common::{CoordinatePointList, LayerId, Position, Stroke, TextEffects, Uuid, Vec2D},
    convert::{FromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr, ToSexprWithName},
    KiCadParseError,
};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbText {
    pub locked: bool,
    pub text: String,
    pub position: Position,
    pub layer: LayerId,
    pub knockout: bool,
    pub tstamp: Uuid,
    pub effects: TextEffects,
}

impl FromSexpr for PcbText {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("gr_text")?;

        let locked = parser.maybe_symbol_matching("locked");
        let text = parser.expect_string()?;
        let position = parser.expect::<Position>()?;
        let (layer, knockout) = parser.expect_list_with_name("layer").and_then(|mut list| {
            let layer = list.expect_string()?.parse()?;
            let knockout = list.maybe_symbol_matching("knockout");
            Ok((layer, knockout))
        })?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;
        let effects = parser.expect::<TextEffects>()?;

        if parser
            .peek_list()
            .is_some_and(|l| l.first_symbol().is_some_and(|s| s == "render_cache"))
        {
            unimplemented!(
                "Found render_cache list in PcbText: {:?}",
                parser.peek_next().unwrap()
            );
        };

        parser.expect_end()?;

        Ok(Self {
            locked,
            text,
            position,
            layer,
            knockout,
            tstamp,
            effects,
        })
    }
}

impl ToSexpr for PcbText {
    fn to_sexpr(&self, context: SerializationContext) -> kicad_sexpr::Sexpr {
        Sexpr::list_with_name(
            "gr_text",
            [
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
                Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
                Some(self.effects.to_sexpr(context)),
            ],
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbTextBox {
    pub locked: bool,
    pub text: String,
    pub position: TextBoxPosition,
    pub angle: Option<f32>,
    pub layer: LayerId,
    pub tstamp: Uuid,
    pub effects: TextEffects,
    pub stroke: Option<Stroke>,
}

impl FromSexpr for PcbTextBox {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("gr_text_box")?;

        let locked = parser.maybe_symbol_matching("locked");
        let text = parser.expect_string()?;

        let Some(next_token) = parser
            .peek_list()
            .and_then(|l| l.first())
            .and_then(|s| s.as_symbol())
        else {
            return Err(KiCadParseError::ExpectedField);
        };

        let position = match next_token.as_str() {
            "start" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                TextBoxPosition::StartEnd(start, end)
            }
            "pts" => {
                let points = parser.expect::<CoordinatePointList>().and_then(|v| {
                    v.try_into()
                        .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                            expected: 4,
                            found: v.len(),
                        })
                })?;
                TextBoxPosition::Points(points)
            }
            _ => {
                return Err(KiCadParseError::ExpectedMutuallyExclusiveField {
                    field1: "start".to_string(),
                    field2: "pts".to_string(),
                })
            }
        };

        let angle = parser.maybe_number_with_name("angle")?;
        let layer = parser.expect_string()?.parse::<LayerId>()?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;
        let effects = parser.expect::<TextEffects>()?;
        let stroke = parser.maybe::<Stroke>()?;

        if parser
            .peek_list()
            .is_some_and(|l| l.first_symbol().is_some_and(|s| s == "render_cache"))
        {
            unimplemented!(
                "Found render_cache list in PcbTextBox: {:?}",
                parser.peek_next().unwrap()
            );
        };

        parser.expect_end()?;

        Ok(Self {
            locked,
            text,
            position,
            angle,
            layer,
            tstamp,
            effects,
            stroke,
        })
    }
}

impl ToSexpr for PcbTextBox {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "gr_text_box",
            [
                &[
                    self.locked.then(|| Sexpr::symbol("locked")),
                    Some(Sexpr::string(&self.text)),
                ][..],
                &match &self.position {
                    TextBoxPosition::StartEnd(start, end) => vec![
                        Some(start.to_sexpr_with_name("start", context)),
                        Some(end.to_sexpr_with_name("end", context)),
                    ],
                    TextBoxPosition::Points(points) => {
                        vec![Some(points.to_vec().to_sexpr(context))]
                    }
                },
                &[
                    self.angle.map(|a| Sexpr::number_with_name("angle", a)),
                    Some(Sexpr::string(self.layer)),
                    Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
                    Some(self.effects.to_sexpr(context)),
                    self.stroke.as_ref().map(|i| i.to_sexpr(context)),
                ][..],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub enum TextBoxPosition {
    StartEnd(Vec2D, Vec2D),
    Points([Vec2D; 4]),
}
