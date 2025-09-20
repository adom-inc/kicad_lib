//! All the shape types allowed within a footprint.

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    common::{CoordinatePointList, LayerId, SimpleFillMode, Stroke, Uuid, Vec2D},
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName,
    },
    KiCadParseError,
};

/// Common properties shared by the different shapes.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintShape {
    pub locked: bool,
    pub kind: FootprintShapeKind,
    pub stroke: Stroke,
    pub layer: LayerId,
    pub tstamp: Uuid,
}

/// All the different types of shapes allowed within a footprint.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum FootprintShapeKind {
    Line(FootprintLine),
    Rectangle(FootprintRectangle),
    Circle(FootprintCircle),
    Arc(FootprintArc),
    Polygon(FootprintPolygon),
    Curve(FootprintBezier),
}

impl FromSexpr for FootprintShape {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let name = parser.expect_symbol_matching_any(&[
            "fp_line",
            "fp_rect",
            "fp_circle",
            "fp_arc",
            "fp_poly",
            "fp_curve",
        ])?;

        let locked = parser.maybe_symbol_matching("locked");

        let (kind, stroke) = match name.as_str() {
            "fp_line" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;

                (
                    FootprintShapeKind::Line(FootprintLine { start, end }),
                    stroke,
                )
            }
            "fp_rect" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<SimpleFillMode>()?;

                (
                    FootprintShapeKind::Rectangle(FootprintRectangle { start, end, fill }),
                    stroke,
                )
            }
            "fp_circle" => {
                let center = parser.expect_with_name::<Vec2D>("center")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<SimpleFillMode>()?;

                (
                    FootprintShapeKind::Circle(FootprintCircle { center, end, fill }),
                    stroke,
                )
            }
            "fp_arc" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let midpoint = parser.expect_with_name::<Vec2D>("mid")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;

                (
                    FootprintShapeKind::Arc(FootprintArc {
                        start,
                        midpoint,
                        end,
                    }),
                    stroke,
                )
            }
            "fp_poly" => {
                let points = parser.expect::<CoordinatePointList>()?;
                let stroke = parser.expect::<Stroke>()?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<SimpleFillMode>()?;

                (
                    FootprintShapeKind::Polygon(FootprintPolygon { points, fill }),
                    stroke,
                )
            }
            "fp_curve" => {
                let points = parser.expect::<CoordinatePointList>().and_then(|v| {
                    v.try_into()
                        .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                            expected: 4,
                            found: v.len(),
                        })
                })?;
                let stroke = parser.expect::<Stroke>()?;

                (
                    FootprintShapeKind::Curve(FootprintBezier { points }),
                    stroke,
                )
            }
            _ => unreachable!(),
        };

        let layer = parser.expect_string_with_name("layer")?.parse()?;
        let tstamp = parser.expect_with_name::<Uuid>("tstamp")?;

        Ok(Self {
            locked,
            kind,
            stroke,
            layer,
            tstamp,
        })
    }
}

impl MaybeFromSexpr for FootprintShape {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &[
            "fp_line",
            "fp_rect",
            "fp_circle",
            "fp_arc",
            "fp_poly",
            "fp_curve",
        ];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for FootprintShape {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let (name, elements, fill) = match &self.kind {
            FootprintShapeKind::Line(line) => (
                "fp_line",
                vec![
                    line.start.to_sexpr_with_name("start", context),
                    line.end.to_sexpr_with_name("end", context),
                ],
                None,
            ),
            FootprintShapeKind::Rectangle(rect) => (
                "fp_rect",
                vec![
                    rect.start.to_sexpr_with_name("start", context),
                    rect.end.to_sexpr_with_name("end", context),
                ],
                Some(rect.fill),
            ),
            FootprintShapeKind::Circle(circle) => (
                "fp_circle",
                vec![
                    circle.center.to_sexpr_with_name("center", context),
                    circle.end.to_sexpr_with_name("end", context),
                ],
                Some(circle.fill),
            ),
            FootprintShapeKind::Arc(arc) => (
                "fp_arc",
                vec![
                    arc.start.to_sexpr_with_name("start", context),
                    arc.midpoint.to_sexpr_with_name("mid", context),
                    arc.end.to_sexpr_with_name("end", context),
                ],
                None,
            ),
            FootprintShapeKind::Polygon(ploy) => (
                "fp_poly",
                vec![ploy.points.to_sexpr(context)],
                Some(ploy.fill),
            ),
            FootprintShapeKind::Curve(bezier) => (
                "fp_curve",
                bezier
                    .points
                    .iter()
                    .map(|p| p.to_sexpr(context))
                    .collect::<Vec<_>>(),
                None,
            ),
        };

        Sexpr::list_with_name(
            name,
            [
                &[self.locked.then(|| Sexpr::symbol("locked"))][..],
                &elements.into_iter().map(Option::Some).collect::<Vec<_>>(),
                &[
                    Some(self.stroke.to_sexpr(context)),
                    fill.map(|f| Sexpr::symbol_with_name("fill", f)),
                    Some(Sexpr::string_with_name("layer", self.layer)),
                    Some(self.tstamp.to_sexpr_with_name("tstamp", context)),
                ][..],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintLine {
    pub start: Vec2D,
    pub end: Vec2D,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintRectangle {
    pub start: Vec2D,
    pub end: Vec2D,
    pub fill: SimpleFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintCircle {
    pub center: Vec2D,
    pub end: Vec2D,
    pub fill: SimpleFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintArc {
    pub start: Vec2D,
    pub midpoint: Vec2D,
    pub end: Vec2D,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintPolygon {
    pub points: CoordinatePointList,
    pub fill: SimpleFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct FootprintBezier {
    pub points: [Vec2D; 4],
}
