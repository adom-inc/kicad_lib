use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    common::{CoordinatePointList, LayerId, SimpleFillMode, Stroke, Uuid, Vec2D},
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName,
    },
    KiCadParseError,
};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbShape {
    pub locked: bool,
    pub kind: PcbShapeKind,
    pub stroke: Stroke,
    pub layer: LayerId,
    pub tstamp: Uuid,
}

impl PcbShape {
    pub fn line(start: Vec2D, end: Vec2D, stroke: Stroke, layer: LayerId) -> Self {
        Self {
            locked: false,
            kind: PcbShapeKind::Line(PcbLine { start, end }),
            stroke,
            layer,
            tstamp: Uuid::new(),
        }
    }

    pub fn rectangle(
        start: Vec2D,
        end: Vec2D,
        stroke: Stroke,
        fill: SimpleFillMode,
        layer: LayerId,
    ) -> Self {
        Self {
            locked: false,
            kind: PcbShapeKind::Rectangle(PcbRectangle { start, end, fill }),
            stroke,
            layer,
            tstamp: Uuid::new(),
        }
    }

    pub fn circle(
        center: Vec2D,
        end: Vec2D,
        stroke: Stroke,
        fill: SimpleFillMode,
        layer: LayerId,
    ) -> Self {
        Self {
            locked: false,
            kind: PcbShapeKind::Circle(PcbCircle { center, end, fill }),
            stroke,
            layer,
            tstamp: Uuid::new(),
        }
    }

    pub fn arc(start: Vec2D, midpoint: Vec2D, end: Vec2D, stroke: Stroke, layer: LayerId) -> Self {
        Self {
            locked: false,
            kind: PcbShapeKind::Arc(PcbArc {
                start,
                midpoint,
                end,
            }),
            stroke,
            layer,
            tstamp: Uuid::new(),
        }
    }

    pub fn polygon(
        points: CoordinatePointList,
        stroke: Stroke,
        fill: SimpleFillMode,
        layer: LayerId,
    ) -> Self {
        Self {
            locked: false,
            kind: PcbShapeKind::Polygon(PcbPolygon { points, fill }),
            stroke,
            layer,
            tstamp: Uuid::new(),
        }
    }

    pub fn curve(points: [Vec2D; 4], stroke: Stroke, layer: LayerId) -> Self {
        Self {
            locked: false,
            kind: PcbShapeKind::Curve(PcbBezier { points }),
            stroke,
            layer,
            tstamp: Uuid::new(),
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum PcbShapeKind {
    Line(PcbLine),
    Rectangle(PcbRectangle),
    Circle(PcbCircle),
    Arc(PcbArc),
    Polygon(PcbPolygon),
    Curve(PcbBezier),
}

impl FromSexpr for PcbShape {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let name = parser.expect_symbol_matching_any(&[
            "gr_line",
            "gr_rect",
            "gr_circle",
            "gr_arc",
            "gr_poly",
            "bezier",
        ])?;

        let locked = parser.maybe_symbol_matching("locked");

        let (kind, stroke) = match name.as_str() {
            "gr_line" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;

                (PcbShapeKind::Line(PcbLine { start, end }), stroke)
            }
            "gr_rect" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<SimpleFillMode>()?;

                (
                    PcbShapeKind::Rectangle(PcbRectangle { start, end, fill }),
                    stroke,
                )
            }
            "gr_circle" => {
                let center = parser.expect_with_name::<Vec2D>("center")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<SimpleFillMode>()?;

                (
                    PcbShapeKind::Circle(PcbCircle { center, end, fill }),
                    stroke,
                )
            }
            "gr_arc" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let midpoint = parser.expect_with_name::<Vec2D>("mid")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let stroke = parser.expect::<Stroke>()?;

                (
                    PcbShapeKind::Arc(PcbArc {
                        start,
                        midpoint,
                        end,
                    }),
                    stroke,
                )
            }
            "gr_poly" => {
                let points = parser.expect::<CoordinatePointList>()?;
                let stroke = parser.expect::<Stroke>()?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<SimpleFillMode>()?;

                (PcbShapeKind::Polygon(PcbPolygon { points, fill }), stroke)
            }
            "bezier" => {
                let points = parser.expect::<CoordinatePointList>().and_then(|v| {
                    v.try_into()
                        .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                            expected: 4,
                            found: v.len(),
                        })
                })?;
                let stroke = parser.expect::<Stroke>()?;

                (PcbShapeKind::Curve(PcbBezier { points }), stroke)
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

impl MaybeFromSexpr for PcbShape {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &[
            "gr_line",
            "gr_rect",
            "gr_circle",
            "gr_arc",
            "gr_poly",
            "bezier",
        ];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for PcbShape {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let (name, elements, fill) = match &self.kind {
            PcbShapeKind::Line(line) => (
                "gr_line",
                vec![
                    line.start.to_sexpr_with_name("start", context),
                    line.end.to_sexpr_with_name("end", context),
                ],
                None,
            ),
            PcbShapeKind::Rectangle(rect) => (
                "gr_rect",
                vec![
                    rect.start.to_sexpr_with_name("start", context),
                    rect.end.to_sexpr_with_name("end", context),
                ],
                Some(rect.fill),
            ),
            PcbShapeKind::Circle(circle) => (
                "gr_circle",
                vec![
                    circle.center.to_sexpr_with_name("center", context),
                    circle.end.to_sexpr_with_name("end", context),
                ],
                Some(circle.fill),
            ),
            PcbShapeKind::Arc(arc) => (
                "gr_arc",
                vec![
                    arc.start.to_sexpr_with_name("start", context),
                    arc.midpoint.to_sexpr_with_name("mid", context),
                    arc.end.to_sexpr_with_name("end", context),
                ],
                None,
            ),
            PcbShapeKind::Polygon(ploy) => (
                "gr_poly",
                vec![ploy.points.to_sexpr(context)],
                Some(ploy.fill),
            ),
            PcbShapeKind::Curve(bezier) => (
                "bezier",
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
pub struct PcbLine {
    pub start: Vec2D,
    pub end: Vec2D,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbRectangle {
    pub start: Vec2D,
    pub end: Vec2D,
    pub fill: SimpleFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbCircle {
    pub center: Vec2D,
    pub end: Vec2D,
    pub fill: SimpleFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbArc {
    pub start: Vec2D,
    pub midpoint: Vec2D,
    pub end: Vec2D,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbPolygon {
    pub points: CoordinatePointList,
    pub fill: SimpleFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PcbBezier {
    pub points: [Vec2D; 4],
}
