// TODO gr_bbox: https://dev-docs.kicad.org/en/file-formats/sexpr-intro/#_annotation_bounding_box

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    common::{CoordinatePointList, Vec2D},
    convert::{
        FromSexpr, MaybeFromSexpr, Parser, SerializationContext, SexprListExt, ToSexpr,
        ToSexprWithName,
    },
    simple_to_from_string, KiCadParseError,
};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadGraphicsPrimitive {
    pub kind: PadGraphicsPrimitiveKind,
    pub width: f32,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum PadGraphicsPrimitiveKind {
    Line(PadLine),
    AnnotationBoundingBox(PadAnnotationBoundingBox),
    Rectangle(PadRectangle),
    Arc(PadArc),
    Circle(PadCircle),
    Curve(PadBezier),
    Polygon(PadPolygon),
}

impl FromSexpr for PadGraphicsPrimitive {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let name = parser.expect_symbol_matching_any(&[
            "gr_line",
            "gr_bbox",
            "gr_rect",
            "gr_arc",
            "gr_circle",
            "gr_curve",
            "gr_poly",
        ])?;

        let (kind, width) = match name.as_str() {
            "gr_line" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let width = parser.expect_number_with_name("width")?;

                (
                    PadGraphicsPrimitiveKind::Line(PadLine { start, end }),
                    width,
                )
            }
            "gr_bbox" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let width = parser.expect_number_with_name("width")?;

                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<PrimitiveFillMode>()?;

                (
                    PadGraphicsPrimitiveKind::AnnotationBoundingBox(PadAnnotationBoundingBox {
                        start,
                        end,
                        fill,
                    }),
                    width,
                )
            }
            "gr_rect" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let width = parser.expect_number_with_name("width")?;

                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<PrimitiveFillMode>()?;

                (
                    PadGraphicsPrimitiveKind::Rectangle(PadRectangle { start, end, fill }),
                    width,
                )
            }
            "gr_arc" => {
                let start = parser.expect_with_name::<Vec2D>("start")?;
                let midpoint = parser.expect_with_name::<Vec2D>("mid")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let width = parser.expect_number_with_name("width")?;

                (
                    PadGraphicsPrimitiveKind::Arc(PadArc {
                        start,
                        midpoint,
                        end,
                    }),
                    width,
                )
            }
            "gr_circle" => {
                let center = parser.expect_with_name::<Vec2D>("center")?;
                let end = parser.expect_with_name::<Vec2D>("end")?;
                let width = parser.expect_number_with_name("width")?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<PrimitiveFillMode>()?;

                (
                    PadGraphicsPrimitiveKind::Circle(PadCircle { center, end, fill }),
                    width,
                )
            }
            "gr_curve" => {
                let points = parser.expect::<CoordinatePointList>().and_then(|v| {
                    v.try_into()
                        .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                            expected: 4,
                            found: v.len(),
                        })
                })?;
                let width = parser.expect_number_with_name("width")?;

                (PadGraphicsPrimitiveKind::Curve(PadBezier { points }), width)
            }
            "gr_poly" => {
                let points = parser.expect::<CoordinatePointList>()?;
                let width = parser.expect_number_with_name("width")?;
                let fill = parser
                    .expect_symbol_with_name("fill")?
                    .parse::<PrimitiveFillMode>()?;

                (
                    PadGraphicsPrimitiveKind::Polygon(PadPolygon { points, fill }),
                    width,
                )
            }
            _ => unreachable!(),
        };

        Ok(Self { kind, width })
    }
}

impl MaybeFromSexpr for PadGraphicsPrimitive {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &[
            "gr_line",
            "gr_bbox",
            "gr_rect",
            "gr_arc",
            "gr_circle",
            "gr_curve",
            "gr_poly",
        ];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for PadGraphicsPrimitive {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let (name, elements, fill) = match &self.kind {
            PadGraphicsPrimitiveKind::Line(line) => (
                "gr_line",
                vec![
                    line.start.to_sexpr_with_name("start", context),
                    line.end.to_sexpr_with_name("end", context),
                ],
                None,
            ),
            PadGraphicsPrimitiveKind::AnnotationBoundingBox(bbox) => (
                "gr_bbox",
                vec![
                    bbox.start.to_sexpr_with_name("start", context),
                    bbox.end.to_sexpr_with_name("end", context),
                ],
                Some(bbox.fill),
            ),
            PadGraphicsPrimitiveKind::Rectangle(rect) => (
                "gr_rect",
                vec![
                    rect.start.to_sexpr_with_name("start", context),
                    rect.end.to_sexpr_with_name("end", context),
                ],
                Some(rect.fill),
            ),
            PadGraphicsPrimitiveKind::Arc(arc) => (
                "gr_arc",
                vec![
                    arc.start.to_sexpr_with_name("start", context),
                    arc.midpoint.to_sexpr_with_name("mid", context),
                    arc.end.to_sexpr_with_name("end", context),
                ],
                None,
            ),
            PadGraphicsPrimitiveKind::Circle(circle) => (
                "gr_circle",
                vec![
                    circle.center.to_sexpr_with_name("center", context),
                    circle.end.to_sexpr_with_name("end", context),
                ],
                Some(circle.fill),
            ),
            PadGraphicsPrimitiveKind::Curve(bezier) => (
                "gr_curve",
                bezier
                    .points
                    .iter()
                    .map(|p| p.to_sexpr(context))
                    .collect::<Vec<_>>(),
                None,
            ),
            PadGraphicsPrimitiveKind::Polygon(ploy) => (
                "gr_poly",
                vec![ploy.points.to_sexpr(context)],
                Some(ploy.fill),
            ),
        };

        Sexpr::list_with_name(
            name,
            [
                &elements.into_iter().map(Option::Some).collect::<Vec<_>>(),
                &[
                    Some(Sexpr::number_with_name("width", self.width)),
                    fill.map(|f| Sexpr::symbol_with_name("fill", f)),
                ][..],
            ]
            .concat(),
        )
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadLine {
    pub start: Vec2D,
    pub end: Vec2D,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadRectangle {
    pub start: Vec2D,
    pub end: Vec2D,
    pub fill: PrimitiveFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadAnnotationBoundingBox {
    pub start: Vec2D,
    pub end: Vec2D,
    pub fill: PrimitiveFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadCircle {
    pub center: Vec2D,
    pub end: Vec2D,
    pub fill: PrimitiveFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadArc {
    pub start: Vec2D,
    pub midpoint: Vec2D,
    pub end: Vec2D,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadPolygon {
    pub points: CoordinatePointList,
    pub fill: PrimitiveFillMode,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PadBezier {
    pub points: [Vec2D; 4],
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrimitiveFillMode {
    Solid,
    None,
}

simple_to_from_string! {
    PrimitiveFillMode,
    yes <-> Solid,
    none <-> None,
}
