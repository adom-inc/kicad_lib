//! Common shape definitions used in schematics and lib symbols
//!
//! Annoyingly, there are many different "Shape" types in KiCad but this is the
//! more general one used by schematics and lib symbols. Other types include
//! [`FootprintShape`](crate::common::footprint::shape::FootprintShape), and
//! [`PcbShape`](crate::pcb::graphics::shape::PcbShape).

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    convert::{FromSexpr, MaybeFromSexpr, Parser, SexprListExt, ToSexpr, ToSexprWithName},
    KiCadParseError,
};

use super::{Color, CoordinatePointList, Stroke, Uuid, Vec2D};

/// Common properties shared by the different shapes.
///

#[derive(Debug, PartialEq, Clone)]
pub struct Shape {
    pub private: bool,
    pub kind: ShapeKind,
    pub stroke: Stroke,
    pub fill: ShapeFillMode,
    pub uuid: Option<Uuid>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ShapeKind {
    Arc(Arc),
    Circle(Circle),
    Rectangle(Rectangle),
    Bezier(Bezier),
    PolyLine(PolyLine),
}

impl FromSexpr for Shape {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        let name = parser.expect_symbol_matching_any(&[
            "arc",
            "circle",
            "rectangle",
            "bezier",
            "polyline",
        ])?;

        let private = parser.maybe_symbol_matching("private");

        let kind = match name.as_str() {
            "arc" => ShapeKind::Arc(Arc {
                start: parser.expect_with_name::<Vec2D>("start")?,
                midpoint: parser.expect_with_name::<Vec2D>("mid")?,
                end: parser.expect_with_name::<Vec2D>("end")?,
            }),
            "circle" => ShapeKind::Circle(Circle {
                center: parser.expect_with_name::<Vec2D>("center")?,
                radius: parser.expect_number_with_name("radius")?,
            }),
            "rectangle" => ShapeKind::Rectangle(Rectangle {
                start: parser.expect_with_name::<Vec2D>("start")?,
                end: parser.expect_with_name::<Vec2D>("end")?,
            }),
            "polyline" => ShapeKind::PolyLine(PolyLine {
                points: parser.expect::<CoordinatePointList>()?,
            }),
            "bezier" => {
                let points = parser.expect::<CoordinatePointList>().and_then(|v| {
                    v.try_into()
                        .map_err(|v: Vec<_>| KiCadParseError::IncorrectNumberOfPoints {
                            expected: 4,
                            found: v.len(),
                        })
                })?;

                ShapeKind::Bezier(Bezier { points })
            }
            _ => unreachable!(),
        };

        let stroke = parser.expect::<Stroke>()?;
        let fill = parser.expect::<ShapeFillMode>()?;
        let uuid = parser.maybe::<Uuid>()?;

        Ok(Self {
            private,
            kind,
            stroke,
            fill,
            uuid,
        })
    }
}

impl MaybeFromSexpr for Shape {
    fn is_present(sexpr: &SexprList) -> bool {
        const VALID_SYMBOLS: &[&str] = &["arc", "circle", "rectangle", "bezier", "polyline"];

        sexpr
            .first_symbol()
            .is_some_and(|s| VALID_SYMBOLS.contains(&s))
    }
}

impl ToSexpr for Shape {
    fn to_sexpr(&self) -> Sexpr {
        let (name, elements) = match &self.kind {
            ShapeKind::Arc(arc) => (
                "arc",
                vec![
                    arc.start.to_sexpr_with_name("start"),
                    arc.midpoint.to_sexpr_with_name("mid"),
                    arc.end.to_sexpr_with_name("end"),
                ],
            ),
            ShapeKind::Circle(circle) => (
                "circle",
                vec![
                    circle.center.to_sexpr_with_name("center"),
                    Sexpr::number_with_name("radius", circle.radius),
                ],
            ),
            ShapeKind::Rectangle(rect) => (
                "rectangle",
                vec![
                    rect.start.to_sexpr_with_name("start"),
                    rect.end.to_sexpr_with_name("end"),
                ],
            ),
            ShapeKind::Bezier(bezier) => (
                "bezier",
                bezier
                    .points
                    .iter()
                    .map(|p| p.to_sexpr())
                    .collect::<Vec<_>>(),
            ),
            ShapeKind::PolyLine(ploy) => ("polyline", vec![ploy.points.to_sexpr()]),
        };

        Sexpr::list_with_name(
            name,
            [
                &[self.private.then(|| Sexpr::symbol("private"))][..],
                &elements.into_iter().map(Option::Some).collect::<Vec<_>>(),
                &[
                    Some(self.stroke.to_sexpr()),
                    Some(self.fill.to_sexpr()),
                    self.uuid.as_ref().map(ToSexpr::to_sexpr),
                ][..],
            ]
            .concat(),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Arc {
    pub start: Vec2D,
    pub midpoint: Vec2D,
    pub end: Vec2D,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Circle {
    pub center: Vec2D,
    pub radius: f32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Rectangle {
    pub start: Vec2D,
    pub end: Vec2D,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Bezier {
    pub points: [Vec2D; 4],
}

#[derive(Debug, PartialEq, Clone)]
pub struct PolyLine {
    pub points: CoordinatePointList,
}

/// The mode to use when filling in the shape
#[derive(Debug, PartialEq, Default, Clone)]
pub enum ShapeFillMode {
    /// Graphic item not filled.
    #[default]
    None,
    /// Graphic item filled with the line color.
    Outline,
    /// Graphic filled with the theme background color.
    Background,
    /// UNDOCUMENTED
    Color(Color),
}

impl FromSexpr for ShapeFillMode {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("fill")?;

        let mode = parser.expect_symbol_with_name("type")?;
        let result = match mode.as_str() {
            "none" => Self::None,
            "outline" => Self::Outline,
            "background" => Self::Background,
            "color" => Self::Color(parser.expect::<Color>()?),
            value => return Err(KiCadParseError::invalid_enum_value::<Self>(value)),
        };

        parser.expect_end()?;

        Ok(result)
    }
}

impl ToSexpr for ShapeFillMode {
    fn to_sexpr(&self) -> Sexpr {
        let kind = match self {
            Self::None => "none",
            Self::Outline => "outline",
            Self::Background => "background",
            Self::Color(_) => "color",
        };

        let color = match self {
            Self::Color(color) => Some(color.to_sexpr()),
            _ => None,
        };

        Sexpr::list_with_name("fill", [Some(Sexpr::symbol_with_name("type", kind)), color])
    }
}
