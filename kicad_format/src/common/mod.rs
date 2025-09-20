//! Common (shared) types within the KiCad Sexpr file format.

use std::{ops::Deref, str::FromStr};

use kicad_sexpr::{Sexpr, SexprList};

use crate::{
    convert::{
        FromSexpr, FromSexprWithName, MaybeFromSexpr, MaybeFromSexprWithName, Parser,
        SerializationContext, SexprListExt, ToSexpr, ToSexprWithName, VecToMaybeSexprVec,
    },
    simple_maybe_from_sexpr, simple_to_from_string, KiCadParseError,
};

pub mod footprint;
pub mod pad;
pub mod shape;
pub mod symbol;
pub mod zone;

/// Generic position type used in many parts of the format.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    /// The `X` attribute defines the horizontal position of the object.
    pub x: f32,
    /// The `Y` attribute defines the vertical position of the object.
    pub y: f32,
    /// The optional `ANGLE` attribute defines the rotational angle of the object.
    /// Not all objects have rotational position definitions.
    pub angle: Option<i16>,
}

impl Position {
    pub fn new(x: f32, y: f32, angle: Option<i16>) -> Self {
        Self { x, y, angle }
    }
}

impl FromSexpr for Position {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("at")?;

        let x = parser.expect_number()?;
        let y = parser.expect_number()?;
        let angle = parser.maybe_number().map(|n| n as i16);

        Ok(Self { x, y, angle })
    }
}

impl ToSexpr for Position {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "at",
            [
                Some(Sexpr::number(self.x)),
                Some(Sexpr::number(self.y)),
                self.angle.map(|a| Sexpr::number(a as f32)),
            ],
        )
    }
}

// ############################################################################

/// A list of 2D coordinates.
pub type CoordinatePointList = Vec<Vec2D>;

impl FromSexpr for CoordinatePointList {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("pts")?;

        let points = parser.expect_many::<Vec2D>()?;

        parser.expect_end()?;

        Ok(points)
    }
}

simple_maybe_from_sexpr!(CoordinatePointList, pts);

impl ToSexpr for CoordinatePointList {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name("pts", self.into_sexpr_vec(context))
    }
}

/// A single coordinate pair
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Vec2D {
    pub x: f32,
    pub y: f32,
}

impl Vec2D {
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}

impl FromSexpr for Vec2D {
    fn from_sexpr(parser: Parser) -> Result<Self, KiCadParseError> {
        Self::from_sexpr_with_name(parser, "xy")
    }
}

impl MaybeFromSexpr for Vec2D {
    fn is_present(sexpr: &SexprList) -> bool {
        Self::is_present_with_name(sexpr, "xy")
    }
}

impl FromSexprWithName for Vec2D {
    fn from_sexpr_with_name(mut parser: Parser, name: &str) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching(name)?;

        let x = parser.expect_number()?;
        let y = parser.expect_number()?;

        parser.expect_end()?;

        Ok(Self { x, y })
    }
}

impl MaybeFromSexprWithName for Vec2D {}

impl ToSexpr for Vec2D {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        self.to_sexpr_with_name("xy", context)
    }
}

impl ToSexprWithName for Vec2D {
    fn to_sexpr_with_name(&self, name: &str, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            name,
            [Some(Sexpr::number(self.x)), Some(Sexpr::number(self.y))],
        )
    }
}

// ############################################################################

/// A single 3D coordinate pair
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Vec3D {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3D {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }
}

impl FromSexpr for Vec3D {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("xyz")?;

        let x = parser.expect_number()?;
        let y = parser.expect_number()?;
        let z = parser.expect_number()?;

        parser.expect_end()?;

        Ok(Self { x, y, z })
    }
}

impl ToSexpr for Vec3D {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        self.to_sexpr_with_name("xyz", context)
    }
}

impl ToSexprWithName for Vec3D {
    fn to_sexpr_with_name(&self, name: &str, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            name,
            [
                Some(Sexpr::number(self.x)),
                Some(Sexpr::number(self.y)),
                Some(Sexpr::number(self.z)),
            ],
        )
    }
}

// ############################################################################

/// A definition of how the outlines of graphical objects should be drawn.
///
/// Used for various graphical objects such as lines, arcs, circles, polygons,
/// and text.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Default, Clone)]
pub struct Stroke {
    /// The `width` token attribute defines the line width of the graphic object.
    pub width: f32,
    /// The type token attribute defines the line style of the graphic object. Valid stroke line styles are:
    ///  - dash
    ///  - dash_dot
    ///  - dash_dot_dot (from version 7)
    ///  - dot
    ///  - default
    ///  - solid
    pub kind: StrokeKind,
    /// The `color` token attributes define the line red, green, blue, and alpha color settings.
    pub color: Option<Color>,
}

impl Stroke {
    pub fn new(width: f32, kind: StrokeKind) -> Self {
        Self {
            width,
            kind,
            color: None,
        }
    }

    pub fn with_color(width: f32, kind: StrokeKind, color: Color) -> Self {
        Self {
            width,
            kind,
            color: Some(color),
        }
    }
}

impl FromSexpr for Stroke {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("stroke")?;

        let width = parser.expect_number_with_name("width")?;
        let kind = parser.expect_symbol_with_name("type")?.parse()?;
        let color = parser.maybe::<Color>()?;

        parser.expect_end()?;

        Ok(Self { width, kind, color })
    }
}

simple_maybe_from_sexpr!(Stroke, stroke);

impl ToSexpr for Stroke {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "stroke",
            [
                Some(Sexpr::number_with_name("width", self.width)),
                Some(Sexpr::symbol_with_name("type", self.kind)),
                self.color.as_ref().map(|i| i.to_sexpr(context)),
            ],
        )
    }
}

/// Defines the possible stroke line styles.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub enum StrokeKind {
    #[default]
    Default,
    Dash,
    DashDot,
    DashDotDot,
    Dot,
    Solid,
}

simple_to_from_string! {
    StrokeKind,
    default <-> Default,
    dash <-> Dash,
    dash_dot <-> DashDot,
    dash_dot_dot <-> DashDotDot,
    dot <-> Dot,
    solid <-> Solid
}

/// An RGBA color
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8,
}

impl FromSexpr for Color {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("color")?;

        let red = parser.expect_number()? as u8;
        let green = parser.expect_number()? as u8;
        let blue = parser.expect_number()? as u8;
        let alpha = parser.expect_number()? as u8;

        parser.expect_end()?;

        Ok(Self {
            red,
            green,
            blue,
            alpha,
        })
    }
}

simple_maybe_from_sexpr!(Color, color);

impl ToSexpr for Color {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "color",
            [
                Some(Sexpr::number(self.red as f32)),
                Some(Sexpr::number(self.green as f32)),
                Some(Sexpr::number(self.blue as f32)),
                Some(Sexpr::number(self.alpha as f32)),
            ],
        )
    }
}

// ############################################################################

/// All text objects can have an optional effects section that defines how the
/// text is displayed.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct TextEffects {
    /// The `font` token attributes define how the text is shown.
    pub font: Font,
    /// The optional `justify` token attributes define if the text is justified
    /// horizontally `right` or `left` and/or vertically `top` or `bottom`
    /// and/or mirrored. If the justification is not defined, then the text is
    /// center justified both horizontally and vertically and not mirrored.
    pub justify: Option<Justify>,
    /// The optional `hide` token defines if the text is hidden.
    pub hide: bool,
    /// UNDOCUMENTED: The `href` token specifies the hyperlink of the text.
    pub href: Option<String>,
}

impl TextEffects {
    pub fn from_size(x: f32, y: f32) -> Self {
        Self {
            font: Font {
                face: None,
                size: Vec2D { x, y },
                line_spacing: None,
                thickness: None,
                bold: false,
                italic: false,
                color: None,
            },
            justify: None,
            hide: false,
            href: None,
        }
    }

    pub fn with_size(self, x: f32, y: f32) -> Self {
        Self {
            font: Font {
                size: Vec2D { x, y },
                ..self.font
            },
            ..self
        }
    }

    pub fn with_hide(self, hide: bool) -> Self {
        Self { hide, ..self }
    }
}

impl FromSexpr for TextEffects {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("effects")?;

        let font = parser.expect::<Font>()?;
        let justify = parser.maybe::<Justify>()?;
        let hide = parser.maybe_symbol_or_boolean_with_name("hide");
        let href = parser.maybe_string_with_name("href")?;

        parser.expect_end()?;

        Ok(Self {
            font,
            justify,
            hide,
            href,
        })
    }
}

impl ToSexpr for TextEffects {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        let hide = if context.explicit_booleans {
            self.hide.then(|| Sexpr::symbol_with_name("hide", "yes"))
        } else {
            self.hide.then(|| Sexpr::symbol("hide"))
        };

        Sexpr::list_with_name(
            "effects",
            [
                Some(self.font.to_sexpr(context)),
                self.justify.as_ref().map(|i| i.to_sexpr(context)),
                hide,
                self.href
                    .as_ref()
                    .map(|h| Sexpr::string_with_name("href", h)),
            ],
        )
    }
}

/// A font definition for text objects.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Font {
    /// The optional face token indicates the font family. It should be a
    /// TrueType font family name or "KiCad Font" for the KiCad stroke font.
    /// (from version 7)
    pub face: Option<String>,
    /// The `size` token attributes define the font height and width.
    pub size: Vec2D,
    /// The `line_spacing` token specifies the spacing between lines as a ratio
    /// of standard line-spacing. (Not yet supported)
    pub line_spacing: Option<f32>,
    /// The `thickness` token attribute defines the line thickness of the font.
    pub thickness: Option<f32>,
    /// The `bold` token specifies if the font should be bold.
    pub bold: bool,
    /// The `italic` token specifies if the font should be italicized.
    pub italic: bool,
    /// UNDOCUMENTED: The `color` token specifies the color of the text.
    pub color: Option<Color>,
}

impl FromSexpr for Font {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("font")?;

        let face = parser.maybe_string_with_name("face")?;
        let size = parser.expect_with_name::<Vec2D>("size")?;
        let line_spacing = parser.maybe_number_with_name("line_spacing")?;
        let thickness = parser.maybe_number_with_name("thickness")?;
        let bold = parser.maybe_symbol_matching("bold");
        let italic = parser.maybe_symbol_matching("italic");
        let color = parser.maybe::<Color>()?;

        parser.expect_end()?;

        Ok(Self {
            face,
            size,
            line_spacing,
            thickness,
            bold,
            italic,
            color,
        })
    }
}

impl ToSexpr for Font {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "font",
            [
                self.face
                    .as_ref()
                    .map(|f| Sexpr::string_with_name("face", f)),
                Some(self.size.to_sexpr_with_name("size", context)),
                self.line_spacing
                    .map(|l| Sexpr::number_with_name("line_spacing", l)),
                self.thickness
                    .map(|t| Sexpr::number_with_name("thickness", t)),
                self.bold.then(|| Sexpr::symbol("bold")),
                self.italic.then(|| Sexpr::symbol("italic")),
                self.color.as_ref().map(|i| i.to_sexpr(context)),
            ],
        )
    }
}

/// Text justification options.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Justify {
    pub horizontal_direction: Option<HorizontalDirection>,
    pub vertical_direction: Option<VerticalDirection>,
    /// The mirror token is only supported in the PCB Editor and Footprints.
    pub mirror: bool,
}

impl FromSexpr for Justify {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("justify")?;

        let horizontal_direction = parser
            .maybe_symbol_matching_any(&["left", "right"])
            .map(|h| h.parse().unwrap());
        let vertical_direction = parser
            .maybe_symbol_matching_any(&["top", "bottom"])
            .map(|h| h.parse().unwrap());
        let mirror = parser.maybe_symbol_matching("mirror");

        parser.expect_end()?;

        Ok(Self {
            horizontal_direction,
            vertical_direction,
            mirror,
        })
    }
}

simple_maybe_from_sexpr!(Justify, justify);

impl ToSexpr for Justify {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "justify",
            [
                self.horizontal_direction.map(Sexpr::symbol),
                self.vertical_direction.map(Sexpr::symbol),
                self.mirror.then(|| Sexpr::symbol("mirror")),
            ],
        )
    }
}

/// See the `horizontal_direction` field in [`Justify`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HorizontalDirection {
    Left,
    Right,
}

simple_to_from_string! {
    HorizontalDirection,
    left <-> Left,
    right <-> Right
}

/// See the `vertical_direction` field in [`Justify`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VerticalDirection {
    Top,
    Bottom,
}

simple_to_from_string! {
    VerticalDirection,
    top <-> Top,
    bottom <-> Bottom
}

// ############################################################################

/// Defines the drawing page size and orientation.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct PageSettings {
    pub size: PageSize,
    /// The portrait token defines if the page is shown in the portrait mode.
    /// If not defined, the landscape page layout mode is used.
    pub portrait: bool,
}

impl FromSexpr for PageSettings {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("paper")?;

        let size_string = parser.expect_string()?;

        let size = match size_string.as_str() {
            "User" => {
                let width = parser.expect_number()?;
                let height = parser.expect_number()?;

                PageSize::Custom(CustomPageSize { width, height })
            }
            s => PageSize::Standard(s.parse::<StandardPageSize>()?),
        };

        let portrait = parser.maybe_symbol_matching("portrait");

        parser.expect_end()?;

        Ok(Self { size, portrait })
    }
}

impl ToSexpr for PageSettings {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "paper",
            [
                match &self.size {
                    PageSize::Standard(s) => vec![Some(Sexpr::string(s.to_string()))],
                    PageSize::Custom(c) => vec![
                        Some(Sexpr::string("User")),
                        Some(Sexpr::number(c.width)),
                        Some(Sexpr::number(c.height)),
                    ],
                },
                vec![self.portrait.then(|| Sexpr::symbol("portrait"))],
            ]
            .concat(),
        )
    }
}

/// The page size definition can either be a standard size or a custom size.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[derive(Debug, PartialEq, Clone)]
pub enum PageSize {
    Standard(StandardPageSize),
    Custom(CustomPageSize),
}

/// All the preset standard page sizes
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StandardPageSize {
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A,
    B,
    C,
    D,
    E,
    Gerber,
    USLetter,
    USLegal,
    USLedger,
}

simple_to_from_string! {
    StandardPageSize,
    A0 <-> A0,
    A1 <-> A1,
    A2 <-> A2,
    A3 <-> A3,
    A4 <-> A4,
    A5 <-> A5,
    A <-> A,
    B <-> B,
    C <-> C,
    D <-> D,
    E <-> E,
    Gerber <-> Gerber,
    USLetter <-> USLetter,
    USLegal <-> USLegal,
    USLedger <-> USLedger
}

/// A custom page size definition.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct CustomPageSize {
    pub width: f32,
    pub height: f32,
}

// ############################################################################

/// Title block information displayed in the corner of the page
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct TitleBlock {
    /// The `title` token attribute is a quoted string that defines the
    /// document title.
    pub title: Option<String>,
    /// The `date` token attribute is a quoted string that defines the document
    /// date using the YYYY-MM-DD format.
    pub date: Option<String>,
    /// The `rev` token attribute is a quoted string that defines the document
    /// revision.
    pub revision: Option<String>,
    /// The `company` token attribute is a quoted string that defines the
    /// document company name.
    pub company: Option<String>,
    /// The `comment` token attributes define the document comments where N is
    /// a number from 1 to 9 and COMMENT is a quoted string.
    pub comments: Vec<Comment>,
}

impl FromSexpr for TitleBlock {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("title_block")?;

        let title = parser.maybe_string_with_name("title")?;
        let date = parser.maybe_string_with_name("date")?;
        let revision = parser.maybe_string_with_name("rev")?;
        let company = parser.maybe_string_with_name("company")?;
        let comments = parser.expect_many::<Comment>()?;

        parser.expect_end()?;

        Ok(Self {
            title,
            date,
            revision,
            company,
            comments,
        })
    }
}

simple_maybe_from_sexpr!(TitleBlock, title_block);

impl ToSexpr for TitleBlock {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "title_block",
            [
                &[
                    self.title
                        .as_ref()
                        .map(|t| Sexpr::string_with_name("title", t)),
                    self.date
                        .as_ref()
                        .map(|d| Sexpr::string_with_name("date", d)),
                    self.revision
                        .as_ref()
                        .map(|r| Sexpr::string_with_name("rev", r)),
                    self.company
                        .as_ref()
                        .map(|c| Sexpr::string_with_name("company", c)),
                ][..],
                &self.comments.into_sexpr_vec(context),
            ]
            .concat(),
        )
    }
}

/// A comment within the [`TitleBlock`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Comment {
    pub index: u8,
    pub text: String,
}

impl FromSexpr for Comment {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("comment")?;

        let index = parser.expect_number()? as u8;
        let text = parser.expect_string()?;

        parser.expect_end()?;

        Ok(Self { index, text })
    }
}

simple_maybe_from_sexpr!(Comment, comment);

impl ToSexpr for Comment {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "comment",
            [
                Some(Sexpr::number(self.index as f32)),
                Some(Sexpr::string(&self.text)),
            ],
        )
    }
}

// ############################################################################

/// A generic property key-value pair.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Property {
    /// The property key attribute is a string that defines the name of the
    /// property. Property keys must be unique.
    pub key: String,
    /// The property value attribute is a string associated with the key
    /// attribute.
    pub value: String,
}

impl FromSexpr for Property {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("property")?;

        let key = parser.expect_string()?;
        let value = parser.expect_string()?;

        Ok(Self { key, value })
    }
}

simple_maybe_from_sexpr!(Property, property);

impl ToSexpr for Property {
    fn to_sexpr(&self, _context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "property",
            [
                Some(Sexpr::string(&self.key)),
                Some(Sexpr::string(&self.value)),
            ],
        )
    }
}

// ############################################################################

/// A universally unique identifier (sometimes referred to as a `tstamp`,
/// although that is an artifact of the legacy file format).
///
/// TODO: replace with just uuid::Uuid
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Uuid(
    /// The UUID attribute is a Version 4 (random) UUID that should be globally
    /// unique. KiCad UUIDs are generated using the
    /// [mt19937 Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
    /// algorithm.
    ///
    /// Files converted from legacy versions of KiCad (prior to 6.0) have their
    ///  locally-unique timestamps re-encoded in UUID format.
    #[cfg_attr(feature = "serde", serde(with = "uuid::serde::simple"))]
    pub uuid::Uuid,
);

#[allow(clippy::new_without_default)]
impl Uuid {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

impl FromSexpr for Uuid {
    fn from_sexpr(parser: Parser) -> Result<Self, KiCadParseError> {
        Self::from_sexpr_with_name(parser, "uuid")
    }
}

impl MaybeFromSexpr for Uuid {
    fn is_present(sexpr: &SexprList) -> bool {
        Self::is_present_with_name(sexpr, "uuid")
    }
}

impl FromSexprWithName for Uuid {
    fn from_sexpr_with_name(mut parser: Parser, name: &str) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching(name)?;

        let uuid_str = match parser.peek_next() {
            Some(Sexpr::String(_)) => parser.expect_string(),
            Some(Sexpr::Symbol(_)) => parser.expect_symbol(),
            _ => Err(KiCadParseError::UnexpectedSexprType {
                expected: crate::SexprKind::Symbol,
            }),
        };
        let uuid = uuid_str?.parse()?;

        Ok(Self(uuid))
    }
}

impl MaybeFromSexprWithName for Uuid {}

impl ToSexpr for Uuid {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        self.to_sexpr_with_name("uuid", context)
    }
}

impl ToSexprWithName for Uuid {
    fn to_sexpr_with_name(&self, name: &str, context: SerializationContext) -> Sexpr {
        if context.uuids_as_strings {
            Sexpr::list_with_name(name, [Some(Sexpr::string(self.to_string()))])
        } else {
            Sexpr::list_with_name(name, [Some(Sexpr::symbol(self.to_string()))])
        }
    }
}

impl Deref for Uuid {
    type Target = uuid::Uuid;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<uuid::Uuid> for Uuid {
    fn from(uuid: uuid::Uuid) -> Self {
        Self(uuid)
    }
}

impl From<Uuid> for uuid::Uuid {
    fn from(value: Uuid) -> Self {
        *value
    }
}

// ############################################################################

/// An embedded bitmap image stored in Base64 encoded PNG format.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Image {
    /// The POSITION_IDENTIFIER defines the X and Y coordinates of the image.
    ///
    /// See [`Position`].
    pub position: Position,
    /// The `layer` token attribute defines the associated board layer of the
    /// image using one canonical layer name. Only used by board and footprint
    /// images.
    ///
    /// See [`Layer`].
    pub layer: Option<LayerId>,
    /// The optional `scale` token attribute defines the SCALE_FACTOR of the
    /// image.
    pub scale: Option<f32>,
    /// The UNIQUE_IDENTIFIER defines the universally unique identifier for the
    /// image.
    pub unique_id: Option<Uuid>,
    /// The `data` token attribute defines the image data in the portable
    /// network graphics format (PNG) encoded with MIME type base64.
    pub data: String,
}

impl FromSexpr for Image {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("image")?;

        let position = parser.expect::<Position>()?;
        let layer = parser
            .maybe_string_with_name("layer")?
            .map(|s| s.parse())
            .transpose()?;
        let scale = parser.maybe_number_with_name("scale")?;
        let unique_id = parser.maybe::<Uuid>()?;
        let data = parser.expect_string()?;

        parser.expect_end()?;

        Ok(Self {
            position,
            scale,
            layer,
            unique_id,
            data,
        })
    }
}

simple_maybe_from_sexpr!(Image, image);

impl ToSexpr for Image {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "image",
            [
                Some(self.position.to_sexpr(context)),
                self.layer.map(|l| Sexpr::string_with_name("layer", l)),
                self.scale.map(|s| Sexpr::number_with_name("scale", s)),
                self.unique_id.as_ref().map(|i| i.to_sexpr(context)),
                Some(Sexpr::string(&self.data)),
            ],
        )
    }
}

// ############################################################################

/// All drawable board and footprint objects exist on a layer which is defined
/// in the drawable item definition. All layers can be renamed by the user.
///
/// Internally, all layer names are canonical. User defined layer names are
/// only used for display and output purposes.
///
/// Layer definitions can be specified as a list of one or more canonical layer
/// names or with a '*' wildcard to represent all layers that match the rest of
/// the wildcard. For instance, *.Cu represents all of the copper layers. This
/// only applies to canonical layers names.
#[repr(u8)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LayerId {
    FCu,
    In1Cu,
    In2Cu,
    In3Cu,
    In4Cu,
    In5Cu,
    In6Cu,
    In7Cu,
    In8Cu,
    In9Cu,
    In10Cu,
    In11Cu,
    In12Cu,
    In13Cu,
    In14Cu,
    In15Cu,
    In16Cu,
    In17Cu,
    In18Cu,
    In19Cu,
    In20Cu,
    In21Cu,
    In22Cu,
    In23Cu,
    In24Cu,
    In25Cu,
    In26Cu,
    In27Cu,
    In28Cu,
    In29Cu,
    In30Cu,
    BCu,
    BAdhes,
    FAdhes,
    BPaste,
    FPaste,
    BSilkS,
    FSilkS,
    BMask,
    FMask,
    DwgsUser,
    CmtsUser,
    Eco1User,
    Eco2User,
    EdgeCuts,
    Margin,
    BCrtYd,
    FCrtYd,
    BFab,
    FFab,
    User1,
    User2,
    User3,
    User4,
    User5,
    User6,
    User7,
    User8,
    User9,
    Rescue,
    Wildcard,
    WildcardCu,
    WildcardInCu,
    FBCu,
    WildcardAdhes,
    WildcardPaste,
    WildcardSilkS,
    WildcardMask,
    WildcardUser,
    WildcardCrtYd,
    WildcardFab,
}

impl LayerId {
    pub fn is_wildcard(&self) -> bool {
        matches!(
            self,
            Self::Wildcard
                | Self::WildcardCu
                | Self::WildcardInCu
                | Self::FBCu
                | Self::WildcardAdhes
                | Self::WildcardPaste
                | Self::WildcardSilkS
                | Self::WildcardMask
                | Self::WildcardUser
                | Self::WildcardCrtYd
                | Self::WildcardFab
        )
    }

    pub fn is_copper(&self) -> bool {
        let is_in_cu = Self::In1Cu as u8 <= *self as u8 && *self as u8 <= Self::In30Cu as u8;

        is_in_cu
            || matches!(
                self,
                Self::FCu | Self::BCu | Self::FBCu | Self::WildcardCu | Self::WildcardInCu
            )
    }
}

impl FromStr for LayerId {
    type Err = KiCadParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "F.Cu" => Self::FCu,
            "In1.Cu" => Self::In1Cu,
            "In2.Cu" => Self::In2Cu,
            "In3.Cu" => Self::In3Cu,
            "In4.Cu" => Self::In4Cu,
            "In5.Cu" => Self::In5Cu,
            "In6.Cu" => Self::In6Cu,
            "In7.Cu" => Self::In7Cu,
            "In8.Cu" => Self::In8Cu,
            "In9.Cu" => Self::In9Cu,
            "In10.Cu" => Self::In10Cu,
            "In11.Cu" => Self::In11Cu,
            "In12.Cu" => Self::In12Cu,
            "In13.Cu" => Self::In13Cu,
            "In14.Cu" => Self::In14Cu,
            "In15.Cu" => Self::In15Cu,
            "In16.Cu" => Self::In16Cu,
            "In17.Cu" => Self::In17Cu,
            "In18.Cu" => Self::In18Cu,
            "In19.Cu" => Self::In19Cu,
            "In20.Cu" => Self::In20Cu,
            "In21.Cu" => Self::In21Cu,
            "In22.Cu" => Self::In22Cu,
            "In23.Cu" => Self::In23Cu,
            "In24.Cu" => Self::In24Cu,
            "In25.Cu" => Self::In25Cu,
            "In26.Cu" => Self::In26Cu,
            "In27.Cu" => Self::In27Cu,
            "In28.Cu" => Self::In28Cu,
            "In29.Cu" => Self::In29Cu,
            "In30.Cu" => Self::In30Cu,
            "B.Cu" => Self::BCu,
            "B.Adhes" => Self::BAdhes,
            "F.Adhes" => Self::FAdhes,
            "B.Paste" => Self::BPaste,
            "F.Paste" => Self::FPaste,
            "B.SilkS" => Self::BSilkS,
            "F.SilkS" => Self::FSilkS,
            "B.Mask" => Self::BMask,
            "F.Mask" => Self::FMask,
            "Dwgs.User" => Self::DwgsUser,
            "Cmts.User" => Self::CmtsUser,
            "Eco1.User" => Self::Eco1User,
            "Eco2.User" => Self::Eco2User,
            "Edge.Cuts" => Self::EdgeCuts,
            "Margin" => Self::Margin,
            "B.CrtYd" => Self::BCrtYd,
            "F.CrtYd" => Self::FCrtYd,
            "B.Fab" => Self::BFab,
            "F.Fab" => Self::FFab,
            "User.1" => Self::User1,
            "User.2" => Self::User2,
            "User.3" => Self::User3,
            "User.4" => Self::User4,
            "User.5" => Self::User5,
            "User.6" => Self::User6,
            "User.7" => Self::User7,
            "User.8" => Self::User8,
            "User.9" => Self::User9,
            "Rescue" => Self::Rescue,
            "*" => Self::Wildcard,
            "*.Cu" => Self::WildcardCu,
            "*In.Cu" => Self::WildcardCu,
            "F&B.Cu" => Self::FBCu,
            "*.Adhes" => Self::WildcardAdhes,
            "*.Paste" => Self::WildcardPaste,
            "*.SilkS" => Self::WildcardSilkS,
            "*.Mask" => Self::WildcardMask,
            "*.User" => Self::WildcardUser,
            "*.CrtYd" => Self::WildcardCrtYd,
            "*.Fab" => Self::WildcardFab,
            _ => return Err(KiCadParseError::InvalidLayer(s.to_string())),
        })
    }
}

impl From<LayerId> for String {
    fn from(value: LayerId) -> String {
        match value {
            LayerId::FCu => "F.Cu",
            LayerId::In1Cu => "In1.Cu",
            LayerId::In2Cu => "In2.Cu",
            LayerId::In3Cu => "In3.Cu",
            LayerId::In4Cu => "In4.Cu",
            LayerId::In5Cu => "In5.Cu",
            LayerId::In6Cu => "In6.Cu",
            LayerId::In7Cu => "In7.Cu",
            LayerId::In8Cu => "In8.Cu",
            LayerId::In9Cu => "In9.Cu",
            LayerId::In10Cu => "In10.Cu",
            LayerId::In11Cu => "In11.Cu",
            LayerId::In12Cu => "In12.Cu",
            LayerId::In13Cu => "In13.Cu",
            LayerId::In14Cu => "In14.Cu",
            LayerId::In15Cu => "In15.Cu",
            LayerId::In16Cu => "In16.Cu",
            LayerId::In17Cu => "In17.Cu",
            LayerId::In18Cu => "In18.Cu",
            LayerId::In19Cu => "In19.Cu",
            LayerId::In20Cu => "In20.Cu",
            LayerId::In21Cu => "In21.Cu",
            LayerId::In22Cu => "In22.Cu",
            LayerId::In23Cu => "In23.Cu",
            LayerId::In24Cu => "In24.Cu",
            LayerId::In25Cu => "In25.Cu",
            LayerId::In26Cu => "In26.Cu",
            LayerId::In27Cu => "In27.Cu",
            LayerId::In28Cu => "In28.Cu",
            LayerId::In29Cu => "In29.Cu",
            LayerId::In30Cu => "In30.Cu",
            LayerId::BCu => "B.Cu",
            LayerId::BAdhes => "B.Adhes",
            LayerId::FAdhes => "F.Adhes",
            LayerId::BPaste => "B.Paste",
            LayerId::FPaste => "F.Paste",
            LayerId::BSilkS => "B.SilkS",
            LayerId::FSilkS => "F.SilkS",
            LayerId::BMask => "B.Mask",
            LayerId::FMask => "F.Mask",
            LayerId::DwgsUser => "Dwgs.User",
            LayerId::CmtsUser => "Cmts.User",
            LayerId::Eco1User => "Eco1.User",
            LayerId::Eco2User => "Eco2.User",
            LayerId::EdgeCuts => "Edge.Cuts",
            LayerId::Margin => "Margin",
            LayerId::FCrtYd => "F.CrtYd",
            LayerId::BCrtYd => "B.CrtYd",
            LayerId::FFab => "F.Fab",
            LayerId::BFab => "B.Fab",
            LayerId::User1 => "User.1",
            LayerId::User2 => "User.2",
            LayerId::User3 => "User.3",
            LayerId::User4 => "User.4",
            LayerId::User5 => "User.5",
            LayerId::User6 => "User.6",
            LayerId::User7 => "User.7",
            LayerId::User8 => "User.8",
            LayerId::User9 => "User.9",
            LayerId::Rescue => "Rescue",
            LayerId::Wildcard => "*",
            LayerId::WildcardCu => "*.Cu",
            LayerId::WildcardInCu => "*In.Cu",
            LayerId::FBCu => "F&B.Cu",
            LayerId::WildcardAdhes => "*.Adhes",
            LayerId::WildcardPaste => "*.Paste",
            LayerId::WildcardSilkS => "*.SilkS",
            LayerId::WildcardMask => "*.Mask",
            LayerId::WildcardUser => "*.User",
            LayerId::WildcardCrtYd => "*.CrtYd",
            LayerId::WildcardFab => "*.Fab",
        }
        .to_string()
    }
}

// ############################################################################

/// A group of items represented by a list of unique identifiers.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone)]
pub struct Group {
    /// The name attribute defines the name of the group.
    pub name: String,
    /// UNDOCUMENTED: The locked token specifies if the group is locked.
    pub locked: bool,
    /// The `id` token attribute defines the unique identifier of the group.
    pub id: Uuid,
    /// The `members` token attributes define a list of unique identifiers of
    /// the objects belonging to the group.
    pub members: Vec<Uuid>,
}

impl FromSexpr for Group {
    fn from_sexpr(mut parser: Parser) -> Result<Self, KiCadParseError> {
        parser.expect_symbol_matching("group")?;

        let name = parser.expect_string()?;
        let locked = parser.maybe_symbol_matching("locked");
        let id = parser.expect_with_name::<Uuid>("id")?;
        let members = parser.expect_list_with_name("members").and_then(|mut p| {
            let members = p
                .expect_many_symbols()?
                .into_iter()
                .map(|s| s.parse::<uuid::Uuid>())
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(Uuid::from)
                .collect();

            p.expect_end()?;

            Ok(members)
        })?;

        parser.expect_end()?;

        Ok(Self {
            name,
            locked,
            id,
            members,
        })
    }
}

simple_maybe_from_sexpr!(Group, group);

impl ToSexpr for Group {
    fn to_sexpr(&self, context: SerializationContext) -> Sexpr {
        Sexpr::list_with_name(
            "group",
            [
                Some(Sexpr::string(&self.name)),
                self.locked.then(|| Sexpr::symbol("locked")),
                Some(self.id.to_sexpr_with_name("id", context)),
                Some(Sexpr::list_with_name(
                    "members",
                    self.members
                        .iter()
                        .map(|u| u.0)
                        .map(|u: uuid::Uuid| Sexpr::symbol(u.to_string()))
                        .map(Option::Some)
                        .collect::<Vec<_>>(),
                )),
            ],
        )
    }
}

// ############################################################################

/// The fill mode used by most solid shapes
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "snake_case"))]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SimpleFillMode {
    Solid,
    None,
}

simple_to_from_string! {
    SimpleFillMode,
    solid <-> Solid,
    none <-> None,
}
